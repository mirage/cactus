include Cache_intf

module Make (K : Hashtbl.HashedType) (V : Lru.Weighted) = struct
  type key = K.t

  type value = V.t

  module Lru = struct
    include Lru.M.Make (K) (V)

    exception EmptyLru

    let unsafe_lru t = match lru t with None -> raise EmptyLru | Some (_, v) -> v

    let clear t =
      let cap = capacity t in
      resize 0 t;
      trim t;
      resize cap t
  end

  module Hashtbl = Hashtbl.Make (K)

  type t = {
    california : value Hashtbl.t;
    lru : Lru.t;
    volatile : value Hashtbl.t;
    flush : key -> value -> unit;
    load : ?available:value -> key -> value;
    mutable filter : value -> [ `California | `Lru | `Volatile ];
  }

  let v ~flush ~load ~filter lru_cap =
    {
      flush;
      load;
      california = Hashtbl.create 4096;
      lru = Lru.create lru_cap;
      volatile = Hashtbl.create 16;
      filter;
    }

  let lru_filled = ref false

  module Queue = struct
    include Queue

    let push =
      let lost_count = ref 0 in
      fun v q ->
        if length q < 128 then push v q
        else (
          incr lost_count;
          if !lost_count mod 1_000 = 0 then
            Log.warn (fun reporter -> reporter "%i buffers lost" !lost_count))
  end

  let availables = Queue.create ()

  let length t = Hashtbl.length t.california + Hashtbl.length t.volatile + Lru.size t.lru

  let find t key =
    match
      (Hashtbl.find_opt t.california key, Lru.find key t.lru, Hashtbl.find_opt t.volatile key)
    with
    (* Lru.find is equivalent to Hashtbl.find_opt *)
    | Some value, None, None | None, None, Some value -> value
    | None, Some value, None ->
        Lru.promote key t.lru;
        value
    | None, None, None ->
        let value =
          match Queue.is_empty availables with
          | true -> t.load key
          | false -> t.load ~available:(Queue.pop availables) key
        in
        (match t.filter value with
        | `California -> Hashtbl.add t.california key value
        | `Lru ->
            Lru.add key value t.lru;
            while Lru.weight t.lru > Lru.capacity t.lru do
              if not !lru_filled then (
                Log.warn (fun reporter -> reporter "LRU is filled");
                lru_filled := true);
              match Lru.lru t.lru with
              | Some (key, value) ->
                  t.flush key value;
                  Queue.push (Lru.unsafe_lru t.lru) availables;
                  Lru.drop_lru t.lru
              | None -> failwith "Empty LRU should not be over capacity"
            done
        | `Volatile ->
            Hashtbl.add t.volatile key value;
            if Hashtbl.length t.volatile > 64 then (
              Log.warn (fun reporter -> reporter "Not enough release");
              assert false));
        value
    | _ -> failwith "Key loaded in several caches"

  let reload t key =
    match
      (Hashtbl.find_opt t.california key, Lru.find key t.lru, Hashtbl.find_opt t.volatile key)
    with
    | Some value, None, None -> (
        match t.filter value with
        | `California -> ()
        | `Lru ->
            Hashtbl.remove t.california key;
            Lru.add key value t.lru
        | `Volatile ->
            Hashtbl.remove t.california key;
            Hashtbl.add t.volatile key value)
    | None, Some value, None -> (
        match t.filter value with
        | `California ->
            Lru.remove key t.lru;
            Hashtbl.add t.california key value
        | `Lru -> Lru.promote key t.lru
        | `Volatile ->
            Lru.remove key t.lru;
            Hashtbl.add t.volatile key value)
    | None, None, Some value -> (
        match t.filter value with
        | `California ->
            Hashtbl.remove t.volatile key;
            Hashtbl.add t.california key value
        | `Lru ->
            Hashtbl.remove t.volatile key;
            Lru.add key value t.lru
        | `Volatile -> ())
    | None, None, None -> failwith "Key is not loaded"
    | _ -> failwith "Key loaded in several caches"

  let update_filter t ~filter =
    t.filter <- filter;
    Hashtbl.filter_map_inplace
      (fun key value ->
        if filter value = `California then Some value
        else (
          t.flush key value;
          None))
      t.california

  let release t =
    Hashtbl.iter
      (fun k v ->
        Queue.push v availables;
        t.flush k v)
      t.volatile;
    Hashtbl.clear t.volatile

  let deallocate t key =
    Hashtbl.remove t.california key;
    Lru.remove key t.lru;
    Hashtbl.remove t.volatile key

  let clear t =
    Hashtbl.clear t.volatile;
    Hashtbl.clear t.california;
    Lru.clear t.lru

  let flush t =
    Hashtbl.iter t.flush t.california;
    Lru.iter t.flush t.lru;
    release t
end
