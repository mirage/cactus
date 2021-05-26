(* No overflow is allowed in this implementation *)

include Vertex_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Bound : BOUND)
  ->
  struct
    type store = Store.t

    type address = Store.address

    module CommonHeader = Header.MakeCommon (Params)
    module CommonFields = Field.MakeCommon (Params)
    module Page = Store.Page
    module FmtHeader = Vertex_header.Make (Params) (Store)

    (* STAT WRAPPERS *)
    open Stats.Func
    open Stats.Nodes

    type t = { store : store; header : FmtHeader.t; buff : bytes }

    let depth =
      match Bound.kind with
      | `Leaf -> ( function _ -> 0)
      | `Node -> (
          function
          | t -> t.header.kind |> CommonHeader.Kind.to_kind |> CommonHeader.Kind.kind_to_depth)

    let entry_number t = FmtHeader.Entry_number.to_int t.header.entry_number

    let flag_sz, key_sz, bound_sz = (CommonHeader.Flag.size, Params.key_sz, Bound.size)

    let entry_sizes = [ flag_sz; key_sz; bound_sz ]

    let flag_off, key_off, bound_off =
      match Utils.sizes_to_offsets entry_sizes with
      | [ flag_off; key_off; bound_off ] -> (flag_off, key_off, bound_off)
      | _ -> assert false

    let entry_size = List.fold_left ( + ) 0 entry_sizes

    let available_size = Params.page_sz - FmtHeader.size

    let () =
      if Params.version = 2 && available_size / entry_size < 2 * Params.fanout then (
        Fmt.pr "Page size must be at least %i@." (FmtHeader.size + (2 * Params.fanout * entry_size));
        assert false)

    let nth_key t n =
      Bytes.sub_string t.buff (FmtHeader.size + (n * entry_size) + key_off) key_sz
      |> Encoder.load
      |> Key.decode

    let nth_dead t n =
      CommonHeader.Flag.v t.buff ~off:(FmtHeader.size + (n * entry_size) + flag_off)
      |> CommonHeader.Flag.to_bool

    let nth_bound t n =
      Bytes.sub_string t.buff (FmtHeader.size + (n * entry_size) + bound_off) bound_sz
      |> Encoder.load
      |> Bound.decode

    let load store address =
      tic stat_load;
      let page = Store.load store address in
      let buff = Page.buff page in
      let header = FmtHeader.load buff in
      let ret = { store; buff; header } in
      tac stat_load;
      ret

    module PP = struct
      open Fmt

      let pp_entry ppf (dead, key, bound) =
        let color = match Bound.kind with `Leaf -> `Blue | `Node -> `Cyan in
        pf ppf "@[<hov 1>dead:@ %a%,%a@]@;@[<hov 1>key:@ %a%,%a@]@;@[<hov 1>bound:@ %a%,%a@]"
          (CommonHeader.Flag.pp_raw |> styled (`Bg `Red))
          dead
          (CommonHeader.Flag.pp |> styled (`Bg `Red) |> styled `Reverse)
          dead
          (Encoder.pp |> styled (`Bg `Green))
          (key |> Key.encode) Key.pp key
          (Encoder.pp |> styled (`Bg color))
          (bound |> Bound.encode)
          (Bound.pp |> styled (`Bg color) |> styled `Reverse)
          bound

      let pp ppf t =
        let entries =
          List.init (entry_number t) (fun i ->
              ( CommonHeader.Flag.v t.buff ~off:(FmtHeader.size + (i * entry_size) + flag_off),
                nth_key t i,
                nth_bound t i ))
        in
        let pp_entries ppf entries =
          List.iteri (fun i entry -> Fmt.pf ppf "@[<v 2>%i:@;%a@]@;" i pp_entry entry) entries
        in
        Fmt.pf ppf "@[<v 2>Header:@;%a@]@;@[<v 2>Content:@;%a@]" FmtHeader.pp t.header pp_entries
          entries
    end

    include PP

    let create store kind address =
      tic stat_create;
      (* initialises the header of a new vertex *)
      assert (
        match (kind : Field.kind) with
        | Node _ -> Bound.kind = `Node
        | Leaf -> Bound.kind = `Leaf
        | _ -> failwith "unexepected kind");
      let page = Store.load store address in
      let buff = Page.buff page in
      let header = FmtHeader.init kind buff in
      Store.reload store address;
      tac stat_create;
      { store; header; buff }

    let clear _t _predicate = failwith "not finished"

    let compare t key n = Key.compare key (nth_key t n)

    let compare_interval t key n =
      let comp = Key.compare key (nth_key t n) in
      if comp <= 0 then comp
      else if n = entry_number t - 1 then 0
      else if Key.compare key (nth_key t (n + 1)) < 0 then 0
      else comp

    let shrink _t = failwith "Not implemented"

    let keys_sorted t =
      let ret = List.init (entry_number t) (fun i -> nth_key t i |> Key.encode |> Encoder.dump) in
      Utils.is_sorted ret

    let split t store address =
      tic stat_split;

      let promoted_rank = entry_number t / 2 in
      let promoted = nth_key t promoted_rank in

      (* the promoted key [promoted] here acts as a pivot to separate the keys remaining in the current vertex
         from those that will be moving to a newly allocated vertex *)
      let mv_t = create store (CommonHeader.Kind.to_kind t.header.kind) address in

      let mv_entry_number = entry_number t - promoted_rank in
      FmtHeader.Entry_number.set mv_t.header.entry_number mv_entry_number;
      Bytes.blit t.buff
        (FmtHeader.size + (promoted_rank * entry_size))
        mv_t.buff FmtHeader.size
        (FmtHeader.size + (mv_entry_number * entry_size));
      FmtHeader.Entry_number.set t.header.entry_number promoted_rank;

      if Params.debug then (
        assert (keys_sorted mv_t);
        assert (keys_sorted t));

      tac stat_split;
      (promoted, mv_t)

    let find t key =
      tic stat_find;
      let compare =
        match Bound.kind with `Leaf -> compare t key | `Node -> compare_interval t key
      in
      let n = Utils.binary_search ~compare 0 (entry_number t) in
      if nth_dead t n then raise Not_found;
      let ret = nth_bound t n in
      tac stat_find;
      ret

    let mem t key =
      tic stat_mem;
      let ret =
        if entry_number t = 0 then false
        else
          let compare =
            match Bound.kind with `Leaf -> compare t key | `Node -> compare_interval t key
          in
          let n = Utils.binary_search ~safe:true ~compare 0 (entry_number t) in
          Key.equal (nth_key t n) key && not (nth_dead t n)
      in
      tac stat_mem;
      ret

    let find_position t key =
      let compare = compare t key in
      if entry_number t = 0 || compare (entry_number t - 1) > 0 then entry_number t
      else
        let position = Utils.binary_search ~safe:true ~compare 0 (entry_number t) in
        if compare position <= 0 then position else position + 1

    let shift t position =
      tic stat_shift;
      let length = (entry_number t - position) * entry_size in
      Bytes.blit t.buff
        (FmtHeader.size + (entry_size * position))
        t.buff
        (FmtHeader.size + (entry_size * (position + 1)))
        length;
      tac stat_shift

    let add t key bound =
      tic stat_add;
      let position = find_position t key in
      let shadow = Key.equal (nth_key t position) key in
      let append = position >= entry_number t in

      if shadow then
        Log.warn (fun reporter ->
            reporter "Shadowing key %s." (key |> Key.debug_dump |> Hex.of_string |> Hex.show));
      if (not shadow) && not append then shift t position;

      let entry =
        Encoder.concat
          [ CommonFields.Flag.(of_bool false |> encode); Key.encode key; Bound.encode bound ]
        |> Encoder.dump
      in
      assert (String.length entry = entry_size);
      Bytes.blit_string entry 0 t.buff (FmtHeader.size + (position * entry_size)) entry_size;

      if append || not shadow then
        FmtHeader.Entry_number.set t.header.entry_number
          (1 + FmtHeader.Entry_number.to_int t.header.entry_number);
      if Params.debug then assert (keys_sorted t);
      tac stat_add

    let remove _t _key = failwith "not finished"

    let length t = entry_number t

    let migrate_headers =
      let open FmtHeader in
      Array.init 10 (fun i ->
          let buff = Bytes.make size '\000' in
          let header = init (Field.Node (1 + i)) buff in
          Entry_number.set header.entry_number Params.fanout;
          Bytes.to_string buff)

    let migrate_leaf_header =
      let open FmtHeader in
      let buff = Bytes.make size '\000' in
      let header = init Field.Leaf buff in
      Entry_number.set header.entry_number Params.fanout;
      Bytes.to_string buff

    let header_of_depth depth =
      let open FmtHeader in
      if depth = 0 then migrate_leaf_header
      else if depth <= 10 then migrate_headers.(depth - 1)
      else
        let buff = Bytes.make size '\000' in
        let header = init (Field.Node depth) buff in
        Entry_number.set header.entry_number Params.fanout;
        Bytes.to_string buff

    let migrate_header kind n =
      let open FmtHeader in
      let depth = CommonHeader.Kind.(kind |> kind_to_depth) in
      if n = Params.fanout then header_of_depth depth
      else
        let buff = Bytes.make size '\000' in
        let header = init (Field.Node depth) buff in
        Entry_number.set header.entry_number n;
        Bytes.to_string buff

    let migrate kvs kind =
      assert (
        match (kind : Field.kind) with
        | Leaf -> Bound.kind = `Leaf
        | Node _ -> Bound.kind = `Node
        | _ -> failwith "Unexpected kind");
      let alive = "\000" in
      let kvs = List.map (( ^ ) alive) kvs in
      let header = migrate_header kind (List.length kvs) in
      String.concat "" (header :: kvs)

    let iter t func =
      for i = 0 to entry_number t - 1 do
        if not (nth_dead t i) then func (nth_key t i) (nth_bound t i)
      done

    let fold_left func acc t =
      List.init (entry_number t) (fun i -> (nth_key t i, nth_bound t i)) |> List.fold_left func acc
  end

module LeafMake (Params : Params.S) (Store : Store.S) (Key : Data.K) (Value : Data.V) = struct
  include
    Make (Params) (Store) (Key)
      (struct
        include Value

        let kind = `Leaf
      end)
end

module NodeMake (Params : Params.S) (Store : Store.S) (Key : Data.K) = struct
  module CommonFields = Field.MakeCommon (Params)

  include
    Make (Params) (Store) (Key)
      (struct
        include CommonFields.Address

        let kind = `Node
      end)
end
