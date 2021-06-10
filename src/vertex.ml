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

    module Common = Field.MakeCommon (Params)
    module Page = Store.Page
    module Header = Vertex_header.Make (Params) (Store) (Common)

    (* STAT WRAPPERS *)
    open Stats.Func
    open Stats.Nodes

    type t = { store : store; header : Header.t; buff : bytes }

    let depth =
      match Bound.kind with
      | `Leaf -> ( function _ -> 0)
      | `Node -> ( function t -> Header.g_kind t.header |> Common.Kind.to_depth)

    let nentry t = Header.g_nentry t.header |> Header.Nentry.from_t


    let flag_sz, key_sz, bound_sz = (Common.Flag.size, Params.key_sz, Bound.size)

    let entry_sizes = [ flag_sz; key_sz; bound_sz ]

    let entry_size = List.fold_left ( + ) 0 entry_sizes

    type offsets = { flag : int; key : int; bound : int }

    let offsets =
      match Utils.sizes_to_offsets entry_sizes with
      | [ flag; key; bound ] -> { flag; key; bound }
      | _ -> failwith "Incorrect offsets"

    let available_size = Params.page_sz - Header.size

    let () =
      if Params.version = 2 && available_size / entry_size < 2 * Params.fanout then (
        Fmt.pr "Page size must be at least %i@." (Header.size + (2 * Params.fanout * entry_size));
        assert false)

    let nth_key t n = Key.get t.buff ~off:(Header.size + (n * entry_size) + offsets.key)

    let nth_dead t n =
      Common.Flag.get t.buff ~off:(Header.size + (n * entry_size) + offsets.flag)
      |> Common.Flag.from_t

    let nth_bound t n = Bound.get t.buff ~off:(Header.size + (n * entry_size) + offsets.bound)

    let load store address =
      tic stat_load;
      let page = Store.load store address in
      let buff = Page.buff page in
      let header = Header.load buff in
      tac stat_load;
      { store; buff; header }

    module PP = struct
      open Fmt

      let pp_entry ppf ~off buff =
        let color = match Bound.kind with `Leaf -> `Blue | `Node -> `Cyan in
        pf ppf "@[<hov 1>dead:@ %a%,%a@]@;@[<hov 1>key:@ %a@]@;@[<hov 1>bound:@ %a@]"
          (Common.Flag.pp_raw ~off:(off + offsets.flag) |> styled (`Bg `Red))
          buff
          (Common.Flag.pp |> styled (`Bg `Red) |> styled `Reverse)
          (Common.Flag.get buff ~off:(off + offsets.flag))
          (*-*)
          Key.pp
          (Key.get buff ~off:(off + offsets.key))
          (*-*)
          (Bound.pp |> styled (`Bg color) |> styled `Reverse)
          (Bound.get buff ~off:(off + offsets.bound))

      let pp ppf t =
        let offs = List.init (nentry t) (fun i -> Header.size + (i * entry_size)) in
        let pp_entries ppf offs =
          List.iteri (fun i off -> Fmt.pf ppf "@[<v 2>%i:@;%a@]@;" i (pp_entry ~off) t.buff) offs
        in
        Fmt.pf ppf "@[<v 2>Header:@;%a@]@;@[<v 2>Content:@;%a@]" Header.pp t.header pp_entries offs
    end

    include PP

    let create store kind address =
      tic stat_create;
      (* initialises the header of a new vertex *)
      assert (
        match (kind : Field.kind) with Node _ -> Bound.kind = `Node | Leaf -> Bound.kind = `Leaf);
      let page = Store.load store address in
      let buff = Page.buff page in
      let header = Header.load buff in
      Header.init header kind;
      Store.reload store address;
      tac stat_create;
      { store; header; buff }

    let clear _t _predicate = failwith "not finished"

    let compare t key n = Key.compare key (nth_key t n)

    let compare_interval t key n =
      let comp = Key.compare key (nth_key t n) in
      if comp <= 0 then comp
      else if n = nentry t - 1 then 0
      else if Key.compare key (nth_key t (n + 1)) < 0 then 0
      else comp

    let shrink _t = failwith "Not implemented"

    let keys_sorted t =
      let ret = List.init (nentry t) (fun i -> nth_key t i |> Key.debug_dump) in
      Utils.is_sorted ret

    let split t store address =
      tic stat_split;

    let split t address =
      tic stat_split;
      shrink t;
      let promoted_rank = nentry t / 2 in
      let promoted = nth_key t promoted_rank in

      (* the promoted key [promoted] here acts as a pivot to separate the keys remaining in the current vertex
         from those that will be moving to a newly allocated vertex *)
      let mv_t = create t.store (Header.g_kind t.header |> Common.Kind.from_t) address in

      let mv_nentry = nentry t - promoted_rank in
      Bytes.blit t.buff
        (Header.size + (promoted_rank * entry_size))
        mv_t.buff Header.size
        (Header.size + (mv_nentry * entry_size));

      mv_nentry |> Header.Nentry.to_t |> Header.s_nentry mv_t.header;
      promoted_rank |> Header.Nentry.to_t |> Header.s_nentry t.header;

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
        if nentry t = 0 then false
        else
          let compare =
            match Bound.kind with `Leaf -> compare t key | `Node -> compare_interval t key
          in
          let n = Utils.binary_search ~safe:true ~compare 0 (nentry t) in
          Key.equal (nth_key t n) key && not (nth_dead t n)
      in
      tac stat_mem;
      ret

    let find_position t key =
      let compare = compare t key in
      if nentry t = 0 || compare (nentry t - 1) > 0 then nentry t
      else
        let position = Utils.binary_search ~safe:true ~compare 0 (nentry t) in
        if compare position <= 0 then position else position + 1

    let shift t position =
      tic stat_shift;
      let length = (nentry t - position) * entry_size in
      Bytes.blit t.buff
        (Header.size + (entry_size * position))
        t.buff
        (Header.size + (entry_size * (position + 1)))
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
      if not (shadow || append) then shift t position;

      let off = Header.size + (position * entry_size) in
      Common.Flag.to_t false |> Common.Flag.set t.buff ~off:(off + offsets.flag);
      key |> Key.set t.buff ~off:(off + offsets.key);
      bound |> Bound.set t.buff ~off:(off + offsets.bound);

      if append || not shadow then Header.s_nentry t.header (nentry t + 1 |> Header.Nentry.to_t);
      if nentry t > 2 * Params.fanout then shrink t;
      if Params.debug then assert (keys_sorted t);
      tac stat_add

    let remove _t _key = failwith "not finished"

    let length t = entry_number t

    let length t = nentry t - ndeadentry t

    let header_of_depth i =
      let open Header in
      let buff = Bytes.make size '\000' in
      let header = load buff in
      let kind : Field.kind = if i = 0 then Leaf else Node i in
      init header kind;
      s_nentry header @@ Nentry.to_t @@ Params.fanout;
      Bytes.to_string buff

    let migrate_headers = Array.init 10 (fun i -> header_of_depth (1 + i))

    let migrate_leaf_header = header_of_depth 0

    let header_of_depth depth =
      if depth = 0 then migrate_leaf_header
      else if depth <= 10 then migrate_headers.(depth - 1)
      else header_of_depth depth

    let migrate_header kind n =
      let depth = kind |> Common.Kind.to_t |> Common.Kind.to_depth in
      if n = Params.fanout then header_of_depth depth
      else
        let open Header in
        let buff = Bytes.make size '\000' in
        let header = load buff in
        init header kind;
        s_nentry header @@ Nentry.to_t @@ n;
        Bytes.to_string buff

    let migrate kvs kind =
      assert (
        match (kind : Field.kind) with Leaf -> Bound.kind = `Leaf | Node _ -> Bound.kind = `Node);
      let not_dead = Bytes.create Common.Flag.size in
      Common.Flag.to_t false |> Common.Flag.set not_dead ~off:0;
      let not_dead = Bytes.to_string not_dead in
      let kvs = List.map (( ^ ) not_dead) kvs in
      let header = migrate_header kind (List.length kvs) in
      String.concat "" (header :: kvs)

    let iter t func =
      for i = 0 to nentry t - 1 do
        if not (nth_dead t i) then func (nth_key t i) (nth_bound t i)
      done

    let fold_left func acc t =
      List.init (nentry t) (fun i -> (nth_key t i, nth_bound t i)) |> List.fold_left func acc
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
