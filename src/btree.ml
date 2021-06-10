exception NotFinished

include Btree_intf

let ( // ) a b = a ^ "/" ^ b

module Make (InKey : Input.Key) (InValue : Input.Value) (Size : Input.Size) :
  S with type key = InKey.t and type value = InValue.t = struct
  type key = InKey.t

  type value = InValue.t

  module Params = Params.Make (Size) (InKey) (InValue)
  module Common = Field.MakeCommon (Params)
  module Entry = Data.Make (InKey) (InValue)
  module Key = Entry.Key
  module Value = Entry.Value
  module Store = Store.Make (Params) (Common)
  module Page = Store.Page
  module Leaf = Leaf.Make (Params) (Store) (Key) (Value)
  module Node = Node.Make (Params) (Store) (Key)

  open Stats.Func
  (** STAT WRAPPERS **)

  open Stats.Btree

  let () = if Params.debug then Log.warn (fun reporter -> reporter "Debug mode is set.")

  let min_key = Key.min

  type t = { store : Store.t }

  type cache = int

  let next_cache = ref 0

  let caches : (cache, t) Hashtbl.t = Hashtbl.create 16

  let roots : (cache, string) Hashtbl.t = Hashtbl.create 16

  let empty_cache () =
    incr next_cache;
    Log.debug (fun reporter -> reporter "Creating a new cache id : %i " !next_cache);
    !next_cache

  let flush tree = Store.flush tree.store

  let clear tree =
    Log.debug (fun reporter -> reporter "clearing");
    Store.clear tree.store;
    Leaf.init tree.store (Store.root tree.store) |> ignore;
    flush tree

  let snapshot ?(depth = 0) t =
    (* for every node/leaf in [t] which are at least [depth] away from the leaves, [snapshot ~depth t], write in a file its rep as given by their corresponding pp function *)
    let snap_page address page =
      let kind = Page.kind page in
      if Common.Kind.to_depth kind >= depth then
        match Common.Kind.from_t kind with
        | Leaf ->
            let leaf = Leaf.load t.store address in
            let out_file =
              open_out ((Store.Private.dir t.store // "pp_page_") ^ string_of_int address ^ ".ansi")
            in
            let formatter = out_file |> Format.formatter_of_out_channel in
            Fmt.set_style_renderer formatter `Ansi_tty;
            Fmt.pf formatter "%a@." Leaf.pp leaf;
            close_out out_file;
            Store.release_ro t.store
        | Node _n ->
            let node = Node.load t.store address in
            let out_file =
              open_out ((Store.Private.dir t.store // "pp_page_") ^ string_of_int address ^ ".ansi")
            in
            let formatter = out_file |> Format.formatter_of_out_channel in
            Fmt.set_style_renderer formatter `Ansi_tty;
            Fmt.pf formatter "%a@." (Node.pp |> Fmt.vbox) node;
            close_out out_file
    in
    flush t;
    Store.iter t.store func;
    let out_header = open_out (Store.Private.dir t.store // "pp_header.ansi") in
    let formatter = out_header |> Format.formatter_of_out_channel in
    Fmt.set_style_renderer formatter `Ansi_tty;
    Fmt.pf formatter "%a@." Store.pp_header t.store;
    close_out out_header

  let length tree =
    let rec aux address =
      let page = Store.load tree.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load tree.store address in
          let ret = Leaf.length leaf in
          Store.release_ro tree.store;
          ret
      | Node _depth ->
          let node = Node.load tree.store address in
          Node.fold_left (fun acc _key address -> acc + aux address) 0 node
    in
    let root = Store.root tree.store in
    aux root

  let create ?cache root =
    Log.info (fun reporter -> reporter "Btree version %i (13 Apr. 2021)" Size.version);
    Log.debug (fun reporter ->
        reporter "Btree at root %s %s" root
          (match cache with
          | None -> "without cache"
          | Some cache -> Fmt.str "with cache id %i" cache));
    let cache = match cache with None -> empty_cache () | Some cache -> cache in
    let overwriting = Hashtbl.mem caches cache && root != Hashtbl.find roots cache in
    if Hashtbl.mem caches cache && not overwriting then Hashtbl.find caches cache
    else
      let just_load = Sys.file_exists (root ^ "/" ^ "b.tree") in
      let t = { store = Store.init ~root } in
      if just_load then Log.debug (fun reporter -> length t |> reporter "Loading %i bindings")
      else (
        Leaf.init t.store (Store.root t.store) |> ignore;
        flush t);
      if overwriting then
        Log.warn (fun reporter ->
            reporter "Overwriting cache with id %i from %s to root %s" cache
              (Hashtbl.find roots cache) root);
      Hashtbl.add roots cache root;
      Hashtbl.add caches cache t;
      t

  let rec go_to_leaf tree key address =
    let page = Store.load tree.store address in
    match Page.kind page |> Common.Kind.from_t with
    | Leaf -> address
    | Node _depth ->
        let node = Node.load tree.store address in
        go_to_leaf tree key (Node.find node key)

  let find tree inkey =
    tic stat_find;
    let key = Key.of_input inkey in
    let go_to_leaf = go_to_leaf tree key in
    let address = go_to_leaf (Store.root tree.store) in
    let leaf = Leaf.load tree.store address in
    let ret = Leaf.find leaf key |> Value.to_input in
    Store.release_ro tree.store;
    tac stat_find;
    ret

  let mem tree inkey =
    tic stat_mem;
    let key = Key.of_input inkey in
    let go_to_leaf = go_to_leaf tree key in
    let address = go_to_leaf (Store.root tree.store) in
    let leaf = Leaf.load tree.store address in
    let ret = Leaf.mem leaf key in
    Store.release_ro tree.store;
    tac stat_mem;
    ret

  let path_to_leaf t key =
    let rec aux path address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf -> address :: path
      | Node _depth ->
          let node = Node.load t.store address in
          aux (address :: path) (Node.find node key)
    in

    aux [] (Store.root t.store)

  let add tree inkey invalue =
    tic stat_add;
    let key = Key.of_input inkey in
    let value = Value.of_input invalue in
    let path = path_to_leaf tree key in
    let leaf_address = List.hd path in

    let rec split_nodes nodes promoted allocated_address =
      match nodes with
      | [] ->
          (* this case happens only when no nodes are there in the first place *and* the leaf has overflowed
             This means that the tree is a single leaf, and we have to create a new root on top of it *)
          let root = Node.create tree.store Common.Kind.(of_depth 1 |> from_t) in
          Node.add root min_key leaf_address;
          Node.add root promoted allocated_address;
          Store.reroot tree.store (Node.self_address root);
          Log.info (fun reporter -> reporter "Btree height increases to 1")
      | [ address ] ->
          (* there are no nodes above : we are at the root *)
          let root = Node.load tree.store address in
          Node.add root promoted allocated_address;
          if Node.overflow root then (
            let promoted, allocated = Node.split root in
            let new_root =
              Node.create tree.store Common.Kind.(of_depth (1 + Node.depth root) |> from_t)
            in
            Node.add new_root min_key address;
            Node.add new_root promoted (Node.self_address allocated);
            Store.reroot tree.store (Node.self_address new_root);
            Log.info (fun reporter -> reporter "Btree height increases to %i" (Node.depth new_root)))
      | address :: nodes ->
          let node = Node.load tree.store address in
          Node.add node promoted allocated_address;
          if Node.overflow node then
            let promoted, allocated = Node.split node in
            split_nodes nodes promoted (Node.self_address allocated)
    in

    let leaf = Leaf.load tree.store leaf_address in
    Leaf.add leaf key value;
    (if Leaf.overflow leaf then
     let promoted, allocated = Leaf.split leaf in
     split_nodes (List.tl path) promoted (Leaf.self_address allocated));
    Store.release tree.store;
    tac stat_add

  let delete _tree _key = raise NotFinished

  let iter func tree =
    let func key value = func (key |> Key.to_input) (value |> Value.to_input) in
    let rec aux address =
      let page = Store.load tree.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load tree.store address in
          Leaf.iter leaf func;
          Store.release_ro tree.store
      | Node _depth ->
          let node = Node.load tree.store address in
          Node.iter node (fun _key address -> aux address)
    in
    let root = Store.root tree.store in
    aux root

  let iteri func tree =
    let counter = ref 0 in
    let f key value =
      incr counter;
      func !counter key value
    in
    iter f tree

  let depth_of n =
    let rec aux h n = if n = 0 then h else aux (h + 1) (n / Params.fanout) in
    aux (-1) n

  let init ~root n ~read =
    Log.info (fun reporter -> reporter "Btree version %i (13 Apr. 2021)" Size.version);
    let store = Store.init ~root in
    Log.info (fun reporter -> reporter "Initialising btree with %i bindings" n);

    let rec nvertices depth =
      match depth with
      | 0 -> 1
      | 1 -> Params.fanout
      | n -> (
          let sqrt = nvertices (depth / 2) in
          let sqrt2 = sqrt * sqrt in
          match n mod 2 with 0 -> sqrt2 | _ -> Params.fanout * sqrt2)
    in

    let sequentiate n depth =
      let step = nvertices depth in
      let steps = List.init (n / step) (fun _ -> step) in
      match n mod step with 0 -> steps | m -> steps @ [ m ]
    in

    let add content = Store.Private.write store content in

    let depth = depth_of n in

    let address = ref (Store.root store - 1) in
    Store.Private.init_migration store;

    let get_address () =
      let open Common.Address in
      let buff = Bytes.create size in
      !address |> to_t |> set buff ~off:0;
      Bytes.to_string buff
    in
    let rec create leftmost depth n =
      match depth with
      | 0 ->
          incr address;
          let kvs = List.init n (fun _ -> read 1) in
          let k_dump = String.sub (List.hd kvs) 0 Params.key_sz in
          let content = Leaf.migrate kvs in
          let pad = Params.page_sz - String.length content in
          if pad < 0 then (
            Fmt.pr "Assertion error : [page size] must be at least %i@." (String.length content);
            assert false);
          add (content ^ String.make pad '\000');
          k_dump
      | _ ->
          let ns = sequentiate n depth in
          assert (n = List.fold_left ( + ) 0 ns);
          let kvs =
            List.mapi
              (fun i n ->
                let k_dump = create (leftmost && i = 0) (depth - 1) n in
                let address_dump = get_address () in
                (if leftmost && i = 0 then min_key |> Key.debug_dump else k_dump) ^ address_dump)
              ns
          in
          let content = Node.migrate kvs Common.Kind.(of_depth depth |> from_t) in
          let pad = Params.page_sz - String.length content in
          incr address;
          add (content ^ String.make pad '\000');
          String.sub (List.hd kvs) 0 Params.key_sz
    in

    create true depth n |> ignore;
    Store.Private.end_migration store (!address + 1) !address;
    incr address;
    { store }

  let pp ppf t =
    Fmt.pf ppf "@[<hov 2>ROOT OF THE TREE:@;%a@]" Leaf.pp (Leaf.load t.store (Store.root t.store))

  module Private = struct
    let dir tree = Store.Private.dir tree.store

    let root tree = Store.root tree.store

    let store tree = tree.store

    let pp t ppf address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load t.store address in
          Fmt.set_style_renderer ppf `Ansi_tty;
          Fmt.pf ppf "%a@." Leaf.pp leaf
      | Node _n ->
          let node = Node.load t.store address in
          Fmt.set_style_renderer ppf `Ansi_tty;
          Fmt.pf ppf "%a@." (Node.pp |> Fmt.vbox) node

    let go_to_leaf tree inkey =
      let key = Key.of_input inkey in
      let rec aux tree key address acc =
        let page = Store.load tree.store address in
        match Page.kind page |> Common.Kind.from_t with
        | Leaf -> address :: acc
        | Node _depth ->
            let node = Node.load tree.store address in
            aux tree key (Node.find node key) (address :: acc)
      in
      aux tree key (Store.root tree.store) []

    module Params = Params
    module Common = Common
    module Entry = Entry
    module Key = Key
    module Value = Value
    module Store = Store
    module Page = Page
    module Leaf = Leaf
    module Node = Node
  end
end
