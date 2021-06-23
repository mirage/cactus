include Store_intf

module type HEADER = sig
  module Common : Field.COMMON

  type t

  val size : int

  val load : bytes -> t

  val dump : t -> bytes

  val init : t -> root:int -> unit

  val pp : Format.formatter -> t -> unit

  (* val g_magic : t -> Common.Magic.t

     val s_magic : t -> Common.Magic.t -> unit *)

  val g_root : t -> Common.Address.t

  val s_root : t -> Common.Address.t -> unit
end

module Make (Params : Params.S) (Common : Field.COMMON) = struct
  module Common = Common

  module Header : HEADER with module Common := Common = struct
    type t = bytes

    open Common

    let sizes = [ Magic.size; Address.size ]

    type offsets = { magic : int; root : int }

    let offsets =
      match Utils.sizes_to_offsets sizes with
      | [ magic; root ] -> { magic; root }
      | _ -> failwith "Incorrect offsets"

    let size = List.fold_left ( + ) 0 sizes

    let load buff = buff

    let g_magic t = Magic.get t ~off:offsets.magic

    let s_magic t magic = Magic.set ~marker:Utils.nop t ~off:offsets.magic magic

    let g_root t = Address.get t ~off:offsets.root

    let s_root t root = Address.set ~marker:Utils.nop t ~off:offsets.root root

    let init t ~root =
      s_magic t @@ Magic.to_t @@ Params.page_magic;
      s_root t @@ Address.to_t @@ root

    let pp ppf t =
      Fmt.pf ppf "@[<v 2>Header:@;Magic:%a@;Root:%a@]" Magic.pp (g_magic t) Address.pp (g_root t)

    let dump t = t

    let () =
      ignore g_magic;
      ignore s_magic
  end

  open Stats.Func
  (** STAT WRAPPERS **)

  open Stats.Store

  type address = int

  type content = { buff : bytes; mutable dirty : bool }

  module AddressHash = struct
    type t = address

    let equal = Int.equal

    let hash = Hashtbl.hash
  end

  module UnweightedContent = struct
    type t = content

    let weight _ = 1
  end

  module CaliforniaCache = Cache.Make (AddressHash) (UnweightedContent)

  type t = {
    mutable n_pages : int;
    mutable dead_pages : int list;
    header : Header.t;
    fd : Unix.file_descr;
    dir : string;
    cache : CaliforniaCache.t;
  }

  type page = { address : address; store : t; content : content }

  module Page = struct
    type pointer = int

    type t = page

    let max_size = Params.page_sz

    let real_offset address = max_size * address

    let _load fd address buff =
      let start = real_offset address in
      Utils.assert_pread fd buff start max_size;
      increment stat_io_r "nb_bytes" max_size;
      let content = { buff; dirty = false } in
      content

    let load fd address =
      let buff = Bytes.create max_size in
      _load fd address buff

    let load_using fd ?available address =
      match available with
      | None -> _load fd address (Bytes.create max_size)
      | Some content -> _load fd address content.buff

    let _flush fd address content =
      if content.dirty then (
        content.dirty <- false;
        tic stat_io_w;
        let write_size =
          Syscalls.pwrite ~fd
            ~fd_offset:(real_offset address |> Optint.Int63.of_int)
            ~buffer:content.buff ~buffer_offset:0 ~length:max_size
        in
        assert (write_size = max_size);
        Index_stats.add_write write_size;
        tac stat_io_w;
        increment stat_io_w "nb_bytes" write_size)

    let flush t = _flush t.store.fd t.address t.content

    let buff t = t.content.buff

    let marker t () = t.content.dirty <- true

    let _buff0 = Bytes.make max_size '\000'

    let init store address =
      tic stat_io_w;
      let write_size =
        Syscalls.pwrite ~fd:store.fd
          ~fd_offset:(real_offset address |> Optint.Int63.of_int)
          ~buffer:_buff0 ~buffer_offset:0 ~length:max_size
      in
      assert (write_size = max_size);
      Index_stats.add_write write_size;
      tac stat_io_w;
      increment stat_io_w "nb_bytes" write_size

    (* Header functions *)

    type offsets = { magic : int; kind : int }

    let sizes = Common.[ Magic.size; Kind.size ]

    let offsets =
      match Utils.sizes_to_offsets sizes with
      | [ magic; kind ] -> { magic; kind }
      | _ -> failwith "Invalid offsets"

    let _kind content = Common.Kind.get content.buff ~off:offsets.kind

    let kind t = _kind t.content
  end

  let fsync t =
    tic stat_fsync;
    Unix.fsync t.fd;
    tac stat_fsync

  let release t = CaliforniaCache.release t.cache

  let release_ro t = CaliforniaCache.clear t.cache

  let flush t =
    (* Log.debug (fun reporter -> reporter "Running flush");*)
    tic stat_flush;
    CaliforniaCache.flush t.cache;
    tac stat_flush

  let load t address = { address; store = t; content = CaliforniaCache.find t.cache address }

  let reload t address = CaliforniaCache.reload t.cache address

  let allocate t =
    let ret =
      match t.dead_pages with
      | [] ->
          let n = t.n_pages in
          Page.init t n (* write junk bytes to increase the b.tree file length *);
          t.n_pages <- n + 1;
          n
      | r :: q ->
          t.dead_pages <- q;
          r
    in
    ret

  let deallocate t address =
    t.dead_pages <- address :: t.dead_pages;
    CaliforniaCache.deallocate t.cache address

  let flush_header t = Utils.assert_pwrite t.fd (Header.dump t.header) 0 Header.size

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        Unix.mkdir dir 0o755;
        k ())
    in
    aux dirname (fun () -> ())

  let max_pages = Params.cache_sz * 1_000_000 / Params.page_sz

  let cache_height =
    let f = Params.fanout |> Float.of_int in
    let max_levels =
      Float.to_int
      @@ (Float.log ((Float.of_int max_pages *. ((2. *. f) -. 1.)) +. 1.) /. Float.log (2. *. f))
    in
    max_levels - 1
  (* leaf height is 0 *)

  let root t = Header.g_root t.header |> Common.Address.from_t

  let check_height t =
    let tree_height = root t |> load t |> Page.kind |> Common.Kind.to_depth in
    Log.debug (fun reporter -> reporter "Tree height is %i" tree_height);
    if tree_height > cache_height then
      Log.warn (fun reporter ->
          reporter "Last %i leaf/node levels are not cached" (tree_height - cache_height))

  let reroot t address =
    ignore CaliforniaCache.update_filter;
    address |> Common.Address.to_t |> Header.s_root t.header;
    flush_header t;
    check_height t;
    let tree_height = address |> load t |> Page.kind |> Common.Kind.to_depth in
    if tree_height > cache_height then
      CaliforniaCache.update_filter t.cache ~filter:(fun content ->
          let depth = Page._kind content |> Common.Kind.to_depth in
          if tree_height - depth <= cache_height then `California
          else if tree_height - depth = cache_height + 1 then `Lru
          else `Volatile)

  (* only cache the top cache_height part of the tree *)

  let init ~root =
    Log.info (fun reporter -> reporter "Cache height is %i" cache_height);
    let ( // ) x y = x ^ "/" ^ y in
    if not (Sys.file_exists root) then mkdir root;
    let file = root // "b.tree" in

    let california_capacity =
      (Utils.pow (2 * Params.fanout) (cache_height + 1) - 1) / ((2 * Params.fanout) - 1)
    in
    let lru_capacity = max_pages - california_capacity in
    Log.debug (fun reporter -> reporter "California can hold up to %i pages" california_capacity);
    Log.debug (fun reporter -> reporter "Lru can hold up to %i pages" lru_capacity);

    if Sys.file_exists file then (
      Log.debug (fun reporter -> reporter "Loading btree file found at %s" file);

      let fd = Unix.openfile file Unix.[ O_RDWR ] 0o600 in

      let buff = Bytes.create Header.size in
      Utils.assert_pread fd buff 0 Header.size;
      let header = Header.load buff in

      let file_size = fd |> Unix.fstat |> fun x -> x.st_size in
      let n_pages = file_size / Params.page_sz in

      let tree_height =
        Header.g_root header
        |> Common.Address.from_t
        |> Page.load fd
        |> Page._kind
        |> Common.Kind.to_depth
      in

      let cache =
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load_using fd)
          ~filter:(fun content ->
            let depth = Page._kind content |> Common.Kind.to_depth in
            if tree_height - depth <= cache_height then `California
            else if tree_height - depth = cache_height + 1 then `Lru
            else `Volatile)
          (* only cache the top cache_height part of the tree *)
          california_capacity lru_capacity
      in

      let t = { n_pages; dead_pages = []; header; fd; dir = root; cache } in
      check_height t;
      t)
    else
      let fd = Unix.openfile file Unix.[ O_RDWR; O_CREAT; O_EXCL ] 0o600 in
      let cache =
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load_using fd)
          ~filter:(fun content ->
            Page._kind content |> Common.Kind.to_depth |> fun _ -> `California)
          california_capacity lru_capacity
      in
      let header = Bytes.create Header.size |> Header.load in
      let store = { n_pages = 0; dead_pages = []; header; fd; dir = root; cache } in

      ignore (allocate store) (* create page_0 for the header *);
      let root = allocate store in
      Header.init header ~root;
      flush_header store;
      store

  let clear t =
    t.n_pages <- 0;
    ignore (allocate t);
    (* create page_0 for the header *)
    allocate t |> Common.Address.to_t |> Header.s_root t.header;
    CaliforniaCache.full_clear t.cache

  let close t =
    flush t;
    Unix.close t.fd

  let iter t func =
    for i = 1 to t.n_pages - 1 do
      load t i |> func i
    done

  let pp_header ppf t =
    Fmt.pf ppf "@[<v 2>Header:@;%a@]@;@[Deallocated pages:@;%a@]" Header.pp t.header
      Fmt.(list int)
      t.dead_pages

  module Private = struct
    let dir t = t.dir

    let cache_size t = (t.cache |> Obj.repr |> Obj.reachable_words) * Sys.word_size / 8

    let write t s =
      let l = String.length s in
      let write_size = Unix.write_substring t.fd s 0 l in
      assert (write_size = l)

    let init_migration t = Unix.(lseek t.fd (root t * Params.page_sz) SEEK_SET) |> ignore

    let end_migration t n root =
      t.n_pages <- n;
      reroot t root
  end
end
