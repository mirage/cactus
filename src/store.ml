(* TODO : allocation should check for dead pages before allocating a new one*)
(* TODO : Utils.assert_write *)

include Store_intf

exception Page_overflow

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

module CaliforniaCache : sig
  (* You can (almost) never leave the california cache *)

  type ('address, 'content) t

  val v :
    flush:('address -> 'content -> unit) ->
    load:('address -> 'content) ->
    filter:('content -> bool) ->
    int ->
    ('address, 'content) t

  val find : ('address, 'content) t -> 'address -> 'content

  val reload : ('address, 'content) t -> 'address -> unit

  val update_filter : ('address, 'content) t -> filter:('content -> bool) -> unit

  val release : ('address, 'content) t -> unit

  val clear : ('address, 'content) t -> unit

  val full_clear : ('address, 'content) t -> unit

  val flush : ('address, 'content) t -> unit
end = struct
  type ('address, 'content) t = {
    california : ('address, 'content) Hashtbl.t;
    volatile : ('address, 'content) Hashtbl.t;
    flush : 'address -> 'content -> unit;
    load : 'address -> 'content;
    mutable filter : 'content -> bool;
  }

  let v ~flush ~load ~filter n =
    { flush; load; california = Hashtbl.create n; volatile = Hashtbl.create 16; filter }

  let find t address =
    match Hashtbl.find_opt t.california address with
    | Some content ->
        assert (not (Hashtbl.mem t.volatile address));
        content
    | None -> (
        match Hashtbl.find_opt t.volatile address with
        | Some content -> content
        | None ->
            let content = t.load address in
            if t.filter content then Hashtbl.add t.california address content
            else (
              Hashtbl.add t.volatile address content;
              if Hashtbl.length t.volatile > 64 then (
                Log.warn (fun reporter -> reporter "Not enough release");
                assert false));
            content)

  let reload t address =
    if Hashtbl.mem t.volatile address then
      let content = Hashtbl.find t.volatile address in
      if t.filter content then (
        Hashtbl.add t.california address content;
        Hashtbl.remove t.volatile address)

  let update_filter t ~filter =
    t.filter <- filter;
    Hashtbl.filter_map_inplace
      (fun key content ->
        if filter content then Some content
        else (
          t.flush key content;
          None))
      t.california

  let release t =
    Hashtbl.iter t.flush t.volatile;
    Hashtbl.clear t.volatile

  let clear t = Hashtbl.clear t.volatile

  let full_clear t =
    Hashtbl.clear t.volatile;
    Hashtbl.clear t.california

  let flush t =
    Hashtbl.iter t.flush t.california;
    release t
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

    let s_magic t magic = Magic.set t ~off:offsets.magic magic

    let g_root t = Address.get t ~off:offsets.root

    let s_root t root = Address.set t ~off:offsets.root root

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

  type t = {
    mutable n_pages : int;
    mutable dead_pages : int list;
    header : Header.t;
    fd : Unix.file_descr;
    dir : string;
    cache : (address, content) CaliforniaCache.t;
  }

  type page = { address : address; store : t; content : content }

  module Page = struct
    type pointer = int

    type t = page

    let max_size = Params.page_sz

    let real_offset address = max_size * address

    let load fd address =
      tic stat_load;
      let buff = Bytes.create max_size in
      let start = real_offset address in
      tic stat_io_r;
      Unix.lseek fd start Unix.SEEK_SET |> ignore;
      Utils.assert_read fd buff 0 max_size;
      tac stat_io_r;
      increment stat_io_r "nb_bytes" max_size;
      let content = { buff; dirty = false } in
      tac stat_load;
      content

    let _flush fd address content =
      if content.dirty || Params.version = 0 then (
        content.dirty <- false;
        tic stat_io_w;
        let write_size =
          Syscalls.pwrite ~fd
            ~fd_offset:(real_offset address |> Optint.Int63.of_int)
            ~buffer:content.buff ~buffer_offset:0 ~length:max_size
        in
        assert (write_size = max_size);
        tac stat_io_w;
        increment stat_io_w "nb_bytes" max_size)

    let flush t = _flush t.store.fd t.address t.content

    let write t ?(with_flush = false) ~offset buff =
      tic stat_write;
      if offset + Bytes.length buff > max_size then raise Page_overflow;
      t.content.dirty <- true;
      Bytes.blit buff 0 t.content.buff offset (Bytes.length buff);
      if with_flush then flush t;
      tac stat_write

    let buff t = t.content.buff

    let _buff0 = Bytes.make max_size '\000'

    let init store address =
      tic stat_io_w;
      let write_size =
        Syscalls.pwrite ~fd:store.fd
          ~fd_offset:(real_offset address |> Optint.Int63.of_int)
          ~buffer:_buff0 ~buffer_offset:0 ~length:max_size
      in
      (* ignore (Unix.lseek store.fd (real_offset address) Unix.SEEK_SET);
         let write_size = Unix.write store.fd _buff0 0 max_size in
      *)
      assert (write_size = max_size);
      tac stat_io_w;
      increment stat_io_w "nb_bytes" max_size

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

  let release t =
    tic stat_release;
    CaliforniaCache.release t.cache;
    tac stat_release

  let release_ro t = CaliforniaCache.clear t.cache

  let flush t =
    Log.debug (fun reporter -> reporter "Running flush");
    CaliforniaCache.flush t.cache;
    fsync t

  let load t address = { address; store = t; content = CaliforniaCache.find t.cache address }

  let reload t address = CaliforniaCache.reload t.cache address

  let allocate t =
    tic stat_allocate;
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
    tac stat_allocate;
    ret

  let deallocate t address = t.dead_pages <- address :: t.dead_pages

  let rewrite_header t =
    (* page 0 is reserved for the header *)
    let page = load t 0 in
    Page.write page ~with_flush:true ~offset:0 (Header.dump t.header);
    fsync t

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

  let cache_height =
    Params.cache_sz |> Float.of_int |> fun x ->
    Float.log x /. Float.log (Float.of_int Params.fanout) |> Float.to_int |> fun x -> x + 1

  let init ~root =
    Log.info (fun reporter -> reporter "Cache height is %i" cache_height);
    let ( // ) x y = x ^ "/" ^ y in
    if not (Sys.file_exists root) then mkdir root;
    let file = root // "b.tree" in

    if Sys.file_exists file then (
      Log.debug (fun reporter -> reporter "Loading btree file found at %s" file);

      let fd = Unix.openfile file Unix.[ O_RDWR ] 0o600 in

      let buff = Bytes.create Header.size in
      Unix.lseek fd 0 Unix.SEEK_SET |> ignore;
      Utils.assert_read fd buff 0 Header.size;
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
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load fd)
          ~filter:(fun content ->
            Page._kind content |> Common.Kind.to_depth |> fun x -> tree_height - x <= cache_height)
          (* only cache the top cache_height part of the tree *)
          Params.cache_sz
      in

      { n_pages; dead_pages = []; header; fd; dir = root; cache })
    else
      let fd = Unix.openfile file Unix.[ O_RDWR; O_CREAT; O_EXCL ] 0o600 in
      let cache =
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load fd)
          ~filter:(fun content -> Page._kind content |> Common.Kind.to_depth |> fun _ -> true)
          Params.cache_sz
      in
      let header = Bytes.create Header.size |> Header.load in
      let store = { n_pages = 0; dead_pages = []; header; fd; dir = root; cache } in

      ignore (allocate store) (* create page_0 for the header *);
      let root = allocate store in
      Header.init header ~root;
      rewrite_header store;
      store

  let clear t =
    t.n_pages <- 0;
    ignore (allocate t);
    (* create page_0 for the header *)
    allocate t |> Common.Address.to_t |> Header.s_root t.header;
    rewrite_header t;
    fsync t;
    CaliforniaCache.full_clear t.cache

  let root t = Header.g_root t.header |> Common.Address.from_t

  let reroot t address =
    ignore CaliforniaCache.update_filter;
    address |> Common.Address.to_t |> Header.s_root t.header;
    rewrite_header t;
    let tree_height = address |> Page.load t.fd |> Page._kind |> Common.Kind.to_depth in
    if tree_height > cache_height then (
      Log.warn (fun reporter ->
          reporter "Last %i leaf/node levels are not cached" (tree_height - cache_height));

      CaliforniaCache.update_filter t.cache ~filter:(fun content ->
          Page._kind content |> Common.Kind.to_depth |> fun x -> tree_height - x <= cache_height));

    (* only cache the top cache_height part of the tree *)
    fsync t

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
