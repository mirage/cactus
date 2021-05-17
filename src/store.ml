(* TODO : allocation should check for dead pages before allocating a new one*)
(* TODO : Utils.assert_write *)

include Store_intf

exception Page_overflow

module MakeHeader (Common : Field.COMMON) (Params : Params.S) = struct
  (* store header *)

  type t = {
    magic : Common.Magic.t;
    mutable root : Common.Address.t;
    mutable lock_tbl : Common.Address.t;
    mutable dead_tbl : Common.Address.t;
  }

  let init () =
    {
      magic =
        Params.page_magic
        |> Encoder.load
        |> Common.Magic.decode ~size:(String.length Params.page_magic);
      root = Common.Address.of_int (-1);
      lock_tbl = Common.Address.of_int (-1);
      dead_tbl = Common.Address.of_int (-1);
    }

  let sizes = [ Common.Magic.size; Common.Address.size; Common.Address.size; Common.Address.size ]

  let offsets = Utils.sizes_to_offsets sizes

  let size = List.fold_left ( + ) 0 sizes

  let load s =
    match List.fold_right2 (fun off len acc -> Encoder.sub s off len :: acc) offsets sizes [] with
    | [ magic; root; lock_tbl; dead_tbl ] ->
        {
          magic = Common.Magic.decode ~size:(String.length Params.page_magic) magic;
          root = Common.Address.decode root;
          lock_tbl = Common.Address.decode lock_tbl;
          dead_tbl = Common.Address.decode dead_tbl;
        }
    | _ -> failwith "this cannot happen -- famous last words."

  let dump { magic; root; lock_tbl; dead_tbl } =
    Encoder.concat
      [
        Common.Magic.encode magic;
        Common.Address.encode root;
        Common.Address.encode lock_tbl;
        Common.Address.encode dead_tbl;
      ]

  let pp ppf (header : t) =
    let open Fmt in
    let magic = Common.Magic.encode header.magic in
    let root = Common.Address.encode header.root in
    let lock_tbl = Common.Address.encode header.lock_tbl in
    let dead_tbl = Common.Address.encode header.dead_tbl in

    let encoder_pp = Encoder.pp |> styled (`Fg `Magenta) in
    pf ppf
      "@[<hov 1>magic:@ %a%a@]@;\
       @[<hov 1>root:@ %a%a@]@;\
       @[<hov 1>locks:@ %a%a@]@;\
       @[<hov 1>deads:@ %a%a@]@]" encoder_pp magic
      (Common.Magic.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      header.magic encoder_pp root
      (Common.Address.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      header.root encoder_pp lock_tbl
      (Common.Address.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      header.lock_tbl encoder_pp dead_tbl
      (Common.Address.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      header.dead_tbl
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

module Make (Params : Params.S) = struct
  module Common = Field.MakeCommon (Params)
  module BtreeHeader = MakeHeader (Common) (Params)

  open Stats.Func
  (** STAT WRAPPERS **)

  open Stats.Store

  type address = int

  type content = { buff : bytes; mutable dirty : bool }

  type t = {
    mutable n_pages : int;
    header : BtreeHeader.t;
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
        (* ignore (Unix.lseek fd (real_offset address) Unix.SEEK_SET);
           let write_size = Unix.write fd content.buff 0 max_size in
        *)
        assert (write_size = max_size);
        tac stat_io_w;
        increment stat_io_w "nb_bytes" max_size)

    let flush t = _flush t.store.fd t.address t.content

    let read t ~offset ~length =
      tic stat_read;
      let ret = Bytes.sub_string t.content.buff offset length |> Encoder.load in
      tac stat_read;
      ret

    let write t ?(with_flush = false) ~offset s =
      tic stat_write;
      if offset + Encoder.length s > max_size then raise Page_overflow;
      t.content.dirty <- true;
      let s = Encoder.dump s in
      Bytes.blit_string s 0 t.content.buff offset (String.length s);
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

    module Header = struct
      type h = { magic : Common.Magic.t; kind : Common.Kind.t; version : Common.Version.t }

      let sizes = Common.[ Magic.size; Kind.size; Version.size ]

      let size = List.fold_left ( + ) 0 sizes

      let offsets = Utils.sizes_to_offsets sizes

      let kind content =
        let offset = List.nth offsets 1 in
        Bytes.sub_string content.buff offset Common.Kind.size |> Encoder.load |> Common.Kind.decode

      let load t =
        let full_header = read t ~offset:0 ~length:size in
        match
          List.fold_right2
            (fun off len acc -> Encoder.sub full_header off len :: acc)
            offsets sizes []
        with
        | [ magic; kind; version ] ->
            {
              magic = Common.Magic.decode ~size:(String.length Params.page_magic) magic;
              kind = Common.Kind.decode kind;
              version = Common.Version.decode version;
            }
        | _ -> failwith "this will not happen -- famous last words"

      let pp_raw ppf t =
        let full_header = read t ~offset:0 ~length:size in
        Fmt.pf ppf "Header : %s" (full_header |> Encoder.dump |> Hex.of_string |> Hex.show)
    end
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
    let n = t.n_pages in
    Page.init t n (* write junk bytes to increase the b.tree file length *);
    t.n_pages <- n + 1;
    tac stat_allocate;
    n

  let rewrite_header t =
    (* page 0 is reserved for the header *)
    let page = load t 0 in
    Page.write page ~with_flush:true ~offset:0 (BtreeHeader.dump t.header);
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
      Unix.lseek fd 0 Unix.SEEK_SET |> ignore;
      let buff = Bytes.create BtreeHeader.size in
      Utils.assert_read fd buff 0 BtreeHeader.size;
      let header = buff |> Bytes.to_string |> Encoder.load |> BtreeHeader.load in
      let file_size = fd |> Unix.fstat |> fun x -> x.st_size in
      let n_pages = file_size / Params.page_sz in
      let tree_height =
        header.root
        |> Common.Address.to_int
        |> Page.load fd
        |> Page.Header.kind
        |> Common.Kind.to_depth
      in
      let cache =
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load fd)
          ~filter:(fun content ->
            Page.Header.kind content |> Common.Kind.to_depth |> fun x ->
            tree_height - x <= cache_height)
          Params.cache_sz
      in
      { n_pages; header; fd; dir = root; cache })
    else
      let fd = Unix.openfile file Unix.[ O_RDWR; O_CREAT; O_EXCL ] 0o600 in
      let cache =
        CaliforniaCache.v ~flush:(Page._flush fd) ~load:(Page.load fd)
          ~filter:(fun content -> Page.Header.kind content |> Common.Kind.to_depth |> fun _ -> true)
          Params.cache_sz
      in
      let store = { n_pages = 0; header = BtreeHeader.init (); fd; dir = root; cache } in
      ignore (allocate store);
      (* create page_0 for the header *)
      store.header.lock_tbl <- allocate store |> Common.Address.of_int;
      store.header.dead_tbl <- allocate store |> Common.Address.of_int;
      store.header.root <- allocate store |> Common.Address.of_int;
      rewrite_header store;
      store

  let clear t =
    t.n_pages <- 0;
    ignore (allocate t);
    (* create page_0 for the header *)
    t.header.lock_tbl <- allocate t |> Common.Address.of_int;
    t.header.dead_tbl <- allocate t |> Common.Address.of_int;
    t.header.root <- allocate t |> Common.Address.of_int;
    rewrite_header t;
    fsync t;
    CaliforniaCache.full_clear t.cache

  let root t = t.header.root |> Common.Address.to_int

  let reroot t address =
    ignore CaliforniaCache.update_filter;
    t.header.root <- address |> Common.Address.of_int;
    rewrite_header t;
    let tree_height =
      address |> load t |> Page.Header.load |> fun x -> Common.Kind.to_depth x.kind
    in
    if tree_height > cache_height then (
      Log.warn (fun reporter ->
          reporter "Last %i leaf/node levels are not cached" (tree_height - cache_height));

      CaliforniaCache.update_filter t.cache ~filter:(fun content ->
          Page.Header.kind content |> Common.Kind.to_depth |> fun x ->
          tree_height - x <= cache_height));

    fsync t

  let iter store func =
    for i = 3 to store.n_pages - 1 do
      load store i |> func i
    done

  let pp_header ppf store = Fmt.pf ppf "@[<v>%a@]" BtreeHeader.pp store.header

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
