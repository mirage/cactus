let () = Random.init 42

module MyKey = struct
  type t = string [@@deriving repr]

  let encode = Fun.id

  let decode = Fun.id

  let encoded_size = 24

  let to_t : string -> t = function
    | s ->
        if String.length s >= encoded_size then String.sub s 0 encoded_size
        else String.make (encoded_size - String.length s) 'x' ^ s
end

let pad len c s =
  if String.length s > len then failwith "Impossible padding"
  else String.make (len - String.length s) c ^ s

let unpad c_pad s =
  let _, padd =
    Seq.fold_left
      (fun (found, off) c ->
        match found with
        | true -> (found, off)
        | false -> if c = c_pad then (false, off + 1) else (true, off))
      (false, 0) (String.to_seq s)
  in
  String.sub s padd (String.length s - padd)

module MyValue = struct
  type t = int * int * int [@@deriving repr]

  let encoded_size = 30

  let encode x = x |> Repr.to_string t |> pad encoded_size '\003'

  let decode s =
    s |> unpad '\003' |> Repr.of_string t |> function Ok t -> t | Error (`Msg msg) -> failwith msg
end

module Input = Btree.Input.Default.Size

let generate_key () =
  MyKey.to_t (String.init MyKey.encoded_size (fun _i -> Char.chr (48 + Random.int 74)))

module MyBtree0 = struct
  include
    Btree.Make (MyKey) (MyValue)
      (struct
        include Input

        let version = 0

        let page_sz = 4050

        let fanout = 30

        let debug = true
      end)

  let create ~root : t = create root
end

module type TREE = sig
  include Btree.S

  val create : root:string -> t
end
with type key = MyKey.t
 and type value = MyValue.t

let get_tree version = match version with `V0 -> (module MyBtree0 : TREE)

let get_migrate_tree version = match version with `V0 -> (module MyBtree0 : TREE)

let reporter ppf statsppf =
  let counter = Mtime_clock.counter () in
  let report _src level ~over k msgf =
    let dt = Mtime_clock.count counter |> Mtime.Span.to_ms in
    let k _ =
      over ();
      k ()
    in
    let print header tags k fmt =
      let open Btree.Private.Tag in
      let kind = match tags with None -> None | Some tags -> Logs.Tag.find kind_tag tags in
      match kind with
      | Some Stats -> Fmt.kpf k statsppf ("%04.0f;@[" ^^ fmt ^^ "@]@.") dt
      | _ ->
          Fmt.kpf k ppf ("[%+04.0fms] %a @[" ^^ fmt ^^ "@]@.") dt Logs_fmt.pp_header (level, header)
    in
    msgf @@ fun ?header ?tags fmt -> print header tags k fmt
  in
  { Logs.report }

let set_report root =
  let logchan = open_out_gen [ Open_append; Open_creat; Open_trunc ] 0o600 (root ^ "/" ^ "b.log") in
  let statchan =
    open_out_gen [ Open_append; Open_creat; Open_trunc ] 0o600 (root ^ "/" ^ "stats.log")
  in
  let logppf = Format.formatter_of_out_channel logchan in
  let statppf = Format.formatter_of_out_channel statchan in
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter logppf statppf)

(* let create root =
  let tree = MyBtree.create ~root in
  set_report root;
  tree
 *)

let v_to_s version = match version with `V0 -> "v0"
