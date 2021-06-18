include Vertex_header_intf

module Make (Params : Params.S) (Store : Store.S) (Common : Field.COMMON) = struct
  open Common

  module Nentry = Field.MakeInt (struct
    let size = Params.max_key_sz
  end)

  module Ndeadentry = Field.MakeInt (struct
    let size = Params.max_key_sz
  end)

  type t = { buff : bytes; marker : unit -> unit }

  type offsets = { magic : int; kind : int; nentry : int; ndeadentry : int }

  let sizes = [ Magic.size; Kind.size; Nentry.size; Ndeadentry.size ]

  let offsets =
    match Utils.sizes_to_offsets sizes with
    | [ magic; kind; nentry; ndeadentry ] -> { magic; kind; nentry; ndeadentry }
    | _ -> failwith "Incorrect offsets"

  let size = List.fold_left ( + ) 0 sizes

  let load ~marker buff = { buff; marker }

  let init t kind =
    Magic.to_t Params.page_magic |> Magic.set ~marker:t.marker t.buff ~off:offsets.magic;
    Kind.to_t kind |> Kind.set ~marker:t.marker t.buff ~off:offsets.kind;
    Nentry.to_t 0 |> Nentry.set ~marker:t.marker t.buff ~off:offsets.nentry

  let g_magic t = Magic.get t.buff ~off:offsets.magic

  let s_magic t magic = Magic.set ~marker:t.marker t.buff ~off:offsets.magic magic

  let g_kind t = Kind.get t.buff ~off:offsets.kind

  let s_kind t kind = Kind.set ~marker:t.marker t.buff ~off:offsets.kind kind

  let g_nentry t = Nentry.get t.buff ~off:offsets.nentry

  let s_nentry t nentry = Nentry.set ~marker:t.marker t.buff ~off:offsets.nentry nentry

  let g_ndeadentry t = Ndeadentry.get t.buff ~off:offsets.ndeadentry

  let s_ndeadentry t ndeadentry =
    Ndeadentry.set ~marker:t.marker t.buff ~off:offsets.ndeadentry ndeadentry

  let pp ppf (t : t) =
    let open Fmt in
    pf ppf
      "@[<hov 1>magic:@ %a%a@]@;\
       @[<hov 1>kind:@ %a%a@]@;\
       @[<hov 1>entry number:@ %a%a@]@;\
       @[<hov 1>dead entry number:@ %a%a@]"
      (Magic.pp_raw ~off:offsets.magic |> styled (`Fg `Magenta))
      t.buff
      (Magic.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      (g_magic t)
      (Kind.pp_raw ~off:offsets.kind |> styled (`Fg `Magenta))
      t.buff
      (Kind.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      (g_kind t)
      (Nentry.pp_raw ~off:offsets.nentry |> styled (`Fg `Magenta))
      t.buff
      (Nentry.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      (g_nentry t)
      (Ndeadentry.pp_raw ~off:offsets.ndeadentry |> styled (`Fg `Magenta))
      t.buff
      (Ndeadentry.pp |> styled (`Bg `Magenta) |> styled `Reverse)
      (g_ndeadentry t)
end
