module type H = sig
  module Common : Header.Common (* module where live header fields common to all pages *)

  module Entry_number : Header.IntHeaderField

  module Dead_entry_number : Header.IntHeaderField

  type t = {
    magic : Common.Magic.t;
    kind : Common.Kind.t;
    version : Common.Version.t;
    entry_number : Entry_number.t;
  }

  val load : bytes -> t

  val init : Field.kind -> bytes -> t

  val size : int

  val pp : Format.formatter -> t -> unit (* human readable representation, for debugging *)
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) ->
  H with module Common = Header.MakeCommon(Params)

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  ->
  struct
    module Common = Header.MakeCommon (Params)

    module Entry_number = Header.MakeInt (struct
      let size = Params.max_key_sz
    end)

    module Dead_entry_number = Header.MakeInt (struct
      let size = Params.max_key_sz
    end)

    type t = {
      magic : Common.Magic.t;
      kind : Common.Kind.t;
      version : Common.Version.t;
      entry_number : Entry_number.t;
    }

    let sizes = [ Common.Magic.size; Common.Kind.size; Common.Version.size; Entry_number.size ]

    let offsets = Utils.sizes_to_offsets sizes

    let size = List.fold_left ( + ) 0 sizes

    let load buff =
      match offsets with
      | [ magic; kind; version; entry_number ] ->
          {
            magic = Common.Magic.v buff ~off:magic;
            kind = Common.Kind.v buff ~off:kind;
            version = Common.Version.v buff ~off:version;
            entry_number = Entry_number.v buff ~off:entry_number;
          }
      | _ -> failwith "Incorrect node header"

    let init (as_kind : Field.kind) buff =
      let t = load buff in
      Common.Magic.set t.magic Params.page_magic;
      Common.Kind.set t.kind as_kind;
      Common.Version.set t.version 2;
      Entry_number.set t.entry_number 0;
      t

    let pp ppf (t : t) =
      let open Fmt in
      pf ppf
        "@[<hov 1>magic:@ %a%a@]@;\
         @[<hov 1>kind:@ %a%a@]@;\
         @[<hov 1>version:@ %a%a@]@;\
         @[<hov 1>entry_number:@ %a%a@]"
        (Common.Magic.pp_raw |> styled (`Fg `Magenta))
        t.magic
        (Common.Magic.pp |> styled (`Bg `Magenta) |> styled `Reverse)
        t.magic
        (Common.Kind.pp_raw |> styled (`Fg `Magenta))
        t.kind
        (Common.Kind.pp |> styled (`Bg `Magenta) |> styled `Reverse)
        t.kind
        (Common.Version.pp_raw |> styled (`Fg `Magenta))
        t.version
        (Common.Version.pp |> styled (`Bg `Magenta) |> styled `Reverse)
        t.version
        (Entry_number.pp_raw |> styled (`Fg `Magenta))
        t.entry_number
        (Entry_number.pp |> styled (`Bg `Magenta) |> styled `Reverse)
        t.entry_number
  end
