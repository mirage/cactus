module type HeaderField = sig
  type t [@@deriving repr]

  val v : bytes -> off:int -> t

  val size : int

  val pp_raw : Format.formatter -> t -> unit (* pp the bytes *)

  val pp : Format.formatter -> t -> unit
end

module type IntHeaderField = sig
  include HeaderField

  val to_int : t -> int

  val set : t -> int -> unit
end

module type StringHeaderField = sig
  include HeaderField

  val to_string : t -> string

  val set : t -> string -> unit
end

module type BoolHeaderField = sig
  include HeaderField

  val to_bool : t -> bool

  val set : t -> bool -> unit
end

module type Size = sig
  val size : int
end

module MakeInt (Size : Size) : IntHeaderField = struct
  type t = { off : int; buff : bytes } [@@deriving repr]

  let size = Size.size

  let get, set =
    match size with
    | 1 -> Bytes.(get_uint8, set_uint8)
    | 2 -> Bytes.(get_uint16_be, set_uint16_be)
    | 4 ->
        Bytes.
          ( (fun buff i -> get_int32_be buff i |> Int32.to_int),
            fun buff i n -> n |> Int32.of_int |> set_int32_be buff i )
    | 8 ->
        Bytes.
          ( (fun buff i -> get_int64_be buff i |> Int64.to_int),
            fun buff i n -> n |> Int64.of_int |> set_int64_be buff i )
    | n -> failwith (Fmt.str "Unsupported int length %i" n)

  let v buff ~off =
    assert (Bytes.length buff >= off + size);
    { buff; off }

  let to_int t = get t.buff t.off

  let set t n = set t.buff t.off n

  let pp ppf t = t |> to_int |> Fmt.int ppf

  let pp_raw ppf t =
    Bytes.sub_string t.buff t.off size |> Hex.of_string |> Hex.show |> Fmt.pf ppf "%s"
end

module MakeString (Size : Size) : StringHeaderField = struct
  type t = { off : int; buff : bytes } [@@deriving repr]

  let size = Size.size

  let v buff ~off =
    assert (Bytes.length buff >= off + size);
    { buff; off }

  let to_string t = Bytes.sub_string t.buff t.off size

  let set t s =
    assert (String.length s = size);
    Bytes.blit_string s 0 t.buff t.off size

  let pp ppf t = t |> to_string |> Fmt.string ppf

  let pp_raw ppf t =
    Bytes.sub_string t.buff t.off size |> Hex.of_string |> Hex.show |> Fmt.pf ppf "%s"
end

module Flag : BoolHeaderField = struct
  type t = { off : int; buff : bytes } [@@deriving repr]

  let size = 1

  let v buff ~off =
    assert (Bytes.length buff >= off + size);
    { buff; off }

  let to_bool t =
    match Bytes.get t.buff t.off with
    | '\000' -> false
    | '\255' -> true
    | c -> failwith (c |> Char.code |> Fmt.str "Unknown bool encoding with char code %i")

  let set t b = Bytes.set t.buff t.off (if b then '\255' else '\000')

  let pp ppf t = t |> to_bool |> Fmt.bool ppf

  let pp_raw ppf t =
    Bytes.sub_string t.buff t.off size |> Hex.of_string |> Hex.show |> Fmt.pf ppf "%s"
end

type kind = Field.kind [@@deriving repr]

module type Common = sig
  module Version : IntHeaderField

  module Magic : StringHeaderField

  module Pointer : IntHeaderField

  module Address : IntHeaderField

  module Flag : BoolHeaderField

  module Kind : sig
    include HeaderField

    val to_kind : t -> kind

    val set : t -> kind -> unit

    val kind_to_depth : kind -> int
  end
end

module MakeCommon : functor (Params : Params.S) -> Common =
functor
  (Params : Params.S)
  ->
  struct
    module Version = MakeInt (struct
      let size = Params.version_sz
    end)

    module Magic = MakeString (struct
      let size = Params.magic_sz
    end)

    module Address = MakeInt (struct
      let size = Params.page_address_sz
    end)

    module Pointer = MakeInt (struct
      let minimal_size =
        10 * Params.page_sz |> Printf.sprintf "%x" |> String.length |> fun x -> (x + 1) / 2

      let size = [ 1; 2; 4; 8 ] |> List.filter (( <= ) minimal_size) |> List.hd

      (* enough bytes to have pointers to 10 overflow pages away *)
    end)

    module Flag = Flag

    module Kind = struct
      type t = { off : int; buff : bytes } [@@deriving repr]

      let size = Params.tree_height_sz

      let get, set =
        match size with
        | 1 -> Bytes.(get_uint8, set_uint8)
        | 2 -> Bytes.(get_uint16_be, set_uint16_be)
        | 4 ->
            Bytes.
              ( (fun buff i -> get_int32_be buff i |> Int32.to_int),
                fun buff i n -> n |> Int32.of_int |> set_int32_be buff i )
        | 8 ->
            Bytes.
              ( (fun buff i -> get_int64_be buff i |> Int64.to_int),
                fun buff i n -> n |> Int64.of_int |> set_int64_be buff i )
        | n -> failwith (Fmt.str "Unsupported int length %i" n)

      let v buff ~off =
        assert (Bytes.length buff >= off + size);
        { buff; off }

      let kind_to_depth (kind : kind) = match kind with Leaf -> 0 | Node n -> n | _ -> -1

      let kind_of_int i : kind =
        if i = 0 then Overflow_leaf
        else if i = 1 then Overflow_node
        else if i = 2 then Leaf
        else Node (i - 2)

      let int_of_kind (kind : kind) =
        match kind with Overflow_leaf -> 0 | Overflow_node -> 1 | Leaf -> 2 | Node n -> 2 + n

      let to_kind t = get t.buff t.off |> kind_of_int

      let set t kind = kind |> int_of_kind |> set t.buff t.off

      let pp ppf t = t |> to_kind |> (Repr.pp kind_t) ppf

      let pp_raw ppf t =
        Bytes.sub_string t.buff t.off size |> Hex.of_string |> Hex.show |> Fmt.pf ppf "%s"
    end
  end
