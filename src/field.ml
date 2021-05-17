module type _COMMON = sig
  type t [@@deriving repr]

  val encode : t -> Encoder.t

  val decode : Encoder.t -> t

  val size : int

  val pp : Format.formatter -> t -> unit
end

module type STRING = sig
  include _COMMON

  val decode : Encoder.t -> size:int -> t

  val dump : t -> Encoder.t (* like encode but without padding *)
end

module type INT = sig
  include _COMMON

  val to_int : t -> int

  val of_int : int -> t
end

module type BOOL = sig
  include _COMMON

  val to_bool : t -> bool

  val of_bool : bool -> t
end

module type BOOLS = sig
  include _COMMON

  val to_bools : t -> bool list

  val of_bools : bool list -> t
end

module type SIZE = sig
  val size : int
end

module MakeString (Size : SIZE) : STRING = struct
  type t = string [@@deriving repr]

  let size = Size.size

  let encode s = Encoder.encode_string ~pad:size s

  let decode e ~size =
    let s = Encoder.dump e in
    String.sub s (String.length s - size) size

  let dump s = Encoder.load s

  let pp = Fmt.string
end

module MakeInt (Size : SIZE) : INT = struct
  type t = int [@@deriving repr]

  let size = Size.size

  let encode = Encoder.encode_int ~pad:size

  let decode = Encoder.decode_int

  let to_int i = i

  let of_int i = i

  let pp = Fmt.int
end

module MakeBool (Size : SIZE) : BOOL = struct
  type t = bool [@@deriving repr]

  let size = Size.size

  let encode = Encoder.encode_bool ~pad:size

  let decode = Encoder.decode_bool

  let to_bool b = b

  let of_bool b = b

  let pp = Fmt.bool
end

module MakeBools (N : sig
  val n : int
end) : BOOLS = struct
  type t = bool list [@@deriving repr]

  let size = ((N.n - 1) lsr 3) + 1

  let encode = Encoder.encode_bools N.n

  let decode = Encoder.decode_bools N.n

  let to_bools bs =
    assert (List.length bs = N.n);
    bs

  let of_bools bs =
    assert (List.length bs = N.n);
    bs

  let pp = Fmt.list Fmt.bool
end

type kind = Leaf | Overflow_leaf | Overflow_node | Node of int [@@deriving repr]

module type COMMON = sig
  module Version : INT

  module Magic : STRING

  module Address : INT

  module Flag : BOOL

  module Kind : sig
    include INT with type t = kind

    val of_depth : int -> t (* kind of a node from its distance to the leaves *)

    val to_depth : t -> int (* distance to the leaves of a node, from its kind *)
  end
end

module MakeCommon (Params : Params.S) : COMMON = struct
  module Version = MakeInt (struct
    let size = Params.version_sz
  end)

  module Magic = MakeString (struct
    let size = Params.magic_sz
  end)

  module Address = MakeInt (struct
    let size = Params.page_address_sz
  end)

  module Flag = MakeBool (struct
    let size = Params.flag
  end)

  module Kind = struct
    type t = kind [@@deriving repr]

    let size = Params.tree_height_sz

    let of_int i =
      if i = 0 then Overflow_leaf
      else if i = 1 then Overflow_node
      else if i = 2 then Leaf
      else Node (i - 2)

    let to_int kind =
      match kind with Overflow_leaf -> 0 | Overflow_node -> 1 | Leaf -> 2 | Node n -> 2 + n

    let of_depth i = if i = 0 then Leaf else Node i

    let to_depth kind = match kind with Leaf -> 0 | Node n -> n | _ -> -1

    let decode s = s |> Encoder.decode_int |> of_int

    let encode kind = kind |> to_int |> Encoder.encode_int ~pad:size

    let pp = Repr.pp t
  end
end
