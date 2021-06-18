module type Size = sig
  val fanout : int

  val version : int

  val page_sz : int

  val cache_sz : int

  val debug : bool
end

module type Key = sig
  type t [@@deriving repr]
  (** The type for keys. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number of bytes. *)

  val decode : string -> t
  (** [decode s] is the decoded form of the encoded value of string [s]. Must satisfy
      [decode (encode t) = t]. *)
end

module type Value = sig
  type t [@@deriving repr]
  (** The type for values. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number of bytes. *)

  val decode : string -> t
  (** [decode s off] is the decoded form of the encoded value at the offset [off] of string [s].
      Must satisfy [decode (encode t) 0 = t]. *)
end

module Default = struct
  module Size : Size = struct
    let fanout = 50

    let version = 0

    let page_sz = 4700

    let cache_sz = 2_000 (* 2 Gb of memory *)

    let debug = false
  end

  module Key : Key = struct
    type t = string [@@deriving repr]

    let encoded_size = 30

    let encode s =
      assert (String.length s = encoded_size);
      s

    let decode s =
      assert (String.length s = encoded_size);
      s
  end

  module Value : Value = struct
    type t = int [@@deriving repr]

    let encoded_size = 15

    let encode i =
      i |> Utils.to_b256 |> fun s -> String.make (encoded_size - String.length s) '\000'

    let decode s =
      assert (String.length s = encoded_size);
      s |> Utils.from_b256
  end
end
