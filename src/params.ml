module type S = sig
  val fanout : int

  val version : int

  val tree_height_sz : int

  val page_sz : int

  val cache_sz : int

  val key_sz : int

  val value_sz : int

  val max_key : int

  val max_key_sz : int

  val version_sz : int

  val btree_magic : string

  val page_magic : string

  val magic_sz : int

  val key_repr_sz : int

  val page_address_sz : int

  val offset_sz : int

  val flag : int

  val debug : bool

  module Debug : sig
    val random_failure : bool
  end
end

module Constant = struct
  let version_sz = 1

  let btree_magic = "TREE"

  let page_magic = "PAGE"

  let tree_height_sz = 2

  let page_address_sz = 4

  let offset_sz = 4

  let flag = 1
end

module Make : functor (I : Input.Size) (Key : Input.Key) (Value : Input.Value) -> S =
functor
  (I : Input.Size)
  (Key : Input.Key)
  (Value : Input.Value)
  ->
  struct
    include I
    include Constant

    let value_sz = Value.encoded_size

    let key_sz = Key.encoded_size

    let max_key = (2 * fanout) + 1

    let max_key_sz = max_key |> Utils.b256size

    let magic_sz = String.(max (length btree_magic) (length page_magic))

    let key_repr_sz = key_sz |> Utils.b256size

    let () =
      if 1 lsl (4 * offset_sz) < page_sz then failwith "Pages are too large to be fully addressable"
  end
