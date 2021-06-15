type config = {
  key_sz : int;
  value_sz : int;
  fanout : int;
  cache_sz : int;
  page_sz : int;
  start_sz : int;
  n : int;
  debug : bool;
  version : int;
  sleep : bool;
}

module type SIZE = sig
  include Btree.Private.Input.Size

  val key_sz : int

  val value_sz : int
end

module type KEY = Btree.Private.Input.Key

module type VALUE = Btree.Private.Input.Value

module type MAKER = functor (Size : SIZE) -> sig
  module Key : KEY with type t = string

  module Value : VALUE

  val generate_key : int -> Key.t

  val generate_value : int -> Value.t

  val batch_initialiser : int -> int -> string
end

module type Input = sig
  module type SIZE = SIZE

  module Make : MAKER

  type config = {
    key_sz : int;
    value_sz : int;
    fanout : int;
    cache_sz : int;
    page_sz : int;
    start_sz : int;
    n : int;
    debug : bool;
    version : int;
    sleep : bool;
  }
end
