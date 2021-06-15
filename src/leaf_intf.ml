module type S = sig
  type t

  type key

  type value

  type store

  type address

  val init : store -> address -> t

  val create : store -> t (* like init but allocates a new page *)

  val load : store -> address -> t

  val self_address : t -> address

  val overflow : t -> bool

  val will_overflow : t -> bool

  val underflow : t -> bool

  val split : t -> key * t
  (** [split leaf] allocates a new leaf to split to, splits [leaf], promotes the middle key, and
      returns [promoted, allocated] *)

  val merge : t -> t -> [ `Partial | `Total ]

  val find : t -> key -> value

  val leftmost : t -> key

  val mem : t -> key -> bool

  val add : t -> key -> value -> unit

  val remove : t -> key -> unit

  val iter : t -> (key -> value -> unit) -> unit

  val length : t -> int
  (** [length t] is the number of bindings in [t] *)

  val migrate : string list -> string

  val pp : t Fmt.t
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) (Value : Data.V) ->
  S
    with type value = Value.t
     and type key = Key.t
     and type store = Store.t
     and type address = Store.address

module type Leaf = sig
  module type S = S

  module Make : MAKER
end
