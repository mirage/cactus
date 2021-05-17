module type S = sig
  type t

  type key

  type store

  type address

  type kind

  val create : store -> kind -> t

  val load : store -> address -> t

  val depth : t -> int

  val self_address : t -> address

  val overflow : t -> bool

  val will_overflow : t -> bool

  val underflow : t -> bool

  val split : t -> key * t

  val find : t -> key -> address

  val add : t -> key -> address -> unit

  val delete : t -> key -> unit

  val iter : t -> (key -> address -> unit) -> unit

  val fold_left : ('a -> key -> address -> 'a) -> 'a -> t -> 'a

  val length : t -> int
  (** [length t] is the number of bindings in [t] *)

  val migrate : string list -> Field.kind -> string
  (** [migrate kvs depth] constructs the serialised representation of the node associated to [kvs],
      the list of string representations for key,value bindings. *)

  val pp : Format.formatter -> t -> unit
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) ->
  S
    with type key = Key.t
     and type address := Store.address
     and type store := Store.t
     and type kind := Field.kind

module type Node = sig
  module type S = S

  module Make : MAKER
end
