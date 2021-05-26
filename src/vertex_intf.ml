module type S = sig
  type t
  (** The type of vertex mapping type [key] to type [value] *)

  type key

  type value

  type address

  type store

  val create : store -> Field.kind -> address -> t
  (** [create s p k1 k2] creates a new empty table, stored at address [p] in [s], with initial size
      [Params.fanout] *)

  val load : store -> address -> t
  (** [load s p] loads the table stored at address [p] in [s]. *)

  val migrate : string list -> Field.kind -> string
  (** [migrate kvs kind] is the representation of the key-value association list [kvs] in a vertex
      of type [kind] *)

  val clear : t -> (key -> bool) -> unit
  (** [clear t predicate] clears every binding from [k] to [v] in [t] that satisfies [predicate k] *)

  val shrink : t -> unit
  (** [shrink t] launches a garbage collection process that shrinks the size of [t] to a minimum. *)

  val split : t -> store -> address -> key * t
  (** [split t s p] moves every binding in [t] from [k] to [v] that satisfies [k >= pivot], where
      [pivot] is the middle key bounded in [t] for the natural key ordering, to a new table [t_mv]
      stored at address [p] in [s], and returns [pivot, t_mv]. *)

  val add : t -> key -> value -> unit
  (** [add t x y] adds a binding from [x] to [y] in [t]. Contrary to [Map.add], previous bindings
      from [x] are not hidden, but deleted. *)

  val find : t -> key -> value
  (** [find t k] returns the current binding of [k] in [t], or raises [Not_found] if no such binding
      exists. *)

  val mem : t -> key -> bool
  (** [mem t k] checks if [k] is bound in [t]. *)

  val iter : t -> (key -> value -> unit) -> unit
  (** [iter t func] applies [func key value] on every bindings [(key, value)] stored in [t] *)

  val fold_left : ('a -> key * value -> 'a) -> 'a -> t -> 'a

  val remove : t -> key -> unit
  (** [remove t k] removes the binding of [k] in [t], or raises [Not_found] if no such binding
      exists. *)

  val length : t -> int
  (** [length t] is the number of keys bound in [t]. It takes constant time. *)

  val depth : t -> int
  (** [depth t] is the depth of the vertex [t] in the btree it is part of *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] outputs a human-readable representation of [t] to the formatter [ppf] *)
end

module type BOUND = sig
  (* what is bound in the vertex *)
  type t

  val encode : t -> Encoder.t

  val decode : Encoder.t -> t

  val size : int

  val pp : Format.formatter -> t -> unit

  val kind : [ `Leaf | `Node ]
end

module type LEAFMAKER = functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : Data.V)
  ->
  S
    with type key := Key.t
     and type value := Value.t
     and type store = Store.t
     and type address = Store.address

module type NODEMAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) ->
  S
    with type key := Key.t
     and type value := Field.MakeCommon(Params).Address.t
     and type store = Store.t
     and type address = Store.address

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) (Bound : BOUND) ->
  S
    with type key := Key.t
     and type value := Bound.t
     and type store = Store.t
     and type address = Store.address

module type Vertex = sig
  module LeafMake : LEAFMAKER

  module NodeMake : NODEMAKER
end
