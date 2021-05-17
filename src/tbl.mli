(** The file format specifications for leaves and nodes should expose a Map.S alike signature. In
    the case of leaves, it is a mapping from keys as [Encoder.t] to values as [Encoder.t]. In the
    case of nodes, it is a mapping from keys as [Encoder.t] to page address as [Encoder.t]. *)

module type S = sig
  type t
  (** The type of tables from type [Encoder.t] to type [Encoder.t] *)

  type key

  type value

  type address

  type store

  val create : store -> address -> t
  (** [Tbl.create s p k1 k2] creates a new empty table, stored at address [p] in [s], with initial
      size [Params.fanout] *)

  val load : store -> address -> t
  (** [Tbl.load s p] loads the table stored at address [p] in [s]. *)

  val clear : t -> (key -> bool) -> unit
  (** [Tbl.clear tbl predicate] clears every binding from [k] to [v] in [tbl] that satisfies
      [predicate k] *)

  val shrink : t -> unit
  (** [Tbl.shrink tbl] launches a garbage collection process that shrinks the size of [tbl] to a
      minimum. *)

  val split : t -> store -> address -> key * t
  (** [Tbl.split tbl s p] moves every binding in [tbl] from [k] to [v] that satisfies [k >= pivot],
      where [pivot] is the middle key bounded in [tbl] for the natural key ordering, to a new table
      [tbl_mv] stored at address [p] in [s], and returns [pivot, tbl_mv]. *)

  val add : t -> key -> value -> unit
  (** [Tbl.add tbl x y] adds a binding from [x] to [y] in [tbl]. Contrary to [Map.add], previous
      bindings from [x] are not hidden, but deleted. *)

  val find : t -> key -> value
  (** [Tbl.find tbl k] returns the current binding of [k] in [tbl], or raises [Not_found] if no such
      binding exists. *)

  val mem : t -> key -> bool
  (** [Tbl.mem tbl k] checks if [k] is bound in [tbl]. *)

  val iter : t -> (key -> value -> unit) -> unit
  (** [Tbl.iter tbl func] applies [func key value] on every bindings [(key, value)] stored in [tbl] *)

  val fold_left : ('a -> key * value -> 'a) -> 'a -> t -> 'a

  val remove : t -> key -> unit
  (** [Tbl.remove tbl k] removes the binding of [k] in [tbl], or raises [Not_found] if no such
      binding exists. *)

  val length : t -> int
  (** [Tbl.length tbl] returns the number of keys bound in [tbl]. It takes constant time. *)

  val pp : Format.formatter -> t -> unit
  (** [Tbl.pp ppf tbl] outputs a human-readable representation of [tbl] to the formatter [ppf] *)
end
