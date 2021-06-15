module type S = sig
  type t
  (** the type for store handles *)

  type address = int (* page address *)

  type page

  module Common : Field.COMMON

  module Page : sig
    type t

    type pointer = int (* offset inside a page*)

    val kind : t -> Common.Kind.t

    val buff : t -> bytes

    val flush : t -> unit
  end
  with type t = page

  val init : root:string -> t

  val root : t -> address

  val reroot : t -> address -> unit
  (** [reroot s p] changes the root of [s] to the page of address [p] *)

  val load : t -> address -> page

  val reload : t -> address -> unit

  val release : t -> unit
  (** [release store] tells the store that no loaded pages is going to be written on. This allows
      the store to clean part of the cache, and must be called as frequently as possible. *)

  val release_ro : t -> unit
  (** [release_ro store] tells the store that no loaded pages is going to be written on, and that
      the loaded one have not been written on. This cleans the store cache without flushing.*)

  val allocate : t -> address

  val deallocate : t -> address -> unit

  val flush : t -> unit

  val clear : t -> unit

  val iter : t -> (address -> page -> unit) -> unit

  val pp_header : t Fmt.t

  module Private : sig
    val dir : t -> string

    val write : t -> string -> unit

    val init_migration : t -> unit

    val end_migration : t -> int -> address -> unit
  end
end

module type MAKER = functor (Params : Params.S) (Common : Field.COMMON) ->
  S with module Common = Common

module type Store = sig
  module type S = S

  module Make : MAKER
end
