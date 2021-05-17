module type S = sig
  type t
  (** the type for store handles *)

  type address = int (* page address *)

  type page

  module Common : Field.COMMON

  module Page : sig
    type t

    type pointer = int (* offset inside a page*)

    module Header : sig
      type h = { magic : Common.Magic.t; kind : Common.Kind.t; version : Common.Version.t }

      val load : t -> h

      val pp_raw : Format.formatter -> t -> unit
    end

    val write : t -> ?with_flush:bool -> offset:pointer -> Encoder.t -> unit

    val read : t -> offset:pointer -> length:int -> Encoder.t

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

  val flush : t -> unit

  val clear : t -> unit

  val iter : t -> (address -> page -> unit) -> unit

  val pp_header : Format.formatter -> t -> unit

  module Private : sig
    val dir : t -> string

    val write : t -> string -> unit

    val init_migration : t -> unit

    val end_migration : t -> int -> address -> unit
  end
end

module type MAKER = functor (Params : Params.S) -> S with module Common = Field.MakeCommon(Params)

module type Store = sig
  module type S = S

  module Make : MAKER
end
