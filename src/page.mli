module type S = sig
  type t

  module Common : Header.Common

  type pointer = int (* offset inside a page*)

  module Header : sig
    type h = { magic : Common.Magic.t; kind : Common.Kind.t; version : Common.Version.t }

    val load : t -> h
  end

  val write : t -> ?with_flush:bool -> offset:pointer -> Encoder.t -> unit

  val read : t -> offset:pointer -> length:int -> Encoder.t

  val init : t -> unit

  val flush : t -> unit
end
