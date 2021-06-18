module type H = sig
  module Common : Field.COMMON (* module where live header fields common to all pages *)

  module Nentry : Field.INT

  module Ndeadentry : Field.INT

  type t

  val load : marker:(unit -> unit) -> bytes -> t

  val init : t -> Field.kind -> unit

  val size : int

  val pp : t Fmt.t

  val g_magic : t -> Common.Magic.t

  val s_magic : t -> Common.Magic.t -> unit

  val g_kind : t -> Common.Kind.t

  val s_kind : t -> Common.Kind.t -> unit

  val g_nentry : t -> Nentry.t

  val s_nentry : t -> Nentry.t -> unit

  val g_ndeadentry : t -> Ndeadentry.t

  val s_ndeadentry : t -> Ndeadentry.t -> unit
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Common : Field.COMMON) ->
  H with module Common := Common

module type Vertex_header = sig
  module Make : MAKER
end
