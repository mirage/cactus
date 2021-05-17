module Func : sig
  (** This module exposes functions to produce statistics on function calls *)

  type t

  val v : counter_names:string list -> t
  (** creates a handle for function call statistics. The optional argument can be used to create
      named additional internal counters to keep track, for instance, of the size of the inputs. Use
      [] to only use the default counter that keeps track of the number of function calls *)

  val increment : t -> string -> int -> unit
  (** [incr t name n] increments the named counter with name [name] of [t] by [n] units*)

  val tic : t -> unit
  (** Play the internal clock *)

  val tac : t -> unit
  (** [tac t n] pauses the internal clock of [t]. Must follow a tic. *)

  val get_count : ?name:string -> t -> int
  (** Number of tic-tacs if no [name] is provided, else the counter associated to [name] *)

  val get_span : t -> Mtime.span
  (** Total time taken by tic-tacs *)

  val pp : Format.formatter -> t -> unit
end

type stats

val reset : unit -> unit

val get : unit -> stats

val get_by_name : stats -> modul:string -> stat:string -> Func.t

val pp : Format.formatter -> stats -> unit

val pp_json : Format.formatter -> stats -> unit

type module_stats

module type Common = sig
  val name : string

  val reset : unit -> unit

  val setup_log : string list -> unit

  val get : unit -> module_stats

  val pp : Format.formatter -> module_stats -> unit
end

module Btree : sig
  include Common

  val stat_add : Func.t

  val stat_find : Func.t

  val stat_mem : Func.t
end

module Nodes : sig
  include Common

  val stat_create : Func.t

  val stat_load : Func.t

  val stat_split : Func.t

  val stat_shift : Func.t

  val stat_add : Func.t

  val stat_find : Func.t

  val stat_mem : Func.t
end

module Store : sig
  include Common

  val stat_allocate : Func.t

  val stat_load : Func.t

  val stat_gc : Func.t

  val stat_read : Func.t

  val stat_write : Func.t

  val stat_io_r : Func.t
  (** read syscalls *)

  val stat_io_w : Func.t
  (** write syscalls *)

  val stat_fsync : Func.t

  val stat_release : Func.t
end

module Utils : sig
  include Common

  val stat_binary_search : Func.t
end
