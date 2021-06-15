module type S = sig
  module Btree : Btree.S

  type t = {
    name : string;
    synopsis : string;
    exec : Btree.t -> unit -> unit;
    dependency : string option;
    kind : [ `RW | `R ];
    speed : [ `Quick | `Slow ];
  }

  type suite = t list

  val name : t -> string

  val suite : suite

  val split : suite -> suite list
  (* splits a suite into its connected dependency subgraphs, each sorted in topological order *)

  val run : t -> string -> Btree.cache -> Results.t

  val minimal_filter : t -> bool
end

module type CONFIG = sig
  val config : Input.config
end

module type MAKER = functor (Config : CONFIG) -> S

module type Benchmark = sig
  module type S = S

  module Make : MAKER
end
