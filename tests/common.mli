module MyKey : sig
  type t = string [@@deriving repr]

  val encode : t -> string

  val decode : string -> t

  val encoded_size : int
end

val generate_key : unit -> MyKey.t

module MyValue : sig
  type t = int * int * int [@@deriving repr]

  val encode : t -> string

  val decode : string -> t

  val encoded_size : int
end

module Input : Btree.Input.Size

module type TREE = Btree.S with type key = MyKey.t and type value = MyValue.t

val get_tree : [ `V0 ] -> (module TREE)

val get_migrate_tree : [ `V0 ] -> (module TREE)

val set_report : string -> unit
(** [set_report root] set a reporter at root [root] *)

val v_to_s : [ `V0 ] -> string
