module Private = struct
  module Utils = Utils
  module Stats = Stats
  module Index_stats = Index_stats
  module Tag = Log.Tag
  module Default = Input.Default
  module Input = Input
  module Data = Data
  module Syscalls = Syscalls
end

module type S = sig
  type key

  type value

  type t

  type cache

  val empty_cache : unit -> cache

  val create : ?cache:cache -> ?record:string -> string -> t
  (* [create root] creates a btree storage in directory [root] *)

  val replay : string -> t -> unit
  (* [replay path] replays the operations stored in file [path] *)

  val init : root:string -> int -> read:(int -> string) -> t
  (** [init ~root n ~read] performs a batch initialisation. [read] is an iterator-like function :
      [read n] reads the next [n] bindings and returns them in a single string chunk which is the
      concatenation of each [key ^ value]. [init] is (much) faster than adding each bindings one by
      one. *)

  val add : t -> key -> value -> unit

  val remove : t -> key -> unit

  val find : t -> key -> value

  val mem : t -> key -> bool

  val clear : t -> unit

  val close : t -> unit

  val flush : t -> unit

  val iter : (key -> value -> unit) -> t -> unit

  val iteri : (int -> key -> value -> unit) -> t -> unit

  val length : t -> int
  (** [length t] is the number of bindings in [t] *)

  val pp : t Fmt.t

  val snapshot : ?depth:int -> t -> unit

  module Private : sig
    module Params : Params.S

    module Common : Field.COMMON

    module Entry : Data.Entry with type input_key = key and type input_value = value

    module Key = Entry.Key
    module Value = Entry.Value

    module Store : Store.S with module Common = Field.MakeCommon(Params)

    module Page = Store.Page

    module Leaf :
      Leaf.S
        with type value = Value.t
         and type key = Key.t
         and type store := Store.t
         and type address := Store.address

    module Node :
      Node.S
        with type key = Key.t
         and type address := Store.address
         and type store := Store.t
         and type kind := Field.kind

    val dir : t -> string

    val store : t -> Store.t

    val root : t -> int

    val go_to_leaf : t -> key -> int list

    val cache_size : t -> int

    val pp : t -> int Fmt.t
  end
end

module type MAKER = functor (InKey : Input.Key) (InValue : Input.Value) (Size : Input.Size) ->
  S with type key = InKey.t and type value = InValue.t

module Input = Input

module type Btree = sig
  module Private = Private

  module type S = S

  module Input = Input

  module Make : MAKER
end
