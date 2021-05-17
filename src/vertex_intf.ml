module type LEAFMAKER = functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : Data.V)
  -> sig
  include Tbl.S

  val create : store -> Field.kind -> address -> t

  val migrate : string list -> Field.kind -> string
end
with type key := Key.t
 and type value := Value.t
 and type store = Store.t
 and type address = Store.address

module type NODEMAKER = functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  -> sig
  include Tbl.S

  val create : store -> Field.kind -> address -> t

  val depth : t -> int

  val migrate : string list -> Field.kind -> string
end
with type key := Key.t
 and type value := Field.MakeCommon(Params).Address.t
 and type store = Store.t
 and type address = Store.address

module type MAKER = functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Bound : sig
     type t

     val encode : t -> Encoder.t

     val decode : Encoder.t -> t

     val size : int

     val pp : Format.formatter -> t -> unit
   end)
  (Kind : sig
     val v : [ `Leaf | `Node ]
   end)
  -> sig
  include Tbl.S

  val create : store -> Field.kind -> address -> t

  val depth : t -> int

  val migrate : string list -> Field.kind -> string
end
with type key := Key.t
 and type value := Bound.t
 and type store = Store.t
 and type address = Store.address

module type Vertex = sig
  module Make : MAKER

  module LeafMake : LEAFMAKER

  module NodeMake : NODEMAKER
end
