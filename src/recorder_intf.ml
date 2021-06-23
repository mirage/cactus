module type S = sig
  type t

  type key

  type value

  type op = Add of (key * value) | Find of (key * bool) | Mem of (key * bool) | Flush

  val v : string -> t (* [v path] creates a recorder to file [path] *)

  val record : t -> op -> unit

  val replay : string -> op Seq.t
  (* [replay path] is the sequence of operations recorded in file [path] *)
end

module type MAKER = functor (InKey : Input.Key) (InValue : Input.Value) ->
  S with type key = InKey.t and type value = InValue.t

module type Recorder = sig
  module Make : MAKER
end
