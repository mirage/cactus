module type CALIFORNIA = sig
  (* You can (almost) never leave the california cache *)

  type key

  type value

  type t

  val v :
    flush:(key -> value -> unit) ->
    load:(?available:value -> key -> value) ->
    filter:(value -> [ `California | `Lru | `Volatile ]) ->
    int ->
    t
  (* The constructor for caches.
     @param flush The function to be called before any binding is discarded from the cache.
     @param load The (possibly costly) function to call to fetch a new binding into the cache. An optional argument [~available] can be passed to recycle memory allocations from another, never used again, value.
     @param filter A hierarchy function between bindings.
       [`California] is for most frequent bindings which should never be discarded.
       [`Lru] is for remaining frequent bindings that are too numerous to fit in memory.
       [`Volatile] is for the least frequent bindings, discarded soon after loading.

     [v ~flush ~load ~filter lru_cap] is a 3 level cache, with a bound of [lru_cap] bindings on the middle level.
  *)

  val find : t -> key -> value

  val reload : t -> key -> unit

  val update_filter : t -> filter:(value -> [ `California | `Lru | `Volatile ]) -> unit

  val release : t -> unit

  val deallocate : t -> key -> unit

  val clear : t -> unit

  val flush : t -> unit
end

module type MAKER = functor (K : Hashtbl.HashedType) (V : Lru.Weighted) ->
  CALIFORNIA with type key = K.t and type value = V.t

module type Cache = sig
  module Make : MAKER
end
