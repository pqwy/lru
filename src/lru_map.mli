(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

(** LRU maps

    Associative containers that keep track of mapping access order, and support
    efficient discarding of the least-recently-used mappings.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Lru_map} *)

type 'a fmt = Format.formatter -> 'a -> unit

(** [F] provides an immutable LRU map.

    Average time complexity of single-element operations is [O(log n)].

    {b Note} The current implementation is backed by a simple non-balancing
    tree, and suffers from the [O(n)] pathological behavior these trees suffer
    from, if the mappings are inserted, {e or accessed}, in key-order. *)
module F : sig

  module type S = sig

    type +'v t

    type k

    val empty : 'v t
    (** [empty] is the [empty] map. *)

    val size : 'v t -> int
    (** [size m] is the [size] of [m]. *)

    val is_empty : 'v t -> bool
    (** [is_empty m] is [true] iff [m] is [empty]. *)

    val mem : k -> 'v t -> bool
    (** [mem k m] tests whether [k] is bound in [m]. *)

    val add : k -> 'v -> 'v t -> 'v t
    (** [add k v m] is the map [m] with the binding [k -> v]. A previous
        binding, if it exists, is overwritten.

        The binding [k -> v] is the most recently used in the resulting map. *)

    val find : k -> 'v t -> ('v * 'v t) option
    (** [find k m] is the pair [(v, m')], where [v] is the value associated to
        [k], and [m'] is a map with the same bindings in [m], or [None], if [k]
        is not bound in [m].

        The binding [k -> v] is the most recently used in [m']. *)

    val remove : k -> 'v t -> 'v t
    (** [remove k m] is [m] with the binding for [k] removed. *)

    val find_remove : k -> 'v t -> ('v * 'v t) option
    (** [find_remove k m] is a pair [(v, m')], where [v] is the binding for [k],
        and [m'] does not contain a binding for [k], or [None], if [k] is not
        bound in [m]. *)

    val lru : 'v t -> (k * 'v) option
    (** [lru m] is the least recently used binding, or [None], is the map is
        empty. *)

    val drop : 'v t -> 'v t
    (** [drop m] is [m] without the binding [lru m]. If [m] is empty, so is
        [drop m]. *)

    val trim : int -> 'v t -> 'v t
    (** [trim n m] is a map with no more than [n] bindings, created from [m] by
        discarding them in the LRU order. *)

    val fold : (k -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
    (** [fold f m s] is the catamorphism.

        The structure of applications of [f] follows the key-order. *)

    val iter : (k -> 'v -> unit) -> 'v t -> unit
    (** [iter f m] visits all bindings in [m] with [f].

        The bindings are visited in key-order. *)

    val map : (k -> 'v -> 'w) -> 'v t -> 'w t
    (** [map f m] [map]s [f] over [m]. *)

    val filter : (k -> 'v -> bool) -> 'v t -> 'v t
    (** [filter f m] discards the bindings when [f k v] is [false]. *)

    val fmap : (k -> 'v -> 'w option) -> 'v t -> 'w t
    (** [fmap f m] combines {{!filter}[filter]} and {{!map}[map]}; if [f k v]
        evaluates to [None], the binding is discarded. *)

    val pp : (k * 'v) fmt -> 'v t fmt
    (** [pp pp_kv ppf m] pretty-prints [m] to [ppf], using [pp_kv] to print
        bindings. *)
  end

  (** [Make (T)] is a {{!S}map} specialized to [T]. *)
  module Make (K : Map.OrderedType) : S with type k = K.t
end

(** [M] provides a mutable LRU map.

    The implementation is backed by a {{!Hashtbl}[Hashtbl]} and equivalent
    operations have same the same complexity. Insertion, querying and removal
    incur [O(1)] time and space for book-keeping. *)
module M : sig

  module type S = sig

    type 'v t

    type k

    val create : ?random:bool -> int -> 'v t
    (** [create ?random n] is a new [t] with the initial capacity [n].

        [~random] randomizes the underlying hash table. See {!Hashtbl.create}. *)

    val size : 'v t -> int
    (** [size m] is the [size] of [m]. *)

    val mem : k -> 'v t -> bool
    (** [mem k m] tests whether [k] is bound in [m]. *)

    val add : k -> 'v -> 'v t -> unit
    (** [add k v m] adds the binding [k -> v]. A previous binding, if it exists,
        is overwritten.

        Adding a binding {e uses} it for the purposes of LRU order.  *)

    val find : k -> 'v t -> 'v option
    (** [find k m] is the value associated to [k] in [m], or [None], if there is
        no such value.

        Finding a binding {e uses} it for the purposes of LRU order. *)

    val remove : k -> 'v t -> unit
    (** [remove k m] removes the current binding for [k], if there is one. *)

    val lru : 'v t -> (k * 'v) option
    (** [lru m] is the least recently used binding, or [None], is the map is
        empty. *)

    val drop : 'v t -> unit
    (** [drop m] drops the binding that [lru m] returns. *)

    val trim : int -> 'v t -> unit
    (** [trim n m] ensures that [m] contains no more than [n] mappings. It
        discards the mappings in the LRU order. *)

    val fold : (k -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
    (** [fold f m s] is the catamorphism.

        The structure of applications of [f] follows the recently-used order. *)

    val iter : (k -> 'v -> unit) -> 'v t -> unit
    (** [iter f m] visits all bindings in [m] with [f].

        The bindings are visited in recently-used order. *)

    val pp : (k * 'v) fmt -> 'v t fmt
    (** [pp pp_kv ppf m] pretty-prints [m] to [ppf], using [pp_kv] to print
        bindings. *)
  end

  (** [Make (T)] is a {{!S}map} backed by {!Hashtbl.S}. *)
  module Make (K : Hashtbl.HashedType) : S with type k = K.t

  (** [MakeSeeded (T)] is a {{!S}map} backed by {!Hashtbl.SeededS}. *)
  module MakeSeeded (K : Hashtbl.SeededHashedType) : S with type k = K.t

end
