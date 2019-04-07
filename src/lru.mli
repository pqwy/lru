(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

(** Scalable LRU caches

    [Lru] provides size-bounded finite maps that remove the least-recently-used
    (LRU) bindings in order to maintain the size constraint. Two implementations
    are provided: one is {{!F}functional}, the other {{!M}imperative}.

    The {{!F}functional} map is backed by a
    {{:https://github.com/pqwy/psq}priority search queue}. Operations on
    individual elements are [O(log n)].

    The {{!M}mutable} map is backed by the standard {!Hashtbl} paired with a
    doubly-linked list. Operations on individual elements incur an [O(1)]
    overhead on top of hash table access.

    Both versions support {{!Weighted}differentially weighted} bindings, and
    have a capacity parameter that limits the combined weight of the bindings.
    To limit the maps by the number of bindings, use [let weight _ = 1].

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Lru} *)

(** Signature of types with measurable weight. *)
module type Weighted = sig
  type t
  val weight : t -> int
  (** [weight t] is a measure of [t]s contribution towards the total map
      capacity. Weight must be strictly positive. *)
end

(** Functional LRU map. *)
module F : sig

  (** Signature of functional LRU maps. *)
  module type S = sig

    (** {1 Functional LRU map} *)

    type t
    (** A map. *)

    type k
    (** Keys in {{!t}[t]}. *)

    type v
    (** Values in {{!t}[t]}. *)

    val empty : int -> t
    (** [empty cap] is an empty map with capacity [cap].

        @raise Invalid_argument when [cap < 0]. *)

    val is_empty : t -> bool
    (** [is_empty t] is [true] iff there are no bindings in [t]. *)

    val size : t -> int
    (** [size t] is the number of bindings in [t]. *)

    (** {1 Limiting the weight of bindings} *)

    val weight : t -> int
    (** [weight t] is the combined weight of bindings in [t]. *)

    val capacity : t -> int
    (** [capacity t] is the maximum combined weight of bindings this map will
        hold before they start being discarded in least-recently-used order. *)

    val resize : int -> t -> t
    (** [resize cap t] sets [t]'s capacity to [cap], while leaving the bindings
        unchanged.

        @raise Invalid_argument when [cap < 0]. *)

    val trim : t -> t
    (** [trim t] is [t'], the map that contains as many most-recently-used
        bindings in [t] as its capacity permits (that is,
        [weight t' <= capacity t']).

        When [t] is over capacity, its bindings are discarded in
        least-recently-used order. Otherwise, [t == t']. *)

    (** {1 Access by [k]} *)

    val mem : k -> t -> bool
    (** [mem k t] is [true] iff [k] is bound in [t]. *)

    val find : k -> t -> v option
    (** [find k t] is [Some v] when [k -> v] is bound in [t], or [None]
        otherwise. *)

    val promote : k -> t -> t
    (** [promote k t] is [t] with the binding for [k] promoted to
        most-recently-used, or [t] if [k] is not bound in [t]. *)

    val add : k -> v -> t -> t
    (** [add k v t] adds the binding [k -> v] to [t] as the most-recently-used
        binding.

        {b Note} [add] does not remove bindings. To ensure that the resulting
        map is not over capacity, compose with {{!trim}[trim]}. *)

    val remove : k -> t -> t
    (** [remove k t] is [t] without the binding for [k], or [t] if [k] is not
        bound in [t]. *)

    val pop : k -> t -> (v * t) option
    (** [pop k t] is [(v, t')], where [v] is the value bound to [k], and [t']
        is [t] without the binding [k -> t], or [None] if [k] is not bound in
        [t]. *)

    (** {1 Access to least-recently-used bindings} *)

    val lru : t -> (k * v) option
    (** [lru t] is the least-recently-used binding in [t], or [None], when [t]
        is empty. *)

    val drop_lru : t -> t
    (** [drop_lru t] is [t] without the binding [lru t], or [t], when [t] is
        empty. *)

    val pop_lru : t -> ((k * v) * t) option
    (** [pop_lru t] is [((k, v), t'], where [(k, v)] is [lru t], and [t'] is [t]
        without that binding. *)

    (** {1 Aggregate access} *)

    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    (** [fold f z t] is [f k0 v0 (... (f kn vn z))]. Binding are folded over in
        key-increasing order. *)

    val iter : (k -> v -> unit) -> t -> unit
    (** [iter f t] applies [f] to all the bindings in [t] in key-increasing order *)

    (** {1 Conversions} *)

    val to_list : t -> (k * v) list
    (** [to_list t] are the bindings in [t] in key-increasing order. *)

    val of_list : (k * v) list -> t
    (** [of_list kvs] is a map with bindings from [kvs]. Its weight and capacity
        are the total weight of its bindings.

        With respect to duplicates and the recently-used order, it behaves as if
        the bindings were added sequentially in the list order.

{[w = weight (of_list kvs)

of_list kvs = List.fold_left (fun m (k, v) -> add k v m) (empty w) kvs]} *)

    open Format

    (** {1 Pretty-printing} *)

    val pp : ?pp_size:(formatter -> (int * int) -> unit) ->
             ?sep:(formatter -> unit -> unit) ->
             (formatter -> k * v -> unit) -> formatter -> t -> unit
    (** [pp ~pp_size ~sep pp_kv ppf t] pretty-prints [t] to [ppf], using [pp_kv]
        to print the bindings, [~sep] to separate them, and [~pp_size] to print
        the {{!weight}[weight]} and {{!capacity}[capacity]}. [~sep] and
        [~pp_size] default to unspecified printers. *)

    (**/**)
    val pp_dump : (formatter -> k * v -> unit) -> formatter -> t -> unit
    (**/**)
  end

  (** [Make(K)(V)] is the {{!S}LRU map} with bindings [K.t -> V.t]. The weight
      of an individual binding is the {!Weighted.weight} of [V.t]. *)
  module Make (K: Map.OrderedType) (V: Weighted):
    S with type k = K.t and type v = V.t
end


(** Mutable LRU map. *)
module M : sig

  (** Signature of mutable LRU maps. *)
  module type S = sig

    type t
    (** A map. *)

    type k
    (** Keys in {{!t}[t]}. *)

    type v
    (** Values in {{!t}[t]}. *)

    val create : ?random:bool -> int -> t
    (** [create ?random cap] is a new map with capacity [cap].

        [~random] randomizes the underlying hash table. It defaults to [false].
        See {!Hashtbl.create}.

        {b Note.} The internal hash table is created with size [cap].

        @raise Invalid_argument when [cap < 0]. *)

    val is_empty : t -> bool
    (** [is_empty t] is [true] iff there are no bindings in [t]. *)

    val size : t -> int
    (** [size t] is the number of bindings in [t]. *)

    (** {1 Limiting the weight of bindings} *)

    val weight : t -> int
    (** [weight t] is the combined weight of bindings in [t]. *)

    val capacity : t -> int
    (** [capacity t] is the maximum combined weight of bindings this map will
        hold before they start being discarded in least-recently-used order. *)

    val resize : int -> t -> unit
    (** [resize cap t] sets [t]'s capacity to [cap], while leaving the bindings
        unchanged.

        @raise Invalid_argument when [cap < 0]. *)

    val trim : t -> unit
    (** [trim t] drops the bindings in [t] until they fit its capacity
        (that is, until [weight t <= capacity t]).

        Binding are discards in least-recently-used order. *)

    (** {1 Access by [k]} *)

    val mem : k -> t -> bool
    (** [mem k t] is [true] iff [k] is bound in [t]. *)

    val find : k -> t -> v option
    (** [find k t] is [Some v] when [k -> v] is bound in [t], or [None]
        otherwise.

        {b Note} This operation does not change the recently-used order. *)

    val promote : k -> t -> unit
    (** [promote k t] sets the binding for [k], if it exists, to be the
        most-recently-used. *)

    val add : k -> v -> t -> unit
    (** [add k v t] adds the binding [k -> v] to [t] as the most-recently-used
        binding.

        {b Note} [add] does not remove bindings. To ensure that the resulting
        map is not over capacity, invoke {{!trim}[trim]}. *)

    val remove : k -> t -> unit
    (** [remove k m] removes the binding for [k] if it exists. *)

    (** {1 Access to least-recently-used bindings} *)

    val lru : t -> (k * v) option
    (** [lru t] is the least-recently-used binding in [t], or [None], when [t]
        is empty. *)

    val drop_lru : t -> unit
    (** [drop_lru t] removes the binding [lru t]. *)

    (** {1 Traversal direction} *)

    type dir = [ `Up | `Down ]
    (** Traversal direction for operations that visit all bindings.
        {ul
        {- [`Up] means least-to-most recently used, or increasing relevance.}
        {- [`Down] means most-to-least recently used, or decreasing relevance.}} *)

    (** {1 Aggregate access} *)

    val fold : ?dir:dir -> (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    (** [fold f z t] is [f k0 v0 (... (f kn vn z))].

        [~dir] controls the order of folding. Defaults to [`Up]. *)

    val iter : ?dir:dir -> (k -> v -> unit) -> t -> unit
    (** [iter f t] applies [f] to all the bindings in [t].

        [~dir] controls the order of application. Defaults to [`Up]. *)

    (** {1 Conversions} *)

    val to_list : ?dir:dir -> t -> (k * v) list
    (** [to_list t] are the bindings in [t]. [~dir] controls the order, defaults
        to [`Up]. *)

    val of_list : (k * v) list -> t
    (** [of_list kvs] is a map with the bindings from [kvs]. Its weight and
        capacity are the total weight of its bindings.

        With respect to duplicates and the recently-used order, it behaves as if
        the bindings were added sequentially in the list order. *)

    open Format

    (** {1 Pretty-printing} *)

    val pp : ?pp_size:(formatter -> int * int -> unit) ->
             ?sep:(formatter -> unit -> unit) ->
             (formatter -> k * v -> unit) -> formatter -> t -> unit
    (** [pp ~pp_size ~sep pp_kv ppf t] pretty-prints [t] to [ppf], using [pp_kv]
        to print the bindings, [~sep] to separate them, and [~pp_size] to print
        the {{!weight}[weight]} and {{!capacity}[capacity]}. [~sep] and
        [~pp_size] default to unspecified printers. *)

    (**/**)
    val pp_dump : (formatter -> k * v -> unit) -> formatter -> t -> unit
    (**/**)
  end

  (** [Make(K)(V)] is the {{!S}LRU map} with bindings [K.t -> V.t]. The weight
      of an individual binding is the {!Weighted.weight} of [V.t]. *)
  module Make (K: Hashtbl.HashedType) (V: Weighted):
    S with type k = K.t and type v = V.t

  (** [MakeSeeded(K)(V)] is a variant backed by {!Hashtbl.SeededS}. *)
  module MakeSeeded (K: Hashtbl.SeededHashedType) (V: Weighted):
    S with type k = K.t and type v = V.t

end

(** {1 One-off memoization} *)

val memo : ?hashed:(('a -> int) * ('a -> 'a -> bool)) -> ?weight:('b -> int) ->
           cap:int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
(** [memo ?hashed ?weight ~cap f] is a new memoized instance of [f], using LRU
    caching. [f] is an open recursive function of one parameter.

    [~hashed] are hashing and equality over the arguments ['a]. It defaults to
    [(Hashtbl.hash, Pervasives.(=))].

    [~weight] is the weighting function over the results ['b]. It defaults to
    [fun _ -> 1].

    [~cap] is the total cache capacity.

    @raise Invalid_argument when [cap < 0]. *)
