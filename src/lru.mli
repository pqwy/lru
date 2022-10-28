(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

(** Scalable LRU caches

    [Lru] provides weight-bounded finite maps that can remove the
    least-recently-used (LRU) bindings in order to maintain a weight constraint.
    Two implementations are provided: one is {{!F}functional}, the other
    {{!M}imperative}.

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

(** {1:sem Semantics}

    A pretty accurate model of a {{!F.S}functional} [k -> v] map is an
    association list ([(k * v) list]) with unique keys.

    {{!F.S.add}Adding} a bindings [k -> v] to [kvs] means
    [List.remove_assoc k kvs @ [(k, v)]], {{!F.S.find}finding} a [k] means
    [List.assoc_opt k kvs], and removing it means [List.remove_assoc k kvs].

    The {{!F.S.lru}LRU binding} is then the first element of the list.

    {{!F.S.promote}Promoting} a binding [k -> v] means removing, and then
    re-adding it.

    {{!F.S.trim}Trimming} [kvs] means retaining the longest suffix with the sum
    of [weight v] not larger than {{!F.S.capacity}capacity}.

    The {{!M.S}imperative} LRU map is like the above, but kept in a reference
    cell. *)

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
    (** [capacity t] is the maximum combined weight of bindings that {!trim}
        will retain. *)

    val resize : int -> t -> t
    (** [resize cap t] sets [t]'s capacity to [cap], while leaving the bindings
        unchanged.

        @raise Invalid_argument when [cap < 0]. *)

    val trim : t -> t
    (** [trim t] is [t'], where [weight t' <= capacity t'].

        This is achieved by discarding bindings in LRU-to-MRU order. *)

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
    (** [remove k t] is [t] without a binding for [k]. *)

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
    (** [fold f z t] is [f k0 v0 (... (f kn vn z))], where [k0 -> v0] is LRU and
        [kn -> vn] is MRU. *)

    val fold_k : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    (** [fold_k f z t] folds in key-increasing order, ignoring the recently-used
        ordering.

        {b Note} [fold_k] is faster than [fold]. *)

    val iter : (k -> v -> unit) -> t -> unit
    (** [iter f t] applies [f] to all the bindings in [t] in in LRU-to-MRU
        order. *)

    val iter_k : (k -> v -> unit) -> t -> unit
    (** [iter_k f t] applies f in key-increasing order, ignoring the
        recently-used ordering.

        {b Note} [iter_k] is faster than [iter]. *)

    (** {1 Conversions} *)

    val of_list : (k * v) list -> t
    (** [of_list kvs] is a map with bindings [kvs], where the order of the list
        becomes LRU-to-MRU ordering, and its {{!capacity}[capacity]} is set to
        its {{!weight}[weight]}.

        The resulting [t] has the same shape as if the bindings were
        sequentially {{!add}added} in list order, except for capacity. *)

    val to_list : t -> (k * v) list
    (** [to_list t] are the bindings in [t] in LRU-to-MRU order. *)

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
    val pp_dump : (formatter -> k -> unit) -> (formatter -> v -> unit)
                    -> formatter -> t -> unit
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

    (** {1 Mutable LRU map} *)

    type t
    (** A map. *)

    type k
    (** Keys in {{!t}[t]}. *)

    type v
    (** Values in {{!t}[t]}. *)

    val create : ?random:bool -> ?initialSize:int -> int -> t
    (** [create ?random ?initialSize cap] is a new map with capacity [cap].

        [~random] randomizes the underlying hash table. It defaults to [false].
        See {!Hashtbl.create}.

        [~initialSize] sets the initial size of the underlying hash table. If not set,
        [initialSize] is set to [cap].

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
    (** [capacity t] is the maximum combined weight of bindings that {!trim}
        will retain. *)

    val resize : int -> t -> unit
    (** [resize cap t] sets [t]'s capacity to [cap], while leaving the bindings
        unchanged.

        @raise Invalid_argument when [cap < 0]. *)

    val trim : t -> unit
    (** [trim t] ensures that [weight t <= capacity t] by dropping bindings in
        LRU-to-MRU order. *)

    (** {1 Access by [k]} *)

    val mem : k -> t -> bool
    (** [mem k t] is [true] iff [k] is bound in [t]. *)

    val find : k -> t -> v option
    (** [find k t] is [Some v] when [k -> v] is bound in [t], or [None]
        otherwise.

        {b Note} This operation does not change the recently-used order. *)

    val promote : k -> t -> unit
    (** [promote k t] promotes the binding for [k], if it exists, to
        most-recently-used. *)

    val add : k -> v -> t -> unit
    (** [add k v t] adds the binding [k -> v] to [t] as the most-recently-used
        binding.

        {b Note} [add] does not remove bindings. To ensure that the resulting
        map is not over capacity, combine with {{!trim}[trim]}. *)

    val remove : k -> t -> unit
    (** [remove k t] is [t] without a binding for [k]. *)

    (** {1 Access to least-recently-used bindings} *)

    val lru : t -> (k * v) option
    (** [lru t] is the least-recently-used binding in [t], or [None], when [t]
        is empty. *)

    val drop_lru : t -> unit
    (** [drop_lru t] removes the binding [lru t]. *)

    (** {1 Aggregate access} *)

    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    (** [fold f z t] is [f k0 v0 (... (f kn vn z))], where [k0 -> v0] is LRU and
        [kn -> vn] is MRU. *)

    val iter : (k -> v -> unit) -> t -> unit
    (** [iter f t] applies [f] to all the bindings in [t] in in LRU-to-MRU
        order. *)

    (** {1 Conversions} *)

    val of_list : (k * v) list -> t
    (** [of_list kvs] is a map with bindings [kvs], where the order of the list
        becomes LRU-to-MRU ordering, and its {{!capacity}[capacity]} is set to
        its {{!weight}[weight]}.

        The resulting [t] has the same shape as if the bindings were
        sequentially {{!add}added} in list order, except for capacity. *)

    val to_list : t -> (k * v) list
    (** [to_list t] are the bindings in [t] in LRU-to-MRU order. *)

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
    val pp_dump : (formatter -> k -> unit) -> (formatter -> v -> unit)
                    -> formatter -> t -> unit
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
