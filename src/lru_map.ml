(* Copyright (c) 2015-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

type 'a fmt = Format.formatter -> 'a -> unit

let pp iter ppkv ppf t =
  let first = ref true in
  Format.fprintf ppf "[@[%a@]]"
  (fun _ () -> t |> iter @@ fun k v ->
    if !first then
      (ppkv ppf (k, v); first := false)
    else Format.fprintf ppf ",@ %a" ppkv (k, v)) ()

module F = struct

  module type S = sig

    type +'v t
    type k

    val empty : 'v t
    val size : 'v t -> int
    val is_empty : 'v t -> bool
    val mem : k -> 'v t -> bool
    val add : k -> 'v -> 'v t -> 'v t
    val find : k -> 'v t -> ('v * 'v t) option
    val remove : k -> 'v t -> 'v t
    val find_remove : k -> 'v t -> ('v * 'v t) option
    val lru : 'v t -> (k * 'v) option
    val drop : 'v t -> 'v t
    val trim : int -> 'v t -> 'v t
    val fold : (k -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
    val iter : (k -> 'v -> unit) -> 'v t -> unit
    val map : (k -> 'v -> 'w) -> 'v t -> 'w t
    val filter : (k -> 'v -> bool) -> 'v t -> 'v t
    val fmap : (k -> 'v -> 'w option) -> 'v t -> 'w t
    val pp : (k * 'v) fmt -> 'v t fmt
  end

  module Make (K : Map.OrderedType) = struct

    type k = K.t

    type +'v tree =
      | Tip
      | Node of (k * 'v * int) * 'v tree * 'v tree

    type +'v t = { gen : int; size : int; tree : 'v tree }

    let cons ?(gen=min_int) size tree =
      { tree; size; gen = if size > 0 then gen else min_int }

    let empty = cons 0 Tip

    let is_empty = function { tree = Tip; _ } -> true | _ -> false

    let mem k t =
      let rec go k1 = function
        | Tip -> false
        | Node ((k, _, _), l, r) ->
            let cmp = K.compare k1 k in
            cmp = 0 || (cmp < 0 && go k1 l) || (cmp > 0 && go k1 r) in
      go k t.tree

    let rec merge t1 t2 = match (t1, t2) with
      | (_, Tip) -> t1
      | (Tip, _) -> t2
      | (Node ((_, _, g1 as kvg1), l1, r1),
        Node ((_, _, g2 as kvg2), l2, r2)) ->
          if g1 < g2 then
            Node (kvg1, l1, merge r1 t2)
          else Node (kvg2, merge t1 l2, r2)

    let find_remove_exn k t =
      let rec go k1 = function
        | Tip -> raise Not_found
        | Node ((k, v, _ as kvg), l, r) ->
            let cmp = K.compare k1 k in
            if cmp < 0 then
              let (x, l) = go k1 l in (x, Node (kvg, l, r))
            else if cmp > 0 then
              let (x, r) = go k1 r in (x, Node (kvg, l, r))
            else (v, merge l r) in
      let (x, tree) = go k t.tree in
      (x, cons ~gen:t.gen (t.size - 1) tree)

    let find_remove k t = try Some (find_remove_exn k t) with Not_found -> None

    let remove_exn k t =
      let rec go k1 = function
        | Tip -> raise Not_found
        | Node ((k, _, _ as kvg), l, r) ->
            let cmp = K.compare k1 k in
            if cmp < 0 then
              Node (kvg, go k1 l, r)
            else if cmp > 0 then
              Node (kvg, l, go k1 r)
            else merge l r in
      (cons ~gen:t.gen (t.size - 1) (go k t.tree))

    let remove k t = try remove_exn k t with Not_found -> t

    let add k v ({ gen; _ } as t) =
      let rec go k1 v1 = function
        | Tip -> Node ((k1, v1, gen), Tip, Tip)
        | Node ((k, _, _ as kvg), l, r) ->
            if K.compare k1 k < 0 then
              Node (kvg, go k1 v1 l, r)
            else Node (kvg, l, go k1 v1 r) in
      { tree = go k v t.tree; gen = gen + 1; size = t.size + 1 }

    let find k t =
      try let (x, t) = find_remove_exn k t in Some (x, add k x t)
      with Not_found -> None

    let lru t = match t.tree with
      | Tip -> None
      | Node ((k, v, _), _, _) -> Some (k, v)

    let drop t = match t.tree with
      | Tip -> t
      | Node (_, l, r) -> cons ~gen:t.gen (t.size - 1) (merge l r)

    let size t = t.size

    let rec trim n t = if size t > n then trim n (drop t) else t

    let iter f t =
      let rec go f = function
        | Tip -> ()
        | Node ((k, v, _), l, r) -> go f l; f k v; go f r in
      go f t.tree

    let fold f t s =
      let rec go f s = function
        | Tip -> s
        | Node ((k, v, _), l, r) -> go f (f k v (go f s r)) l in
      go f s t.tree

    let fmap f t =
      let rec go f = function
        | Tip -> (Tip, 0)
        | Node ((k, v, g), l, r) ->
            let (l, n1) = go f l and (r, n2) = go f r in
            match f k v with
            | None   -> (merge l r, n1 + n2)
            | Some v -> (Node ((k, v, g), l, r), n1 + n2 + 1) in
      let (tree, size) = go f t.tree in
      { t with tree; size }

    let map f t = fmap (fun k v -> Some (f k v)) t

    let filter f t = fmap (fun k v -> if f k v then Some v else None) t

    let pp ppkv ppf t = pp iter ppkv ppf t
  end

end

module M = struct

  module Q = struct

    type 'a node = {
      value : 'a;
      mutable next : 'a node option;
      mutable prev : 'a node option
    }

    type 'a t = {
      mutable first : 'a node option;
      mutable last  : 'a node option
    }

    let detach t n =
      let np = n.prev and nn = n.next in
      ( match np with
        | None   -> t.first <- nn
        | Some x -> x.next <- nn; n.prev <- None );
      ( match nn with
        | None   -> t.last <- np
        | Some x -> x.prev <- np; n.next <- None )

    let append t n =
      let on = Some n in
      match t.last with
      | Some x as l -> x.next <- on; t.last <- on; n.prev <- l
      | None        -> t.first <- on; t.last <- on

    let node x = { value = x; prev = None; next = None }

    let create () = { first = None; last = None }

    let iterl f t =
      let rec go f = function
        | None   -> ()
        | Some n -> f n.value; go f n.next in
      go f t.first

    let fold f t s =
      let rec go f s = function
        | None -> s
        | Some n -> go f (f n.value s) n.next in
      go f s t.first
  end

  module type S = sig

    type 'v t
    type k

    val create : ?random:bool -> int -> 'v t
    val size : 'v t -> int
    val mem : k -> 'v t -> bool
    val add : k -> 'v -> 'v t -> unit
    val find : k -> 'v t -> 'v option
    val remove : k -> 'v t -> unit
    val lru : 'v t -> (k * 'v) option
    val drop : 'v t -> unit
    val trim : int -> 'v t -> unit
    val fold : (k -> 'v -> 'a -> 'a) -> 'v t -> 'a -> 'a
    val iter : (k -> 'v -> unit) -> 'v t -> unit
    val pp : (k * 'v) fmt -> 'v t fmt
  end

  module Bake (HT : Hashtbl.SeededS) = struct

    type k = HT.key

    type 'v t = {
      ht : (k * 'v) Q.node HT.t;
      q  : (k * 'v) Q.t;
    }

    let create ?random n = { ht = HT.create ?random n; q = Q.create () }

    let remove k t =
      try
        let n = HT.find t.ht k in
        HT.remove t.ht k;
        Q.detach t.q n
      with Not_found -> ()

    let add k v t =
      let n = Q.node (k, v) in
      HT.add t.ht k n;
      Q.append t.q n

    let find k t =
      try
        let n = HT.find t.ht k in
        Q.( detach t.q n; append t.q n );
        Some (snd n.Q.value)
      with Not_found -> None

    let mem k t = HT.mem t.ht k

    let lru t = match t.q.Q.first with
      | Some n -> Some n.Q.value
      | None   -> None

    let drop t = match t.q.Q.first with
      | Some n ->
          HT.remove t.ht (fst n.Q.value);
          Q.detach t.q n
      | None -> ()

    let size t = HT.length t.ht

    let rec trim n t = if size t > n then (drop t; trim n t)

    let iter f t = Q.iterl (fun (k, v) -> f k v) t.q

    let fold f t s = Q.fold (fun (k, v) a -> f k v a) t.q s

    let pp ppkv ppf t = pp iter ppkv ppf t

  end

  module Make (K : Hashtbl.HashedType) =
    Bake (Hashtbl.MakeSeeded (struct include K let hash (_: int) x = hash x end))

  module MakeSeeded (K : Hashtbl.SeededHashedType) =
    Bake (Hashtbl.MakeSeeded (K))

end
