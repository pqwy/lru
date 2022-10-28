(* Copyright (c) 2015-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

module type Weighted = sig type t val weight : t -> int end

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

type 'a fmt = Format.formatter -> 'a -> unit

let pf = Format.fprintf

let pp_iter ?(sep = Format.pp_print_space) pp ppf i =
  let first = ref true in
  i @@ fun x ->
    (match !first with true -> first := false | _ -> sep ppf ());
    pp ppf x

let cap_makes_sense ~m ~f cap =
  if cap < 0 then invalid_arg "Lru.%s.%s: ~cap:%d" m f cap

module F = struct

  module type S = sig
    type t
    type k
    type v
    val empty : int -> t
    val is_empty : t -> bool
    val size : t -> int
    val weight : t -> int
    val capacity : t -> int
    val resize : int -> t -> t
    val trim : t -> t
    val mem : k -> t -> bool
    val find : k -> t -> v option
    val promote : k -> t -> t
    val add : k -> v -> t -> t
    val remove : k -> t -> t
    val pop : k -> t -> (v * t) option
    val lru : t -> (k * v) option
    val drop_lru : t -> t
    val pop_lru : t -> ((k * v) * t) option
    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val fold_k : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : (k -> v -> unit) -> t -> unit
    val iter_k : (k -> v -> unit) -> t -> unit
    val of_list : (k * v) list -> t
    val to_list : t -> (k * v) list
    val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt
    val pp_dump : k fmt -> v fmt -> t fmt
  end

  module Make (K: Map.OrderedType) (V: Weighted) = struct

    module Q = Psq.Make (K) (struct
      type t = int * V.t
      let compare (g1, _) (g2, _) = compare (g1: int) g2
    end)

    type k = K.t
    type v = V.t

    type t = { cap: int; w: int; gen: int; q: Q.t }

    let g0 = min_int

    let is_empty t = Q.is_empty t.q
    let size t = Q.size t.q
    let weight t = t.w
    let capacity t = t.cap

    let cap_makes_sense = cap_makes_sense ~m:"F"

    let empty cap =
      cap_makes_sense ~f:"empty" cap; { cap; w = 0; gen = g0; q = Q.empty }

    let resize cap t = cap_makes_sense ~f:"resize" cap; { t with cap }

    let mem k t = Q.mem k t.q

    let find k t = match Q.find k t.q with Some (_, v) -> Some v | _ -> None

    let trim t =
      let rec go t w q =
        if w > t.cap then match Q.pop q with
          Some ((_, (_, v)), q) -> go t (w - V.weight v) q
        | None -> assert false
        else { t with w; q } in
      if t.w > t.cap then go t t.w t.q else t

    let promote k ({ gen; _ } as t) =
      if gen = max_int then empty t.cap else
        { t with gen = gen + 1; q = Q.adjust k (fun (_, v) -> gen, v) t.q }

    let rec add k v ({ gen; _ } as t) =
      if gen = max_int then add k v (empty t.cap) else
        let p = Some (gen, v) and p0 = ref None in
        let q = Q.update k (fun x -> p0 := x; p) t.q in
        let w = t.w + V.weight v -
          (match !p0 with Some (_, v0) -> V.weight v0 | _ -> 0) in
        { t with gen = gen + 1; w; q }

    let remove k t = match Q.find k t.q with
      None -> t
    | Some (_, v) -> { t with w = t.w - V.weight v; q = Q.remove k t.q }

    let pop k t = match Q.find k t.q with
      None -> None
    | Some (_, v) ->
        Some (v, { t with w = t.w - V.weight v; q = Q.remove k t.q })

    let lru t = match Q.min t.q with Some (k, (_, v)) -> Some (k, v) | _ -> None

    let pop_lru t = match Q.pop t.q with
      None -> None
    | Some ((k, (_, v)), q) ->
        Some ((k, v), { t with w = t.w - V.weight v; q })

    let drop_lru t = match Q.pop t.q with
      None -> t
    | Some ((_, (_, v)), q) -> { t with w = t.w - V.weight v; q }

    let sort_uniq_r xs =
      let rec sieve k0 kv0 = function
      | [] -> [kv0]
      | (k, _ as kv)::kvs when K.compare k0 k = 0 -> sieve k kv kvs
      | (k, _ as kv)::kvs -> kv0 :: sieve k kv kvs in
      let cmp (k1, (g1, _)) (k2, (g2, _)) =
        match K.compare k1 k2 with 0 -> compare (g1: int) g2 | r -> r
      in
      match List.sort cmp xs with [] -> [] | (k, _ as kv)::kvs -> sieve k kv kvs

    let of_list xs =
      let rec annotate g acc = function
      | (k, v)::kvs -> annotate (succ g) ((k, (g, v))::acc) kvs
      | [] -> g, sort_uniq_r acc in
      let gen, kgvs = annotate g0 [] xs in
      let q = Q.of_sorted_list kgvs in
      let w = Q.fold (fun _ (_, v) w -> w + V.weight v) 0 q in
      { cap = w; w; gen; q }

    let fold f z t =
      List.fold_right (fun (k, (_, v)) acc -> f k v acc)
        (Q.to_priority_list t.q) z
    let iter f t =
      Q.to_priority_list t.q |> List.iter (fun (k, (_, v)) -> f k v)
    let to_list t = fold (fun k v kvs -> (k, v) :: kvs) [] t

    let fold_k f z t = Q.fold (fun k (_, v) -> f k v) z t.q
    let iter_k f t = Q.iter (fun k (_, v) -> f k v) t.q

    let pp ?(pp_size = fun _ -> ignore) ?sep pp ppf t =
      let ppx ppf (k, (_, v)) = pp ppf (k, v) in
      pf ppf "@[%a@[%a@]@]" pp_size (t.w, t.cap)
        (pp_iter ?sep ppx) (fun f -> List.iter f (Q.to_priority_list t.q))

    let pp_dump ppk ppv ppf =
      let sep ppf () = pf ppf ";@ "
      and ppkv ppf (k, v) = pf ppf "(@[%a,@ %a@])" ppk k ppv v in
      pf ppf "of_list [%a]" (pp ~sep ppkv)
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

    let iter f t =
      let rec go f = function Some n -> f n.value; go f n.next | _ -> () in
      go f t.first

    let fold f t z =
      let rec go f z = function Some n -> go f (f n.value z) n.prev | _ -> z in
      go f z t.last
  end

  module type S = sig
    type t
    type k
    type v
    val create : ?random:bool -> ?initialSize: int -> int -> t
    val is_empty : t -> bool
    val size : t -> int
    val weight : t -> int
    val capacity : t -> int
    val resize : int -> t -> unit
    val trim : t -> unit
    val mem : k -> t -> bool
    val find : k -> t -> v option
    val promote : k -> t -> unit
    val add : k -> v -> t -> unit
    val remove : k -> t -> unit
    val lru : t -> (k * v) option
    val drop_lru : t -> unit
    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : (k -> v -> unit) -> t -> unit
    val of_list : (k * v) list -> t
    val to_list : t -> (k * v) list
    val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt
    val pp_dump : k fmt -> v fmt -> t fmt
  end

  module Bake (HT: Hashtbl.SeededS) (V: Weighted) = struct

    type k = HT.key
    type v = V.t

    type t = {
      ht : (k * v) Q.node HT.t;
      q  : (k * v) Q.t;
      mutable cap : int;
      mutable w   : int;
    }

    let size t = HT.length t.ht
    let weight t = t.w
    let capacity t = t.cap
    let is_empty t = HT.length t.ht = 0

    let cap_makes_sense = cap_makes_sense ~m:"M"

    let create ?random ?initialSize cap =
      let hashSize =
        match initialSize with | None -> cap | (Some v) -> v in
      cap_makes_sense ~f:"create" cap;
      { cap; w = 0; ht = HT.create ?random hashSize; q = Q.create () }

    let lru t = match t.q.Q.first with Some n -> Some n.Q.value | _ -> None

    let drop_lru t = match t.q.Q.first with
      None -> ()
    | Some ({ Q.value = (k, v); _ } as n) ->
        t.w <- t.w - V.weight v;
        HT.remove t.ht k;
        Q.detach t.q n

    let rec trim t = if weight t > t.cap then (drop_lru t; trim t)

    let resize cap t = cap_makes_sense ~f:"resize" cap; t.cap <- cap

    let remove k t =
      try
        let n = HT.find t.ht k in
        t.w <- t.w - (snd n.Q.value |> V.weight);
        HT.remove t.ht k; Q.detach t.q n
      with Not_found -> ()

    let add k v t =
      remove k t;
      let n = Q.node (k, v) in
      t.w <- t.w + V.weight v;
      HT.add t.ht k n; Q.append t.q n

    let promote k t =
      try
        let n = HT.find t.ht k in Q.( detach t.q n; append t.q n )
      with Not_found -> ()

    let find k t =
      try Some (snd (HT.find t.ht k).Q.value) with Not_found -> None

    let mem k t = HT.mem t.ht k

    let iter f t = Q.iter (fun (k, v) -> f k v) t.q
    let fold f z t = Q.fold (fun (k, v) a -> f k v a) t.q z
    let to_list t = Q.fold (fun x xs -> x::xs) t.q []

    let of_list xs =
      let t = create 0 in
      List.iter (fun (k, v) -> add k v t) xs;
      resize (Q.fold (fun (_, v) w -> w + V.weight v) t.q 0) t;
      t

    let pp ?(pp_size = fun _ -> ignore) ?sep pp ppf t =
      pf ppf "@[%a@[%a@]@]" pp_size (t.w, t.cap)
        (pp_iter ?sep pp) (fun f -> Q.iter f t.q)

    let pp_dump ppk ppv ppf =
      let sep ppf () = pf ppf ";@ "
      and ppkv ppf (k, v) = pf ppf "(@[%a,@ %a@])" ppk k ppv v in
      pf ppf "of_list [%a]" (pp ~sep ppkv)
  end

  module Make (K: Hashtbl.HashedType) (V: Weighted) =
    Bake (Hashtbl.MakeSeeded (struct
      include K
      let hash _ = hash
      let seeded_hash = hash [@@ocaml.warning "-32"]
    end)) (V)

  module MakeSeeded (K : Hashtbl.SeededHashedType) (V: Weighted) =
    Bake (Hashtbl.MakeSeeded (K)) (V)

end

let memo (type k) (type v)
    ?(hashed=(Hashtbl.hash, (=))) ?(weight = fun _ -> 1) ~cap f =
  let module C =
    M.Make (struct type t = k let hash = fst hashed let equal = snd hashed end)
           (struct type t = v let weight = weight end) in
    let c = C.create cap in
    let rec g k = match C.find k c with
      None   -> let v = f g k in C.add k v c; v
    | Some v -> C.promote k c; v in
    g
