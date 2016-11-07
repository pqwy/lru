(* Copyright (c) 2015-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

module type Ordered = sig type t val compare : t -> t -> int end
module type Weighted = sig type t val weight : t -> int end

type 'a fmt = Format.formatter -> 'a -> unit

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

module F = struct

  module type S = sig
    type t
    type k
    type v
    val empty : int -> t
    val is_empty : t -> bool
    val items : t -> int
    val size : t -> int
    val capacity : t -> int
    val resize : int -> t -> t
    val mem : k -> t -> bool
    val find : k -> t -> (v * t) option
    val add : k -> v -> t -> t
    val remove : k -> t -> t
    val unadd : k -> t -> (v * t) option
    val lru : t -> (k * v) option
    val drop_lru : t -> t
    val pop_lru : t -> ((k * v) * t) option
    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : (k -> v -> unit) -> t -> unit
    val to_list : t -> (k * v) list
    val of_list : ?cap:int -> (k * v) list -> t
    val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt
    val pp_dump : (k * v) fmt -> t fmt
  end

  module Make (K: Ordered) (V: Weighted) = struct

    module Q = Psq.Make (K) (struct
      type t = int * V.t
      let compare (g1, _) (g2, _) = compare (g1: int) g2
    end)

    type k = K.t
    type v = V.t

    type t = { cap: int; w: int; gen: int; q: Q.t }

    let g0 = min_int

    let is_empty t = Q.is_empty t.q
    let items t = Q.size t.q
    let size t = t.w
    let capacity t = t.cap

    let cap_makes_sense caller cap =
      if cap < 1 then invalid_arg "Lru.F.%s: given capacity %d" caller cap

    let empty cap =
      cap_makes_sense "empty" cap; { cap; w = 0; gen = g0; q = Q.empty }

    let mem k t = Q.mem k t.q

    let trim t =
      let rec go t w q =
        if w > t.cap then match Q.pop q with
          | Some ((_, (_, v)), q) -> go t (w - V.weight v) q
          | None -> assert false
        else { t with w; q } in
      if t.w > t.cap then go t t.w t.q else t

    let resize cap t = cap_makes_sense "resize" cap; trim { t with cap }

    let remove k t = match Q.find k t.q with
      | None -> t
      | Some (_, v) -> { t with w = t.w - V.weight v; q = Q.remove k t.q }

    let unadd k t = match Q.find k t.q with
      | None -> None
      | Some (_, v) ->
          Some (v, { t with w = t.w - V.weight v; q = Q.remove k t.q })

    let rec add k v t =
      if t.gen < max_int then
        let p   = (t.gen, v)
        and w   = t.w + V.weight v
        and gen = t.gen + 1 in
        trim @@ match Q.find k t.q with
        | None -> { t with gen; w; q = Q.add k p t.q }
        | Some (_, v0) ->
            { t with gen; w = w - V.weight v0; q = Q.adjust (fun _ -> p) k t.q }
      else add k v (empty t.cap)

    let find k ({ gen; _ } as t) =
      let r = ref None in
      let q = Q.adjust (fun (_, v) -> r := Some v; (gen, v)) k t.q in
      match !r with
      | Some v when gen < max_int -> Some (v, { t with gen = gen + 1; q })
      | Some v -> Some (v, add k v (empty t.cap))
      | None -> None

    let lru t = match Q.min t.q with Some (k, (_, v)) -> Some (k, v) | _ -> None

    let pop_lru t = match Q.pop t.q with
      | None -> None
      | Some ((k, (_, v)), q) ->
          Some ((k, v), { t with w = t.w - V.weight v; q })

    let drop_lru t = match Q.pop t.q with
      | None -> t
      | Some ((_, (_, v)), q) -> { t with w = t.w - V.weight v; q }

    let fold f z t = Q.fold (fun k (_, v) -> f k v) z t.q

    let iter f t = Q.iter (fun k (_, v) -> f k v) t.q

    let list_w = List.fold_left (fun w (_, v) -> w + V.weight v) 0

    let of_list ?cap xs =
      let cap = match cap with Some x -> x | _ -> list_w xs in
      let rec annotate g w acc = function
        | []         -> (g, w, acc)
        | (k, v)::xs ->
            annotate (succ g) (w + V.weight v) ((k, (g, v))::acc) xs in
      let (gen, w, kgvs) = annotate g0 0 [] xs in
      trim { cap; w; gen; q = Q.of_list kgvs }

    let to_list t = Q.fold (fun k (_, v) kvs -> (k, v) :: kvs) [] t.q

    let pp ?pp_size ?sep pp ppf t =
      ( match pp_size with
        | Some pps -> pps ppf (t.w, t.cap)
        | _        -> Format.fprintf ppf "size: %d/%d;@ " t.w t.cap );
      Q.pp ?sep (fun ppf (k, (_, v)) -> pp ppf (k, v)) ppf t.q

    let pp_dump pp ppf t =
      let g x = x - g0 in
      let sep ppf () = Format.fprintf ppf ",@ "
      and ppkv ppf (k, (gen, v)) =
        Format.fprintf ppf "@[%a @@ %d@]" pp (k, v) (g gen) in
      Format.fprintf ppf "{@[size: %d/%d;@ gen: %d;@ @[%a@]@]}"
        t.w t.cap (g t.gen) Q.(pp ~sep ppkv) t.q
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

    let fold ?(dir=`Up) f s t =
      let rec go_r f s = function
        Some n -> go_r f (f n.value s) n.next | _ -> s in
      let rec go_l f s = function
        Some n -> go_l f (f n.value s) n.prev | _ -> s in
      match dir with `Up -> go_r f s t.first | `Down -> go_l f s t.last

    let iter ?(dir=`Up) f t =
      let rec go_r f = function Some n -> f n.value; go_r f n.next | _ -> () in
      let rec go_l f = function Some n -> f n.value; go_l f n.prev | _ -> () in
      match dir with `Up -> go_r f t.first | `Down -> go_l f t.last
  end

  module type S = sig
    type t
    type k
    type v
    val create : ?random:bool -> int -> t
    val is_empty : t -> bool
    val items : t -> int
    val size : t -> int
    val capacity : t -> int
    val resize : int -> t -> unit
    val mem : k -> t -> bool
    val find : k -> t -> v option
    val add : k -> v -> t -> unit
    val remove : k -> t -> unit
    type dir = [ `Up | `Down ]
    val cache : t -> (k -> v) -> k -> v
    val lru : t -> (k * v) option
    val drop_lru : t -> unit
    val fold : ?dir:dir -> (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : ?dir:dir -> (k -> v -> unit) -> t -> unit
    val to_list : ?dir:dir -> t -> (k * v) list
    val of_list : ?cap:int -> (k * v) list -> t
    val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt
    val pp_dump : (k * v) fmt -> t fmt
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

    let items t = HT.length t.ht

    let size t = t.w

    let capacity t = t.cap

    let is_empty t = HT.length t.ht = 0

    let err_capacity = invalid_arg "Lru.M.%s: given capacity %d"

    let create ?random cap =
      if cap < 1 then err_capacity "create" cap;
      { cap; w = 0; ht = HT.create ?random cap; q = Q.create () }

    let lru t = match t.q.Q.first with
      | Some n -> Some n.Q.value
      | None   -> None

    let drop_lru t = match t.q.Q.first with
      | None -> ()
      | Some ({ Q.value = (k, v); _ } as n) ->
          t.w <- t.w - V.weight v;
          HT.remove t.ht k;
          Q.detach t.q n

    let rec trim t = if size t > t.cap then (drop_lru t; trim t)

    let resize cap t =
      if cap < 1 then err_capacity "resize" cap;
      t.cap <- cap; trim t

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
      HT.add t.ht k n; Q.append t.q n; trim t

    let find k t =
      try
        let n = HT.find t.ht k in
        Q.( detach t.q n; append t.q n );
        Some (snd n.Q.value)
      with Not_found -> None

    let mem k t = HT.mem t.ht k

    let cache t f k = match find k t with
      | Some v -> v
      | None   -> let v = f k in add k v t; v

    type dir = [ `Up | `Down ]

    let iter ?dir f t = Q.iter ?dir (fun (k, v) -> f k v) t.q

    let fold ?dir f z t = Q.fold ?dir (fun (k, v) a -> f k v a) z t.q

    let to_list ?dir t = Q.fold ?dir (fun x xs -> x::xs) [] t.q

    let list_w = List.fold_left (fun w (_, v) -> w + V.weight v) 0

    let of_list ?cap xs =
      let cap = match cap with Some x -> x | _ -> list_w xs in
      let t = create cap in List.iter (fun (k, v) -> add k v t) xs; t

    let pp_q sep pp ppf t =
      let fst = ref true in
      Format.fprintf ppf "@[%a@]"
        (fun ppf -> Q.iter ~dir:`Down @@ fun kv ->
          if not !fst then sep ppf (); fst := false; pp ppf kv)
        t.q

    let pp ?(pp_size) ?(sep=Format.pp_print_space) pp ppf t =
      ( match pp_size with
        | Some pps -> pps ppf (t.w, t.cap)
        | _        -> Format.fprintf ppf "size: %d/%d;@ " t.w t.cap );
      pp_q sep pp ppf t

    let pp_dump pp ppf t =
      let sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "{@[size: %d/%d;@ MRU: %a@]}"
        t.w t.cap (pp_q sep pp) t
end

  module SeededHash (H: Hashtbl.HashedType) = struct
    include H let hash _ x = hash x
  end

  module Make (K : Hashtbl.HashedType) (V: Weighted) =
    Bake (Hashtbl.MakeSeeded (SeededHash (K))) (V)

  module MakeSeeded (K : Hashtbl.SeededHashedType) (V: Weighted) =
    Bake (Hashtbl.MakeSeeded (K)) (V)

end
