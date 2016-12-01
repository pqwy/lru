(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

open Alcotest

module K = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
  let weight _ = 1
end

module F = Lru.F.Make (K) (K)
module M = Lru.M.Make (K) (K)

let strf = Format.sprintf

let double xs = List.map (fun x -> (x, x)) xs

type ('k, 'v, 't) ops = {
  mk       : 'k list -> 't;
  find     : 'k -> 't -> ('v * 't) option;
  mem      : 'k -> 't -> bool;
  remove   : 'k -> 't -> 't;
  lru      : 't -> ('k * 'v) option;
  drop_lru : 't -> 't;
  add      : 'k -> 'v -> 't -> 't;
  size     : 't -> int
}

let ops_m = M.{
  mk       = (fun xs -> M.of_list (double xs));
  find     = (fun k m -> match M.find k m with Some v -> Some (v, m) | _ -> None);
  remove   = (fun k m -> M.remove k m; m);
  drop_lru = (fun m -> drop_lru m; m);
  add      = (fun k v m -> add k v m; m);
  mem; lru; size
}

and ops_f = F.{
  mk = (fun xs -> of_list (double xs));
  find; mem; remove; lru; drop_lru; add; size
}

let pp_ii ppf (x, y) = Format.fprintf ppf "@[%d@ ->@ %d@]" x y

let map f = function Some a -> Some (f a) | _ -> None

let random _ = Random.int 1_000_000_000

let rec init f = function 0 -> [] | n -> let x = f n in x :: init f (pred n)

let contains { mem; find; _ } m xs =
  List.iter (fun x -> check (option int) "find" (Some x) (find x m |> map fst)) xs;
  List.iter (fun x -> check bool "mem" true (mem x m)) xs

let remembers ({ mk; find; remove; _ } as ops) () =
  let ns = init random 200 in
  let mp = mk ns in
  contains ops mp ns;
  let (xs, ys) = List.partition (fun x -> x mod 2 = 0) ns in
  let mp' = List.fold_left (fun mp x -> remove x mp) mp xs in
  contains ops mp' ys;
  List.iter (fun x ->
    check (option int) "does not contain" None (find x mp' |> map fst)
  ) xs

let lru { mk; lru; drop_lru; _ } () =
  let ns = init random 200 in
  let f m x =
    check (option (pair int int)) "lru" (Some (x, x)) (lru m); drop_lru m in
  List.fold_left f (mk ns) ns |> ignore

let replaces { mk; size; add; remove; _ } () =
  let (ns1, ns2) = (init (fun x -> x) 100, init (fun x -> x + 100) 100) in
  let mp = mk (ns1 @ ns2) in
  let mp = List.fold_left (fun a k -> remove k a) mp ns2 in
  check int "size" 100 (size mp);
  let mp = List.fold_left (fun m k -> add k (13 * k) m) mp ns1 in
  check int "size after replace" 100 (size mp)

let memo () =
  let fib f = function 0|1 as n -> n | n -> f (n - 1) + f (n - 2) in
  Lru.memo ~cap:2 fib 300 |> ignore (* It's about the time. *)


let () = Random.self_init ()

let () = run "lru" [
  "F", [
    "add/remove", `Quick, remembers ops_m;
    "lru order",  `Quick, lru ops_m;
    "replaces",   `Quick, replaces ops_m;
  ] ;
  "M", [
    "add/remove", `Quick, remembers ops_m;
    "lru order",  `Quick, lru ops_m;
    "replaces",   `Quick, replaces ops_m;
    "memo",       `Quick, memo;
  ] ;
]
