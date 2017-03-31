(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

open Lru
open Alcotest

module K = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
  let weight _ = 1
end

let strf = Format.sprintf

let double xs = List.map (fun x -> (x, x)) xs

let map f = function Some a -> Some (f a) | _ -> None

let random _ = Random.int 1_000_000_000

let rec init f = function 0 -> [] | n -> let x = f n in x :: init f (pred n)

type 'a m = (module F.S with type t = 'a and type k = int and type v = int)

let contains (type t) (m: t m) (t: t) xs =
  let module M = (val m) in
  List.iter (fun x -> check (option int) "find" (Some x) (M.find x t |> map fst)) xs;
  List.iter (fun x -> check bool "mem" true (M.mem x t)) xs

let remembers (type t) (m: t m) () =
  let module M = (val m) in
  let ns = init random 200 in
  let mp = M.of_list (double ns) in
  contains m mp ns;
  let (xs, ys) = List.partition (fun x -> x mod 2 = 0) ns in
  let mp' = List.fold_left (fun mp x -> M.remove x mp) mp xs in
  contains m mp' ys;
  List.iter (fun x ->
    check (option int) "does not contain" None (M.find x mp' |> map fst)
  ) xs

let lru (type t) (m: t m) () =
  let module M = (val m) in
  let ns = init random 200 in
  let f m x =
    check (option (pair int int)) "lru" (Some (x, x)) (M.lru m);
    M.drop_lru m in
  List.fold_left f (M.of_list @@ double ns) ns |> ignore

let replaces (type t) (m: t m) () =
  let module M = (val m) in
  let (ns1, ns2) = (init (fun x -> x) 100, init (fun x -> x + 100) 100) in
  let mp = M.of_list @@ double (ns1 @ ns2) in
  let mp = List.fold_left (fun a k -> M.remove k a) mp ns2 in
  check int "size" 100 (M.size mp);
  let mp = List.fold_left (fun m k -> M.add k (13 * k) m) mp ns1 in
  check int "size after replace" 100 (M.size mp)

let list_conv (type t) (m: t m) () =
  let module M = (val m) in
  let mp = M.of_list [(3,1); (1,3); (3,2); (2,2); (1,2); (3,3); (1,1)] in
  let xs = M.to_list mp |> List.sort compare in
  check int "size" 3 (M.size mp);
  check int "capacity" 3 (M.capacity mp);
  check (list (pair int int)) "bindings" [(1,1);(2,2);(3,3)] xs

let memo () =
  let fib f = function 0|1 as n -> n | n -> f (n - 1) + f (n - 2) in
  Lru.memo ~cap:2 fib 300 |> ignore (* It's about the time. *)

let () = Random.self_init ()

let fm: F.Make(K)(K).t m =
  let module M = F.Make(K)(K) in (module M)

let mm: Adapt.M_as_F(K)(K).t m =
  let module M = Adapt.M_as_F(K)(K) in (module M)

let () = run "lru" [
  "F", [
    "add/remove", `Quick, remembers fm;
    "lru order",  `Quick, lru fm;
    "replaces",   `Quick, replaces fm;
    "list conv",  `Quick, list_conv fm;
  ] ;
  "M", [
    "add/remove", `Quick, remembers mm;
    "lru order",  `Quick, lru mm;
    "replaces",   `Quick, replaces mm;
    "list conv",  `Quick, list_conv fm;
    "memo",       `Quick, memo;
  ] ;
]
