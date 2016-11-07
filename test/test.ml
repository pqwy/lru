(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

open Alcotest

module K = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
end

module F = Lru_map.F.Make (K)
module M = Lru_map.M.Make (K)

let mk_m xs = let m = M.create 20 in List.iter (fun x -> M.add x x m) xs; m
let find_m k m = match M.find k m with Some v -> Some (v, m) | _ -> None
let mem_m = M.mem
let remove_m k m = M.remove k m; m
let lru_m = M.lru
let drop_m m = M.drop m; m

let mk_f = List.fold_left (fun m x -> F.add x x m) F.empty
let find_f = F.find
let mem_f = F.mem
let remove_f = F.remove
let lru_f = F.lru
let drop_f = F.drop

let () = Random.self_init ()

let map f = function Some a -> Some (f a) | _ -> None

let random _ = Random.int 1_000_000

let rec init f = function 0 -> [] | n -> let x = f n in x :: init f (pred n)

let contains tag ~mem ~find m xs =
  let f m k =
    match find k m with
    | None -> fail (Format.sprintf "contains: %s: %d not found" tag k)
    | Some (v, m) ->
        check int (Format.sprintf "contains: %s: %d -> %d" tag k k) k v;
        m in
  List.fold_left f m xs;
  List.iter (fun x -> check bool "mem" true (mem x m)) xs

let remembers ~mk ~mem ~find ~remove () =
  let ns = init random 200 in
  let mp = mk ns in
  contains "after insertion" ~mem ~find mp ns;
  let (xs, ys) = List.partition (fun x -> x mod 2 = 0) ns in
  let mp' = List.fold_left (fun mp x -> remove x mp) mp xs in
  contains "after removal" ~mem ~find mp' ys;
  xs |> List.iter (fun x ->
    check (option int) "does not contain" None (find x mp' |> map fst));
  ()

let lru ~mk ~lru ~drop () =
  let ns = init random 200 in
  let f m x =
    check (option (pair int int)) "lru" (Some (x, x)) (lru m);
    drop m in
  List.fold_left f (mk ns) ns |> ignore

let () = run "lru_map" [
  "M", [
    "remembers", `Quick, remembers ~mk:mk_m ~mem:mem_m ~find:find_m ~remove:remove_m;
    "lru",       `Quick, lru ~mk:mk_m ~lru:lru_m ~drop:drop_m;
  ] ;
  "F", [
    "remembers", `Quick, remembers ~mk:mk_f ~mem:mem_f ~find:find_f ~remove:remove_f;
    "lru",       `Quick, lru ~mk:mk_f ~lru:lru_f ~drop:drop_f;
  ]
]
