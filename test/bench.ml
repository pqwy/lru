(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

module Int = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
end
module K = Lru_map.F.Make (Int)
module L = Lru_map.M.Make (Int)

let () = Unmark.warmup ()

let rec init f = function 0 -> [] | n ->
  let x = f n in x :: init f (pred n)

let randoms = init (fun _ -> Random.int 2_000_000)

let f_of_xs = List.fold_left (fun m x -> K.add x x m) K.empty
let i_of_xs xs =
  let m = L.create 20 in List.iter (fun x -> L.add x x m) xs; m
let ht_of_xs xs =
  let h = Hashtbl.create 20 in
  xs |> List.iter (fun x -> Hashtbl.replace h x x); h

let rquery tag mk q n =
  let rs  = randoms n in
  let m   = mk rs in
  let tag = Format.sprintf "q: %s/%d" tag n in
  Unmark.time ~tag ~measure:`Cputime_ns ~n:1000 @@ fun () ->
    rs |> List.iter (fun x -> q x m)

let () =
  let (mk, q) = (f_of_xs, (fun x q -> K.find x q |> ignore)) in
  rquery "fun" mk q 10;
  rquery "fun" mk q 100;
  rquery "fun" mk q 1000;
  let (mk, q) = (i_of_xs, (fun x q -> L.find x q |> ignore)) in
  rquery "imp" mk q 10;
  rquery "imp" mk q 100;
  rquery "imp" mk q 1000;
  let (mk, q) = (ht_of_xs, (fun x q -> Hashtbl.find q x |> ignore)) in
  rquery "ht" mk q 10;
  rquery "ht" mk q 100;
  rquery "ht" mk q 1000;
  ()
