(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

module I = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
  let weight _ = 1
end
module F = Lru.F.Make (I) (I)
module M = Lru.M.Make (I) (I)

let () = Unmark.warmup ()

let double xs = List.map (fun x -> (x, x)) xs

let rec init f = function 0 -> [] | n ->
  let x = f n in x :: init f (pred n)

let randoms = init (fun _ -> Random.int 2_000_000)

let f_of_xs xs = F.of_list (double xs)
let m_of_xs xs = M.of_list (double xs)
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
  Format.printf "+ find\n%!";
  let (mk, q) = (f_of_xs, (fun x q -> F.find x q |> ignore)) in
  rquery "fun" mk q 10;
  rquery "fun" mk q 100;
  rquery "fun" mk q 1000;
  let (mk, q) = (m_of_xs, (fun x q -> M.find x q |> ignore)) in
  rquery "imp" mk q 10;
  rquery "imp" mk q 100;
  rquery "imp" mk q 1000;
  let (mk, q) = (ht_of_xs, (fun x q -> Hashtbl.find q x |> ignore)) in
  rquery "ht" mk q 10;
  rquery "ht" mk q 100;
  rquery "ht" mk q 1000;
  ()

let radd tag mk a n =
  let (rs, rs') = (randoms n, randoms n) in
  let m         = mk rs in
  let tag       = Format.sprintf "q: %s/%d" tag n in
  Unmark.time ~tag ~measure:`Cputime_ns ~n:1000 @@ fun () ->
    rs' |> List.iter (fun x -> a x x m)

let () =
  Format.printf "+ add\n%!";
  let (mk, q) = (f_of_xs, (fun k v m -> F.add k v m |> ignore)) in
  radd "fun" mk q 10;
  radd "fun" mk q 100;
  radd "fun" mk q 1000;
  let (mk, q) = (m_of_xs, M.add) in
  radd "imp" mk q 10;
  radd "imp" mk q 100;
  radd "imp" mk q 1000;
  let (mk, q) = (ht_of_xs, (fun k v m -> Hashtbl.replace m k v)) in
  radd "ht" mk q 10;
  radd "ht" mk q 100;
  radd "ht" mk q 1000;
  ()
