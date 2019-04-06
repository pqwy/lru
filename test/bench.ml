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

module type S = sig
  type t
  val mk : int list -> t
  val q : int -> t -> unit
  val a : int -> int -> t -> unit
end

let double xs = List.map (fun x -> (x, x)) xs
let randoms n = List.init n (fun _ -> Random.int 2_000_000)

open Unmark

let suite ms n =
  let rs, rs1 = randoms n, randoms n in
  group (string_of_int n) [
    group "q" (ms |> List.map @@ fun (name, (module M: S)) ->
      let t = M.mk rs in
      (* bench name (fun () -> M.q (Random.int 2_000_000) t)) *)
      bench name (fun () -> rs |> List.iter (fun x -> M.q x t)))
  ; group "a" (ms |> List.map @@ fun (name, (module M: S)) ->
      let t = M.mk rs in
      (* bench name (fun () -> let x = Random.int 2_000_000 in M.a x x t)) *)
      bench name (fun () -> rs1 |> List.iter (fun x -> M.a x x t)))
  ]

let impls = [
  "fun", (module struct
    type t = F.t
    let mk xs = F.of_list (double xs)
    let q x q = F.find ~promote:false x q |> Sys.opaque_identity |> ignore
    let a x y q = F.add x y q |> Sys.opaque_identity |> ignore
  end: S)
; "imp",
  (module struct
    type t = M.t
    let mk xs = M.of_list (double xs)
    let q x q = M.find ~promote:false x q |> Sys.opaque_identity |> ignore
    let a a b m = M.add a b m
  end: S)
; "ht",
  (module struct
    type t = (int, int) Hashtbl.t
    let mk xs =
      let h = Hashtbl.create 20 in
      xs |> List.iter (fun x -> Hashtbl.replace h x x);
      h
    let a k v m = Hashtbl.replace m k v
    let q k m = Hashtbl.add m k |> ignore
  end: S)
]

let arg = Cmdliner.Arg.(value @@ opt (list int) [10; 100; 1000] @@ info ["size"])
let _ = Unmark_cli.main_ext "lru" ~arg @@ List.map (suite impls)
