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
  val q : int -> t -> int option
  val a : int -> int -> t -> unit
  val r : int -> t -> unit
end

let r_int () = Random.int 2_000_000
let double xs = List.map (fun x -> (x, x)) xs
let randoms n = List.init n (fun _ -> r_int ())

open Unmark

let suite ms n =
  let rs = randoms n in
  (* let rs1 = randoms n in *)
  group (string_of_int n) [
    (* group "mk" (ms |> List.map @@ fun (name, (module M: S)) -> *)
    (*   bench name (fun () -> M.mk rs)); *)
    group "find" (ms |> List.map @@ fun (name, (module M: S)) ->
      let t = M.mk rs in
      let x = r_int () in
      bench name (fun () -> M.q x t))
      (* bench name (fun () -> rs |> List.iter (fun x -> M.q x t |> ignore))) *)
  ; group "add" (ms |> List.map @@ fun (name, (module M: S)) ->
      let t = M.mk rs in
      let x = r_int () in
      bench name (fun () -> M.a x x t))
      (* bench name (fun () -> *)
      (*   let t = M.mk rs in rs1 |> List.iter (fun x -> M.a x x t))) *)
  ; group "remove" (ms |> List.map @@ fun (name, (module M: S)) ->
      let t = M.mk rs in
      let x = r_int () in
      bench name (fun () -> M.r x t));
      (* bench name (fun () -> *)
      (*   let t = M.mk rs in rs1 |> List.iter (fun x -> M.r x t))); *)
  ]

let impls = [
  "fun", (module struct
    type t = F.t ref
    let mk xs = ref (F.of_list (double xs))
    let q k q = F.find k !q
    let a k v q = q := F.add k v !q
    let r k q = q := F.remove k !q
  end: S)
; "imp",
  (module struct
    type t = M.t
    let mk xs = M.of_list (double xs)
    let q k q = M.find k q
    let a = M.add
    let r = M.remove
  end: S)
; "ht",
  (module struct
    type t = (int, int) Hashtbl.t
    let mk xs =
      let h = Hashtbl.create 20 in
      xs |> List.iter (fun x -> Hashtbl.replace h x x);
      h
    let q k m = Hashtbl.find_opt m k
    let a k v m = Hashtbl.replace m k v
    let r k m = Hashtbl.remove m k
  end: S)
]

let arg = Cmdliner.Arg.(
  value @@ opt (list int) [10; 100; 1000] @@ info ["sizes"])
let _ = Unmark_cli.main_ext "lru" ~arg @@ List.map (suite impls)
