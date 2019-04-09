(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

let id x = x
let (%) f g x = f (g x)

module I = struct
  type t = int
  let compare (a: int) b = compare a b
  let equal (a: int) b = a = b
  let hash (i: int) = Hashtbl.hash i
  let weight _ = 1
end

let sort_uniq_r (type a) cmp xs =
  let module S = Set.Make (struct type t = a let compare = cmp end) in
  List.fold_right S.add xs S.empty |> S.elements
let uniq_r (type a) cmp xs =
  let module S = Set.Make (struct type t = a let compare = cmp end) in
  let rec go s acc = function
    []    -> acc
  | x::xs -> if S.mem x s then go s acc xs else go (S.add x s) (x :: acc) xs in
  go S.empty [] (List.rev xs)
let list_of_iter_2 i =
  let xs = ref [] in i (fun a b -> xs := (a, b) :: !xs); List.rev !xs

let list_trim w xs =
  let rec go wacc acc = function
    []     -> acc
  | kv::xs -> let w' = I.weight (snd kv) + wacc in
             if w' <= w then go w' (kv::acc) xs else acc in
  go 0 [] (List.rev xs)
let list_weight = List.fold_left (fun a (_, v) -> a + I.weight v) 0

let cmpi (a: int) b = compare a b
let cmp_k (k1, _) (k2, _) = cmpi k1 k2
let sorted_by_k xs = List.sort cmp_k xs

let size = QCheck.Gen.(small_nat >|= fun x -> x mod 1_000)
let bindings = QCheck.(
  make Gen.(list_size size (pair small_nat small_nat))
    ~print:Fmt.(to_to_string Fmt.(Dump.(list (pair int int))))
    ~shrink:Shrink.list)

let test name gen p =
  QCheck.Test.make ~name gen p |> QCheck_alcotest.to_alcotest


module F = Lru.F.Make (I) (I)
let pp_f = Fmt.(F.pp_dump int int)
let (!) f = `Sem F.(to_list f, size f, weight f)
let sem xs = `Sem List.(xs, length xs, list_weight xs)
let lru = QCheck.(
  map F.of_list bindings ~rev:F.to_list |>
    set_print Fmt.(to_to_string pp_f))
let lru_w_nat = QCheck.(pair lru small_nat)

let () = Alcotest.run ~and_exit:false "Lru.F" [

  "of_list", [
    test "sem" bindings
      (fun xs -> !F.(of_list xs) = sem (uniq_r cmp_k xs));
    test "cap" bindings
      (fun xs -> F.(capacity (of_list xs)) = list_weight (uniq_r cmp_k xs));
  ];

  "membership", [
    test "find sem" lru_w_nat
      (fun (m, x) -> F.find x m = List.assoc_opt x (F.to_list m));
    test "mem ==> find" lru_w_nat
      (fun (m, e) -> QCheck.assume (F.mem e m); F.find e m <> None);
    test "find ==> mem" lru_w_nat
      (fun (m, e) -> QCheck.assume (F.find e m <> None); F.mem e m);
  ];

  "add", [
    test "sem" lru_w_nat
      (fun (m, k) ->
        !(F.add k k m) = sem (List.remove_assoc k (F.to_list m) @ [k, k]));
  ];

  "remove", [
    test "sem" lru_w_nat
      (fun (m, k) -> !(F.remove k m) = sem (List.remove_assoc k (F.to_list m)));
  ];

  "trim", [
    test "sem" lru_w_nat
      (fun (m, x) ->
        !F.(resize x m |> trim) = sem (list_trim x (F.to_list m)));
  ];

  "promote", [
    test "sem" lru_w_nat
      (fun (m, x) ->
        !(F.promote x m) =
          !(match F.find x m with Some v -> F.add x v m | _ -> m));
  ];

  "lru", [
    test "lru sem" lru
      (fun m ->
        QCheck.assume (F.size m > 0);
        F.lru m = Some (List.hd (F.to_list m)));
    test "drop_lru sem" lru
      (fun m ->
        QCheck.assume (F.size m > 0);
        F.(to_list (drop_lru m) = List.tl (F.to_list m)));
  ];

  "conv", [
    test "to_list inv" lru (fun m -> !F.(of_list (to_list m)) = !m);
    test "to_list = fold" lru
      (fun m -> F.to_list m = F.fold (fun k v a -> (k, v)::a) [] m);
    test "to_list = iter" lru
      (fun m -> list_of_iter_2 (fun f -> F.iter f m) = F.to_list m);
    test "fold_k sem" lru
      (fun m ->
        F.fold_k (fun k v a -> (k, v)::a) [] m = sorted_by_k (F.to_list m));
    test "iter_k sem" lru
      (fun m ->
        list_of_iter_2 (fun f -> F.iter_k f m) = sorted_by_k (F.to_list m));
  ]

]

module M = Lru.M.Make (I) (I)
let pp_m = Fmt.(M.pp_dump int int)
let (!!) m = `Sem M.(to_list m, size m, weight m)
let lru = QCheck.(
  map M.of_list bindings ~rev:M.to_list |>
    set_print Fmt.(to_to_string pp_m))
let lru_w_nat = QCheck.(pair lru small_nat)
let lrus = QCheck.(
  map (fun xs -> M.of_list xs, F.of_list xs) ~rev:(F.to_list % snd) bindings
  |> set_print Fmt.(to_to_string pp_f % snd))
let lrus_w_nat = QCheck.(pair lrus small_nat)

let () = Alcotest.run "Lru.M" [

  "of_list", [
    test "sem" bindings
      (fun xs -> !!M.(of_list xs) = sem (uniq_r cmp_k xs));
    test "cap" bindings
      (fun xs -> M.(capacity (of_list xs)) = list_weight (uniq_r cmp_k xs));
  ];

  "membership", [
    test "find" lrus_w_nat (fun ((m, f), x) -> M.find x m = F.find x f);
    test "mem" lrus_w_nat (fun ((m, f), x) -> M.mem x m = F.mem x f);
  ];

  "add", [
    test "eqv" lrus_w_nat
      (fun ((m, f), x) -> M.add x x m; !!m = !(F.add x x f))
  ];

  "remove", [
    test "eqv" lrus_w_nat
      (fun ((m, f), x) -> M.remove x m; !!m = !(F.remove x f));
  ];

  "trim", [
    test "eqv" lrus_w_nat
      (fun ((m, f), x) ->
        M.resize x m; M.trim m; !!m = !F.(resize x f |> trim));
  ];

  "promote", [
    test "eqv" lrus_w_nat
      (fun ((m, f), x) -> M.promote x m; !!m = !(F.promote x f));
  ];

  "lru", [
    test "eqv" lrus (fun (m, f) -> M.lru m = F.lru f);
    test "drop eqv" lrus (fun (m, f) -> M.drop_lru m; !!m = !F.(drop_lru f));
  ];

  "conv", [
    test "to_list inv" lru (fun m -> !!M.(of_list (to_list m)) = !!m);
    test "to_list = fold" lru
      (fun m -> M.fold (fun k v a -> (k, v)::a) [] m = M.to_list m);
    test "to_list = iter" lru
      (fun m -> list_of_iter_2 (fun f -> M.iter f m) = M.to_list m)
  ];

  "pp", [
    test "eqv" lrus
      (fun (m, f) -> Fmt.(to_to_string pp_m m = to_to_string pp_f f));
  ]
]
