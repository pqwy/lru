#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opams = [Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["unmark"])]

let () =
  Pkg.describe "lru-map" ~opams @@ fun c ->
  Ok [ Pkg.mllib "src/lru-map.mllib";
       Pkg.test "test/test";
       Pkg.test ~run:false "test/bench"; ]
