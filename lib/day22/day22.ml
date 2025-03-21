open Core

let is_example = true

let filename =
  if is_example then "lib/day22/example.txt" else "lib/day22/input.txt"

module Pos = struct
  module T = struct
    type t = { x : int; y : int }

    let compare t1 t2 =
      let cmp = Int.compare t1.x t2.x in
      if cmp <> 0 then cmp else Int.compare t1.y t2.y

    let sexp_of_t t : Sexp.t = List [ Int.sexp_of_t t.x; Int.sexp_of_t t.y ]
    let hash t = Hashtbl.hash (t.x, t.y)
  end

  include T
  include Comparator.Make (T)
end

let aoc_input = In_channel.read_lines filename
let height = List.length aoc_input
let width = String.length (List.hd_exn aoc_input)
let viruses = Set.empty (module Pos)
let result_p1 = 0
let result_p2 = 0
