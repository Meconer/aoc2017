open Core

let is_example = true

let filename =
  if is_example then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename

let parse_line line =
  let parts = String.split line ~on:'/' |> List.map ~f:int_of_string in
  let components = (List.hd_exn parts, List.nth_exn parts 1) in
  let next_match = ref 0 in

  components

let solve_p1 () =
  let comps = List.map aoc_input ~f:parse_line in
  comps

let result_p1 = 0
let result_p2 = 0
