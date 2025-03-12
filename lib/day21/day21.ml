open Core

let is_example = false

let filename =
  if is_example then "lib/day21/example.txt" else "lib/day21/input.txt"

let aoc_input = In_channel.read_lines filename

let parse_line line =
  let input_pattern, output_pattern =
    Scanf.sscanf line "%s => %s" (fun a b -> (a, b))
  in
  (input_pattern, output_pattern)

let rules = List.map aoc_input ~f:parse_line
let result_p1 = 0
let result_p2 = 0
