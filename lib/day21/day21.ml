open Core

let is_example = true

let filename =
  if is_example then "lib/day21/example.txt" else "lib/day21/input.txt"

let aoc_input = In_channel.read_lines filename
let start_pattern = [| [| 0; 1; 0 |]; [| 0; 0; 1 |]; [| 1; 1; 1 |] |]

let print_pattern pattern =
  Array.iter pattern ~f:(fun line ->
      Array.iter line ~f:(fun px ->
          Printf.printf "%c" (if px = 1 then '#' else '.'));
      Printf.printf "\n")

let parse_line line =
  let input_pattern, output_pattern =
    Scanf.sscanf line "%s => %s" (fun a b -> (a, b))
  in
  (input_pattern, output_pattern)

let rules = List.map aoc_input ~f:parse_line
let result_p1 = 0
let result_p2 = 0
