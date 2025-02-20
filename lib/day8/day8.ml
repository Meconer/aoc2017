open Core

let is_example = true

let filename =
  if is_example then "lib/day8/example.txt" else "lib/day8/input.txt"

let aoc_input = In_channel.read_lines filename

let parse_line line =
  let mod_reg, op, cond_reg, cond, cond_val =
    Scanf.scanf "%s %s %d if %s %s %d" line ~f:(fun a b c d e f ->
        (a, b, c, d, e, f))
  in
  (mod_reg, op, cond_reg, cond, cond_val)

let result_p1 = 0
let result_p2 = 0
