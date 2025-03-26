open Core

let is_example = true

let filename =
  if is_example then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename
let result_p1 = 0
let result_p2 = 0
