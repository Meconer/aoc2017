open Core

let is_example = false

let filename =
  if is_example then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_lines filename
let result_p1 = 0
let result_p2 = 0
