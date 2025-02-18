open Core

let is_example = false

let filename =
  if is_example then "lib/day7/example.txt" else "lib/day6/input.txt"

let aoc_input = In_channel.read_lines filename
let result_p1 = 0
let result_p2 = 0
