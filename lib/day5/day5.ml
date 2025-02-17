open Core

let aoc_input = In_channel.read_lines "lib/day5/input.txt"
let offsets = List.map aoc_input ~f:int_of_string |> Array.of_list
let result_p1 = 0
let result_p2 = 0
