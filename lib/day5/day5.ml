open Core

let is_example = false

let aoc_input =
  if is_example then [ "0"; "3"; "0"; "1"; "-3" ]
  else In_channel.read_lines "lib/day5/input.txt"

let offsets = List.map aoc_input ~f:int_of_string |> Array.of_list

let solve_p1 offsets =
  let ptr = ref 0 in
  let rec loop ctr =
    if !ptr < 0 || !ptr >= Array.length offsets then ctr
    else
      let n_ptr = !ptr + offsets.(!ptr) in
      offsets.(!ptr) <- offsets.(!ptr) + 1;
      ptr := n_ptr;
      loop (ctr + 1)
  in
  loop 0

let result_p1 = 0
let result_p2 = 0
