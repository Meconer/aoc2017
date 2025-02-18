open Core

let is_example = true

let aoc_input =
  if is_example then "0\t2\t7\t0" else In_channel.read_all "lib/day6/input.txt"

let blocks =
  String.split aoc_input ~on:'\t' |> List.map ~f:int_of_string |> Array.of_list

let solve_p1 blocks =
  let len = Array.length blocks in
  let max_val = Array.fold ~init:(-1) blocks ~f:(fun a el -> max a el) in
  let spread_idx, _ = Array.findi_exn blocks ~f:(fun _ el -> el = max_val) in
  let rec loop idx amount =
    Printf.printf "%d\n" amount;
    if amount = 0 then ()
    else (
      blocks.(idx) <- blocks.(idx) + 1;
      let n_idx = (idx + 1) mod len in
      loop n_idx (max 0 (amount - 1)))
  in
  let start_idx = (spread_idx + 1) mod len in
  blocks.(spread_idx) <- 0;
  loop start_idx max_val

let result_p1 = 0
let result_p2 = 0
