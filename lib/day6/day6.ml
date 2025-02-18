open Core

let is_example = false

let aoc_input =
  if is_example then "0\t2\t7\t0" else In_channel.read_all "lib/day6/input.txt"

let blocks =
  String.split aoc_input ~on:'\t' |> List.map ~f:int_of_string |> Array.of_list

let string_of_arr arr =
  Array.fold arr ~init:"" ~f:(fun acc el -> acc ^ string_of_int el ^ ":")

let solve_p1 blocks =
  let len = Array.length blocks in
  let seen = Set.empty (module String) in

  let rec loop seen count =
    let str_of_blocks = string_of_arr blocks in
    if Set.mem seen str_of_blocks then count
    else
      let seen = Set.add seen (string_of_arr blocks) in
      let max_val = Array.fold ~init:(-1) blocks ~f:(fun a el -> max a el) in
      let spread_idx, _ =
        Array.findi_exn blocks ~f:(fun _ el -> el = max_val)
      in

      let rec spread idx amount =
        if amount = 0 then ()
        else (
          blocks.(idx) <- blocks.(idx) + 1;
          let n_idx = (idx + 1) mod len in
          spread n_idx (max 0 (amount - 1)))
      in
      let start_idx = (spread_idx + 1) mod len in
      blocks.(spread_idx) <- 0;
      spread start_idx max_val;
      loop seen (count + 1)
  in
  loop seen 0

let result_p1 = solve_p1 blocks

(* Reset blocks array *)
let blocks =
  String.split aoc_input ~on:'\t' |> List.map ~f:int_of_string |> Array.of_list

let solve_p2 blocks =
  let len = Array.length blocks in
  let seen = Map.empty (module String) in

  let rec loop seen count =
    let str_of_blocks = string_of_arr blocks in
    if Map.mem seen str_of_blocks then count - Map.find_exn seen str_of_blocks
    else
      let seen = Map.set seen ~key:(string_of_arr blocks) ~data:count in
      let max_val = Array.fold ~init:(-1) blocks ~f:(fun a el -> max a el) in
      let spread_idx, _ =
        Array.findi_exn blocks ~f:(fun _ el -> el = max_val)
      in

      let rec spread idx amount =
        if amount = 0 then ()
        else (
          blocks.(idx) <- blocks.(idx) + 1;
          let n_idx = (idx + 1) mod len in
          spread n_idx (max 0 (amount - 1)))
      in
      let start_idx = (spread_idx + 1) mod len in
      blocks.(spread_idx) <- 0;
      spread start_idx max_val;
      loop seen (count + 1)
  in
  loop seen 0

let result_p2 = solve_p2 blocks
