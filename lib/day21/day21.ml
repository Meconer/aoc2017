open Core

let is_example = true

let filename =
  if is_example then "lib/day21/example.txt" else "lib/day21/input.txt"

let aoc_input = In_channel.read_lines filename

let print_pattern pattern =
  Array.iter pattern ~f:(fun line ->
      Array.iter line ~f:(fun px -> Printf.printf "%c" px);
      Printf.printf "\n")

let parse_line line =
  let input_pattern, output_pattern =
    Scanf.sscanf line "%s => %s" (fun a b -> (a, b))
  in
  (input_pattern, output_pattern)

let pattern_of_string s =
  let parts = String.split s ~on:'/' in
  List.map parts ~f:String.to_array |> Array.of_list

let start_pattern = pattern_of_string ".#./..#/###"

(*
   12 31 43 24
   34 42 21 13
*)
let rotations2 p =
  [|
    [| p.(0).(0); p.(0).(1); p.(1).(0); p.(1).(1) |];
    (* Orig*)
    [| p.(1).(0); p.(0).(0); p.(1).(1); p.(1).(0) |];
    (* 1 rot right*)
    [| p.(1).(1); p.(1).(0); p.(0).(1); p.(0).(0) |];
    (* 2 rot right*)
    [| p.(0).(1); p.(1).(1); p.(0).(0); p.(1).(0) |];
    (* 3 rot right*)
  |]

(*
   123 412 741 874 987 698 369 236
   456 753 852 951 654 357 258 159
   789 896 963 632 321 214 147 478
*)
let rotations3 pattern =
  let rec loop acc count p =
    if count = 0 then List.rev acc |> Array.of_list
    else
      let rot =
        [|
          [| p.(1).(0); p.(0).(0); p.(0).(1) |];
          [| p.(2).(0); p.(1).(1); p.(0).(2) |];
          [| p.(2).(1); p.(2).(2); p.(1).(2) |];
        |]
      in
      loop (rot :: acc) (count - 1) rot
  in
  loop [ pattern ] 7 pattern

let rules = List.map aoc_input ~f:parse_line
let result_p1 = 0
let result_p2 = 0
