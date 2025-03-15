open Core

let is_example = true

let filename =
  if is_example then "lib/day21/example.txt" else "lib/day21/input.txt"

let aoc_input = In_channel.read_lines filename

let print_pattern pattern =
  Array.iter pattern ~f:(fun line ->
      Array.iter line ~f:(fun px -> Printf.printf "%c" px);
      printf "\n");
  printf "\n"

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
  [
    [| [| p.(0).(0); p.(0).(1) |]; [| p.(1).(0); p.(1).(1) |] |];
    (* Orig*)
    [| [| p.(1).(0); p.(0).(0) |]; [| p.(1).(1); p.(0).(1) |] |];
    (* 1 rot right*)
    [| [| p.(1).(1); p.(1).(0) |]; [| p.(0).(1); p.(0).(0) |] |];
    (* 2 rot right*)
    [| [| p.(0).(1); p.(1).(1) |]; [| p.(0).(0); p.(1).(0) |] |];
    (* 3 rot right*)
  ]

let flips2 p =
  [
    [| [| p.(0).(0); p.(0).(1) |]; [| p.(1).(0); p.(1).(1) |] |];
    (* Orig*)
    [| [| p.(0).(1); p.(0).(0) |]; [| p.(1).(1); p.(1).(0) |] |];
    (* Flip hor*)
    [| [| p.(1).(0); p.(1).(1) |]; [| p.(0).(0); p.(0).(1) |] |];
    (* Flip ver*)
  ]

(*
   123 412 741 874 987 698 369 236
   456 753 852 951 654 357 258 159
   789 896 963 632 321 214 147 478
*)
let rotations3 pattern =
  let rec loop acc count p =
    if count = 0 then List.rev acc
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

let flips3 p =
  [
    [|
      [| p.(0).(0); p.(0).(1); p.(0).(2) |];
      [| p.(1).(0); p.(1).(1); p.(1).(2) |];
      [| p.(2).(0); p.(2).(1); p.(2).(2) |];
    |];
    [|
      [| p.(0).(2); p.(0).(1); p.(0).(0) |];
      [| p.(1).(2); p.(1).(1); p.(1).(0) |];
      [| p.(2).(2); p.(2).(1); p.(2).(0) |];
    |];
    [|
      [| p.(2).(0); p.(2).(1); p.(2).(2) |];
      [| p.(1).(0); p.(1).(1); p.(1).(2) |];
      [| p.(0).(0); p.(0).(1); p.(0).(2) |];
    |];
  ]

let flips_and_rots p =
  let l = Array.length p in
  if l mod 3 = 0 then flips3 p |> List.concat_map ~f:rotations3
  else flips2 p |> List.concat_map ~f:rotations2

let is_patt_match p1 p2 =
  Array.for_all2_exn p1 p2 ~f:(fun row1 row2 ->
      Array.for_all2_exn row1 row2 ~f:Char.equal)

let is_inp_match inp p =
  let fr = flips_and_rots p in
  List.exists fr ~f:(fun p -> is_patt_match p inp)

let extract pattern start_row start_col size =
  let arr = Array.make_matrix ~dimx:size ~dimy:size '.' in
  for r = 0 to size - 1 do
    for c = 0 to size - 1 do
      arr.(r).(c) <- pattern.(start_row + r).(start_col + c)
    done
  done;
  arr

let replace_pattern pattern rules =
  let


let expand pattern =
  let l = Array.length pattern in
  let size = if l mod 3 = 0 then 3 else 2 in
  let no_pats = Array.length pattern / size in
  let rows = ref [] in
  for row = 0 to no_pats - 1 do
    let cols = ref [] in
    for col = 0 to no_pats - 1 do
      let sub_pattern = extract pattern (row * size) (col * size) size in
      let new_pattern = replace_pattern sub_pattern in
      cols := sub_pattern :: !cols
    done;
    rows := !cols :: !rows
  done

let rules = List.map aoc_input ~f:parse_line
let result_p1 = 0
let result_p2 = 0
