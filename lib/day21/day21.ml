open Core

let is_example = false

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
   123 741 987 369
   456 852 654 258
   789 963 321 147
*)
let rotations3 pattern =
  let rec loop acc count p =
    if count = 0 then List.rev acc
    else
      let rot =
        [|
          [| p.(2).(0); p.(1).(0); p.(0).(0) |];
          [| p.(2).(1); p.(1).(1); p.(0).(1) |];
          [| p.(2).(2); p.(1).(2); p.(0).(2) |];
        |]
      in
      loop (rot :: acc) (count - 1) rot
  in
  loop [ pattern ] 3 pattern

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
  let size = Array.length p1 in
  let size2 = Array.length p2 in
  if size <> size2 then false
  else
    let rec row_loop row =
      let rec col_loop col =
        if col < 0 then true
        else if Char.equal p1.(row).(col) p2.(row).(col) then col_loop (col - 1)
        else false
      in
      if row < 0 then true
      else if col_loop (size - 1) then row_loop (row - 1)
      else false
    in
    row_loop (size - 1)

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
  let variants = flips_and_rots pattern in
  let rec loop rules =
    match rules with
    | [] -> failwith "No rule found"
    | rule :: rest ->
        (* printf "Rule : %s\n" (fst rule); *)
        if
          List.exists variants ~f:(fun p ->
              let patt_of_rule = pattern_of_string (fst rule) in
              is_patt_match p patt_of_rule)
        then (
          (* printf "Found pattern\n";
             print_pattern pattern;
             print_pattern (pattern_of_string (fst rule)); *)
          let rp = pattern_of_string (snd rule) in
          printf "Output patt: \n";
          print_pattern rp;
          rp)
        else loop rest
  in
  loop rules

let expand pattern rules =
  let l = Array.length pattern in
  let size = if l mod 3 = 0 then 3 else 2 in
  (* printf "Size: %d\n" size; *)
  let no_pats = Array.length pattern / size in
  (* printf "no_pats: %d\n" no_pats; *)
  let rows = ref [] in
  for row = 0 to no_pats - 1 do
    let cols = ref [] in
    for col = 0 to no_pats - 1 do
      let sub_pattern = extract pattern (row * size) (col * size) size in
      let new_pattern = replace_pattern sub_pattern rules in
      cols := new_pattern :: !cols
    done;
    rows := !cols @ !rows
  done;
  let exp_pat = Array.of_list !rows in
  (* Array.iteri exp_pat ~f:(fun i p ->
      printf "%d:\n" i;
      print_pattern p); *)
  (* printf "Size of pat: %d\n" (Array.length exp_pat); *)
  let new_size = (size + 1) * no_pats in
  let new_arr = Array.make_matrix ~dimx:new_size ~dimy:new_size '.' in
  for row = 0 to no_pats - 1 do
    for col = 0 to no_pats - 1 do
      for r = 0 to size do
        for c = 0 to size do
          let pat_no = (row * no_pats) + col in
          (* printf "pat_no %d, r %d, c %d\n" pat_no r c; *)
          let target_row = (row * (size + 1)) + r in
          let target_col = (col * (size + 1)) + c in
          new_arr.(target_row).(target_col) <- exp_pat.(pat_no).(r).(c)
        done
      done
    done
  done;
  new_arr

let rules = List.map aoc_input ~f:parse_line

let pixel_count pattern =
  Array.fold ~init:0 pattern ~f:(fun acc sp ->
      acc
      + Array.fold sp ~init:0 ~f:(fun acc el ->
            acc + if Char.equal el '#' then 1 else 0))

let solve_p1 pattern rules no_iters =
  let rec loop count pattern =
    print_pattern pattern;
    if count = 0 then pattern else loop (count - 1) (expand pattern rules)
  in
  let exp_pattern = loop no_iters pattern in
  pixel_count exp_pattern

let result_p1 = solve_p1 start_pattern rules 2

(* 152 too high *)
(* 136 too low *)
let result_p2 = 0
