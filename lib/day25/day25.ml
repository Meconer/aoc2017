open Core

let is_example = false

let filename =
  if is_example then "lib/day25/example.txt" else "lib/day25/input.txt"

let aoc_input = In_channel.read_lines filename

type action_t = { write_val : int; move : int; next_state : int }

let state_of_char state_ch = int_of_char state_ch - int_of_char 'A'

let build_half_state lines =
  if List.length lines <> 3 then failwith "Must be 3 lines"
  else
    let write_val =
      Scanf.sscanf (List.nth_exn lines 0) "    - Write the value %d." Fn.id
    in
    let move_str =
      Scanf.sscanf (List.nth_exn lines 1) "    - Move one slot to the %s" Fn.id
    in
    let move = if String.is_prefix move_str ~prefix:"left" then -1 else 1 in
    let next_state_ch =
      Scanf.sscanf (List.nth_exn lines 2) "    - Continue with state %c." Fn.id
    in
    let next_state = state_of_char next_state_ch in
    { write_val; move; next_state }

let build_state lines =
  if List.length lines <> 9 then failwith "Must be 9 lines"
  else
    let st_0 = build_half_state (List.sub lines ~pos:2 ~len:3) in
    let st_1 = build_half_state (List.sub lines ~pos:6 ~len:3) in
    [| st_0; st_1 |]

let parse_input lines =
  let start_state =
    Scanf.sscanf (List.hd_exn lines) "Begin in state %c." Fn.id
  in
  let no_of_steps =
    Scanf.sscanf (List.nth_exn lines 1)
      "Perform a diagnostic checksum after %d steps." Fn.id
  in
  let rec loop acc lines =
    if List.length lines < 2 then List.rev acc
    else
      (* List.iter lines ~f:(fun l -> printf "%s\n" l); *)
      let state = build_state (List.sub lines ~pos:0 ~len:9) in
      loop (state :: acc) (List.drop lines 10)
  in
  let states = loop [] (List.drop lines 3) |> Array.of_list in
  (state_of_char start_state, no_of_steps, states)

let result_p1 = 0
let result_p2 = 1
