open Core

let is_example = false

let filename =
  if is_example then "lib/day19/example.txt" else "lib/day19/input.txt"

let aoc_input = In_channel.read_lines filename

let grid =
  aoc_input |> List.map ~f:String.to_list |> List.map ~f:Array.of_list
  |> Array.of_list

let printline n =
  for i = 0 to Array.length grid.(0) - 1 do
    Printf.printf "%c" grid.(n).(i)
  done

let printgrid () =
  for i = 0 to Array.length grid - 1 do
    printline i;
    print_endline ""
  done

let start_col () =
  fst (Option.value_exn (Array.findi grid.(0) ~f:(fun _ c -> Char.equal c '|')))

type dir_t = Up | Down | Left | Right
type pos_t = { r : int; c : int }
type state_t = { pos : pos_t; dir : dir_t }

let get_ch pos = grid.(pos.r).(pos.c)

let move pos dir =
  match dir with
  | Up -> { r = pos.r - 1; c = pos.c }
  | Down -> { r = pos.r + 1; c = pos.c }
  | Left -> { r = pos.r; c = pos.c - 1 }
  | Right -> { r = pos.r; c = pos.c + 1 }

let turn pos dir =
  match dir with
  | Up ->
      let left = move pos Left in
      let right = move pos Right in
      if Char.equal (get_ch left) ' ' then (right, Right) else (left, Left)
  | Down ->
      let left = move pos Right in
      let right = move pos Left in
      if Char.equal (get_ch left) ' ' then (right, Left) else (left, Right)
  | Left ->
      let left = move pos Down in
      let right = move pos Up in
      if Char.equal (get_ch left) ' ' then (right, Up) else (left, Down)
  | Right ->
      let left = move pos Up in
      let right = move pos Down in
      if Char.equal (get_ch left) ' ' then (right, Down) else (left, Up)

let string_of_dir dir =
  match dir with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let print_state state =
  Printf.printf "Pos: r-%d c-%d   |  Dir: %s\n" state.pos.r state.pos.c
    (string_of_dir state.dir)

let solve_p1 () =
  let state = { pos = { r = 0; c = start_col () }; dir = Down } in
  let step_count = ref 0 in

  let rec loop acc state =
    (* print_state state; *)
    step_count := !step_count + 1;
    let ch = get_ch state.pos in
    match (state.dir, ch) with
    | _, ' ' -> (String.of_list (List.rev acc), !step_count - 1)
    | dir, '|' -> loop acc { state with pos = move state.pos dir }
    | dir, '-' -> loop acc { state with pos = move state.pos dir }
    | dir, '+' ->
        let new_pos, new_dir = turn state.pos dir in
        loop acc { pos = new_pos; dir = new_dir }
    | dir, ch when Char.is_alpha ch ->
        loop (ch :: acc) { state with pos = move state.pos dir }
    | _, _ -> failwith "Wtf?"
  in
  loop [] state

let result_p1, result_p2 = solve_p1 ()
