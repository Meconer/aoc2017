open Core

let is_example = false

let file_name =
  if is_example then "lib/day2/example.txt" else "lib/day2/input.txt"

let aoc_input = In_channel.read_lines file_name
let split_char = if is_example then ' ' else '\t'

let list_of_line line =
  String.split ~on:split_char line |> List.map ~f:int_of_string

let get_diffs line_lst =
  List.map line_lst ~f:(fun line ->
      let i_list = list_of_line line in
      let sorted = List.sort i_list ~compare:Int.descending in
      List.hd_exn sorted - List.last_exn sorted)

let diffs = get_diffs aoc_input
let result_p1 = List.fold ~init:0 diffs ~f:( + )

let find_factor a lst =
  let rec loop lst =
    match lst with
    | [] -> None
    | b :: rest ->
        let d1 = max a b in
        let d2 = min a b in
        if d1 mod d2 = 0 then Some (d1 / d2) else loop rest
  in
  loop lst

let get_factor lst =
  let rec loop lst =
    match lst with
    | [] | _ :: [] -> failwith "Did not find divisable"
    | a :: rest -> (
        let factor_opt = find_factor a rest in
        match factor_opt with None -> loop rest | Some f -> f)
  in
  loop lst

let solve_p2 lines =
  let lists = List.map lines ~f:list_of_line in
  let line_factors = List.map lists ~f:(fun list -> get_factor list) in
  List.fold line_factors ~init:0 ~f:( + )

let result_p2 = solve_p2 aoc_input
