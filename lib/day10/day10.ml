open Core

let is_example = true

let filename, list_size =
  if is_example then ("lib/day10/example.txt", 5)
  else ("lib/day10/input.txt", 256)

let aoc_input = In_channel.read_all filename

let parse_input s =
  String.split s ~on:',' |> List.map ~f:String.strip
  |> List.map ~f:int_of_string

let solve_p1 l_seq lst =
  let rec loop l_seq skip_size lst =
    match l_seq with
    | [] -> lst
    | move_len :: rest ->
        let part1 = List.sub lst ~pos:0 ~len:move_len in
        let part2 =
          List.sub lst ~pos:move_len ~len:(List.length lst - move_len)
        in
        let new_list = List.rev part1 @ part2 in
        let skip_length = move_len + skip_size in
        let list_after_skip =
          List.sub new_list ~pos:skip_length
            ~len:(List.length new_list - skip_length)
          @ List.sub new_list ~pos:0 ~len:skip_length
        in
        loop rest (skip_size + 1) list_after_skip
  in
  loop l_seq 0 lst

let lst = List.init list_size ~f:(fun i -> i)
let result_p1 = 0
let result_p2 = 0
