open Core

let is_example = false

let filename =
  if is_example then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_all filename

let remove_cancelled s =
  let lst = String.to_list s in
  let rec rem_loop acc lst =
    match lst with
    | [] -> List.rev acc
    | a :: [] -> if Char.( = ) a '!' then List.rev acc else List.rev (a :: acc)
    | a :: b :: rest ->
        if Char.( = ) a '!' then rem_loop acc rest
        else rem_loop (a :: acc) (b :: rest)
  in
  String.of_list (rem_loop [] lst)

let result_p1 = 0
let result_p2 = 0
