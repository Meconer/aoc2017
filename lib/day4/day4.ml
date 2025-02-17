open Core

let aoc_input = In_channel.read_lines "lib/day4/input.txt"

let is_valid line =
  let parts = String.split ~on:' ' line in
  let rec loop lst =
    match lst with
    | [] -> true
    | word :: rest ->
        if List.exists rest ~f:(fun rest_word -> String.equal rest_word word)
        then false
        else loop rest
  in
  loop parts

let is_valid_p2 line =
  let parts =
    String.split ~on:' ' line |> List.map ~f:String.to_list
    |> List.map ~f:(List.sort ~compare:Char.compare)
    |> List.map ~f:String.of_list
  in
  let rec loop lst =
    match lst with
    | [] -> true
    | word :: rest ->
        if List.exists rest ~f:(fun rest_word -> String.equal rest_word word)
        then false
        else loop rest
  in
  loop parts

let result_p1 = List.count aoc_input ~f:is_valid
let result_p2 = List.count aoc_input ~f:is_valid_p2
