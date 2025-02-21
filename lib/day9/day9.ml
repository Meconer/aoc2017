open Core

let is_example = true

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

let remove_garbage s =
  let lst = String.to_list s in
  let rec rem_loop acc lst in_gc =
    match lst with
    | [] -> List.rev acc
    | a :: rest when Char.equal a '<' -> rem_loop acc rest true
    | a :: rest when Char.equal a '>' ->
        if in_gc then rem_loop acc rest false
        else rem_loop (a :: acc) rest false
    | a :: rest ->
        if in_gc then rem_loop acc rest true else rem_loop (a :: acc) rest false
  in
  String.of_list (rem_loop [] lst false)


let count_groups s = 
  let lst = String.to_list s in
  let rec count_loop acc_count level lst = 
    match lst with 
    | [] -> acc_count
    | a::rest when Char.equal a '{'->  count_loop (acc_count) (level+1) rest
    | a::rest when Char.equal a '}'-> count_loop (acc_count + level) (level -1) rest
    

let result_p1 = 0
let result_p2 = 0
