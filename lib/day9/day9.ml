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

let remove_garbage s =
  let lst = String.to_list s in
  let rec rem_loop acc count lst in_gc =
    match lst with
    | [] -> (List.rev acc, count)
    | a :: rest when Char.equal a '<' ->
        let n_count = if in_gc then count + 1 else count in
        rem_loop acc n_count rest true
    | a :: rest when Char.equal a '>' ->
        if in_gc then rem_loop acc count rest false
        else rem_loop (a :: acc) count rest false
    | a :: rest ->
        if in_gc then rem_loop acc (count + 1) rest true
        else rem_loop (a :: acc) count rest false
  in
  let lst, count = rem_loop [] 0 lst false in
  (String.of_list lst, count)

let count_groups s =
  let lst = String.to_list s in
  let rec count_loop acc_count level lst =
    match lst with
    | [] -> acc_count
    | a :: rest when Char.equal a '{' -> count_loop acc_count (level + 1) rest
    | a :: rest when Char.equal a '}' ->
        count_loop (acc_count + level) (level - 1) rest
    | _ :: rest -> count_loop acc_count level rest
  in
  count_loop 0 0 lst

let solve_p1 s =
  let s1 = remove_cancelled s in
  let s2, _ = remove_garbage s1 in
  count_groups s2

let solve_p2 s =
  let s1 = remove_cancelled s in
  let _, count = remove_garbage s1 in
  count

let result_p1 = solve_p1 aoc_input
let result_p2 = solve_p2 aoc_input

(* 5865 too low *)
