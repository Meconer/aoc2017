open Core

let is_example = false

let filename, list_size =
  if is_example then ("lib/day10/example.txt", 5)
  else ("lib/day10/input.txt", 256)

let aoc_input = In_channel.read_all filename

let parse_input s =
  String.split s ~on:',' |> List.map ~f:String.strip
  |> List.map ~f:int_of_string

let pr_list hd l =
  Printf.printf "%s : " hd;
  List.iter l ~f:(Printf.printf "%d, ");
  Printf.printf "\n";
  ()

let solve_p1 l_seq lst =
  let rec loop l_seq skip_size skip_sum move_sum lst =
    Printf.printf "Skip_size %d\n" skip_size;
    match l_seq with
    | [] -> (lst, skip_sum, move_sum)
    | move_len :: rest ->
        let part1 = List.sub lst ~pos:0 ~len:move_len in
        pr_list "part1" part1;
        let part2 =
          List.sub lst ~pos:move_len ~len:(List.length lst - move_len)
        in
        pr_list "part2" part2;
        let new_list = List.rev part1 @ part2 in
        pr_list "new_list" new_list;
        let skip_length = (move_len + skip_size) mod List.length new_list in
        let list_after_skip =
          List.sub new_list ~pos:skip_length
            ~len:(List.length new_list - skip_length)
          @ List.sub new_list ~pos:0 ~len:skip_length
        in
        pr_list "LAS" list_after_skip;
        loop rest (skip_size + 1) (skip_sum + skip_size) (move_sum + move_len)
          list_after_skip
  in
  loop l_seq 0 0 0 lst

let l_seq = parse_input aoc_input
let lst = List.init list_size ~f:(fun i -> i)
let lst, skip_sum, move_sum = solve_p1 l_seq lst
let total_moves = (skip_sum + move_sum) mod List.length lst
let start = List.length lst - total_moves

let unmoved_lst =
  List.sub lst ~pos:start ~len:(List.length lst - start)
  @ List.sub lst ~pos:0 ~len:start

let result_p1 = List.hd_exn unmoved_lst * List.nth_exn unmoved_lst 1
let parse_input_p2 s = String.to_list s |> List.map ~f:int_of_char
let e_seq = [ 17; 31; 73; 47; 23 ]
let l_seq_p2 = parse_input_p2 aoc_input @ e_seq
let n_arr = Array.init 256 ~f:(fun i -> i)

let solve_p2 l_seq n_arr =
  let rec loop skip_size curr_idx l_seq =
    Printf.printf "Skip_size %d\n" skip_size;
    match l_seq with
    | [] -> n_arr
    | move_len :: rest ->
        let rec rev_loop idx cnt =
          if cnt = 0 then ()
          else
            let i1 = idx mod 256 in
            let i2 = (move_len - idx) mod 256 in
            let v1 = Array.get n_arr i1 in
            let v2 = Array.get n_arr i2 in
            Array.set n_arr i1 v2;
            Array.set n_arr i2 v1;
            rev_loop ((idx + 1) mod 256) (cnt - 1)
        in
        let _ = rev_loop curr_idx (move_len / 2) in
        loop ((skip_size + 1) mod 256) ((curr_idx + move_len) mod 256) rest
  in

  loop 0 0 l_seq

let test_arr = Array.init 5 ~f:Fn.id
let test_seq = [ 3; 4; 1; 5 ]
let result_p2 = 0
