open Core

let is_example = false

let filename, list_size =
  if is_example then ("lib/day10/example.txt", 5)
  else ("lib/day10/input.txt", 256)

let aoc_input = In_channel.read_all filename

let parse_input s =
  String.split s ~on:',' |> List.map ~f:String.strip
  |> List.map ~f:int_of_string

let l_seq = parse_input aoc_input
let parse_input_p2 s = String.to_list s |> List.map ~f:int_of_char
let e_seq = [ 17; 31; 73; 47; 23 ]
let l_seq_p2 = parse_input_p2 aoc_input @ e_seq
let n_arr = Array.init 256 ~f:(fun i -> i)

let print_arr arr =
  Array.iter arr ~f:(fun el -> Printf.printf "%d," el);
  Printf.printf "\n"

let solve_p1 l_seq n_arr =
  let arr_len = Array.length n_arr in
  let rec loop skip_size curr_idx l_seq =
    match l_seq with
    | [] -> n_arr
    | move_len :: rest ->
        let rec rev_loop idx cnt =
          if cnt = 0 then ()
          else
            let i1 = (curr_idx + idx) mod arr_len in
            let i2 = (curr_idx + move_len - idx - 1) % arr_len in
            let v1 = Array.get n_arr i1 in
            let v2 = Array.get n_arr i2 in
            Array.set n_arr i1 v2;
            Array.set n_arr i2 v1;
            rev_loop ((idx + 1) mod arr_len) (cnt - 1)
        in
        let _ = rev_loop 0 (move_len / 2) in
        loop (skip_size + 1)
          ((curr_idx + move_len + skip_size) mod arr_len)
          rest
  in

  loop 0 0 l_seq

let p1_arr = solve_p1 l_seq n_arr
let result_p1 = Array.get p1_arr 0 * Array.get p1_arr 1
let test_arr = Array.init 5 ~f:Fn.id
let test_seq = [ 3; 4; 1; 5 ]
let result_p2 = 0
