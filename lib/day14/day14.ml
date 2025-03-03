open Core

let is_example = true
let aoc_input = if is_example then "flqrgnkx" else "nbysizxe"
let parse_input s = String.to_list s |> List.map ~f:int_of_char
let n_arr = Array.init 256 ~f:(fun i -> i)

let print_arr arr =
  Array.iter arr ~f:(fun el -> Printf.printf "%d," el);
  Printf.printf "\n"

let calc_knot l_seq n_arr skip_size start_idx =
  let arr_len = Array.length n_arr in
  let rec loop skip_size curr_idx l_seq =
    match l_seq with
    | [] -> (n_arr, skip_size, curr_idx)
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
  loop skip_size start_idx l_seq

let result_p1 = 0

let calc_sparse_hash l_seq_p2 =
  let n_arr = Array.init 256 ~f:(fun i -> i) in
  let rec loop skip_size curr_idx count n_arr =
    if count = 0 then n_arr
    else
      let n_arr, skip_size, curr_idx =
        calc_knot l_seq_p2 n_arr skip_size curr_idx
      in
      loop skip_size curr_idx (count - 1) n_arr
  in
  loop 0 0 64 n_arr

let hex_of_int n = Printf.sprintf "%02x" n
let combine lst = List.fold lst ~init:0 ~f:(fun acc el -> acc lxor el)

let calc_dense_hash sparse_hash =
  let sp_hash_list = Array.to_list sparse_hash in
  let rec loop acc lst =
    match lst with
    | [] -> List.rev acc
    | lst ->
        let w_list = List.take lst 16 in
        let r_list = List.drop lst 16 in
        let hex = combine w_list in
        loop (hex :: acc) r_list
  in
  let hex_list = loop [] sp_hash_list in
  List.fold hex_list ~init:"" ~f:(fun acc n -> acc ^ hex_of_int n)

let e_seq = [ 17; 31; 73; 47; 23 ]

let get_hash s =
  let l_seq_p2 = parse_input s @ e_seq in
  let sparse_hash = calc_sparse_hash l_seq_p2 in
  let dense_hash = calc_dense_hash sparse_hash in
  dense_hash

let bit_count c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 1
  | '3' -> 2
  | '4' -> 1
  | '5' -> 2
  | '6' -> 2
  | '7' -> 3
  | '8' -> 1
  | '9' -> 2
  | 'a' -> 2
  | 'b' -> 3
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 3
  | 'f' -> 4
  | _ -> failwith "Illegal hex char"

let result_p2 = 0
