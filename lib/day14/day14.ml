open Core

let is_example = false
let aoc_input = if is_example then "flqrgnkx" else "nbysizxe"
let parse_input s = String.to_list s |> List.map ~f:int_of_char

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

let bit_count_of_c c =
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

let count_bits s =
  String.to_list s |> List.map ~f:bit_count_of_c |> List.fold ~init:0 ~f:( + )

let get_hash_of_row row_no =
  let row_str = aoc_input ^ "-" ^ string_of_int row_no in
  get_hash row_str

let solve_p1 () =
  let rec loop acc row =
    if row = 128 then acc
    else
      let hash_str = get_hash_of_row row in
      let bit_count = count_bits hash_str in
      loop (acc + bit_count) (row + 1)
  in
  loop 0 0

let result_p1 = solve_p1 ()
let bit_array = Array.make_matrix ~dimx:128 ~dimy:128 0

let bits_of_ch ch =
  let n =
    if Char.( > ) ch '9' then int_of_char ch - int_of_char 'a' + 10
    else int_of_char ch - int_of_char '0'
  in
  let bit_mask = ref 8 in

  let arr = [| 0; 0; 0; 0 |] in
  for i = 0 to 3 do
    if n land !bit_mask > 0 then arr.(i) <- 1;
    bit_mask := !bit_mask lsr 1
  done;
  arr

let build_bit_arr_row row str =
  let char_list = String.to_list str in
  let rec loop idx char_list =
    match char_list with
    | [] -> ()
    | ch :: rest ->
        let bits = bits_of_ch ch in
        Array.iteri bits ~f:(fun i el -> bit_array.(row).((idx * 4) + i) <- el);
        loop (idx + 1) rest
  in
  loop 0 char_list

let idx_of_rc row col = (row * 128) + col
let rc_of_idx idx = (idx / 128, idx mod 128)

let get_neighbours row col =
  [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]

let visited = ref (Set.empty (module Int))

let visit_neighbours row col =
  let rec bfs row col =
    if Set.mem !visited (idx_of_rc row col) then ()
    else visited := Set.add !visited (idx_of_rc row col);
    if bit_array.(row).(col) = 0 then ()
    else
      let neighbours =
        get_neighbours row col
        |> List.filter ~f:(fun (r, c) -> r >= 0 && r < 128 && c >= 0 && c < 128)
        |> List.filter ~f:(fun (r, c) -> bit_array.(r).(c) = 1)
        |> List.filter ~f:(fun (r, c) -> not (Set.mem !visited (idx_of_rc r c)))
      in
      List.iter neighbours ~f:(fun nb -> bfs (fst nb) (snd nb))
  in
  bfs row col

let solve_p2 () =
  let rec loop row =
    if row = 128 then ()
    else
      let hash_str = get_hash_of_row row in
      build_bit_arr_row row hash_str;
      loop (row + 1)
  in
  loop 0;
  let count = ref 0 in
  for row = 0 to 127 do
    for col = 0 to 127 do
      if Set.mem !visited (idx_of_rc row col) then ()
      else if bit_array.(row).(col) = 1 then (
        visit_neighbours row col;
        count := !count + 1)
    done
  done;
  !count

let result_p2 = solve_p2 ()
