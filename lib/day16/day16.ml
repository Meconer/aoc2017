open Core

let is_example = false
let filename = "lib/day16/input.txt"
let aoc_input = In_channel.read_all filename
let no_of_progs = if is_example then 5 else 16

let p_arr =
  Array.init no_of_progs ~f:(fun i -> char_of_int (int_of_char 'a' + i))

let spin n =
  let temp = Array.copy p_arr in
  let part1 = Array.sub temp ~pos:(no_of_progs - n) ~len:n in
  let part2 = Array.sub temp ~pos:0 ~len:(no_of_progs - n) in
  Array.blit ~dst:p_arr ~dst_pos:0 ~src:part1 ~src_pos:0 ~len:n;
  Array.blit ~dst:p_arr ~dst_pos:n ~src:part2 ~src_pos:0 ~len:(no_of_progs - n);
  ()

let exchange i1 i2 =
  let temp = p_arr.(i1) in
  p_arr.(i1) <- p_arr.(i2);
  p_arr.(i2) <- temp;
  ()

let swap c1 c2 =
  let i1 = fst (Array.findi_exn p_arr ~f:(fun _ c -> Char.(c1 = c))) in
  let i2 = fst (Array.findi_exn p_arr ~f:(fun _ c -> Char.(c2 = c))) in
  exchange i1 i2;
  ()

type dance_t = Spin | Exchange | Swap

let parse s =
  match s with
  | s when Char.(s.[0] = 's') ->
      let n = Scanf.sscanf s "s%d" (fun n -> n) in
      (Spin, n, 0, '_', '_')
  | s when Char.(s.[0] = 'x') ->
      let i1, i2 = Scanf.sscanf s "x%d/%d" (fun i1 i2 -> (i1, i2)) in
      (Exchange, i1, i2, '_', '_')
  | s when Char.(s.[0] = 'p') ->
      let c1, c2 = Scanf.sscanf s "p%c/%c" (fun c1 c2 -> (c1, c2)) in
      (Swap, -1, -1, c1, c2)
  | _ -> failwith "Illegal format"

let do_dance dance =
  let dance_type, i1, i2, c1, c2 = dance in
  match dance_type with
  | Spin -> spin i1
  | Swap -> swap c1 c2
  | Exchange -> exchange i1 i2

let solve_p1 () =
  let dance_list = String.split ~on:',' aoc_input |> List.map ~f:parse in
  let rec loop dance_list =
    match dance_list with
    | [] -> ()
    | dance :: rest ->
        do_dance dance;
        loop rest
  in
  loop dance_list;
  String.of_array p_arr

let result_p1 = solve_p1 ()

let solve_p2 () =
  let dance_list = String.split ~on:',' aoc_input |> List.map ~f:parse in
  let rec loop dance_list =
    match dance_list with
    | [] -> ()
    | dance :: rest ->
        do_dance dance;
        loop rest
  in
  let rec find_cycle i =
    loop dance_list;
    (* let s = String.of_array p_arr in *)
    (* Printf.printf "%d : %s\n" i s; *)
    if String.equal (String.of_array p_arr) "abcdefghijklmnop" then i + 1
    else find_cycle (i + 1)
  in
  let cycle = find_cycle 1 in
  (* Printf.printf "Cycle : %d\n" cycle; *)
  let n = 1_000_000_000 mod cycle in
  for _ = 1 to n do
    loop dance_list
  done;
  String.of_array p_arr

let result_p2 = solve_p2 ()
