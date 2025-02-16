open Core

let file_name = "lib/day1/input.txt"
let aoc_input = In_channel.read_all file_name

let make_array s =
  String.to_list s
  |> List.map ~f:(fun c -> int_of_char c - int_of_char '0')
  |> List.to_array

let rec match_next_digit acc = function
  | [] -> acc
  | _ :: [] -> acc
  | a :: b :: tail ->
      if a = b then match_next_digit (acc + a) (b :: tail)
      else match_next_digit acc (b :: tail)

let get_dig_val c = int_of_char c - int_of_char '0'
let lst = String.to_list aoc_input |> List.map ~f:get_dig_val
let res = match_next_digit 0 lst

let result_p1 =
  if List.hd_exn lst = List.last_exn lst then res + List.hd_exn lst else res

let match_opposite_digit s =
  let dig_arr = make_array s in
  let l = Array.length dig_arr in
  let sum = ref 0 in
  for idx = 0 to l - 1 do
    let opp_idx = (idx + (l / 2)) mod l in
    let d1 = dig_arr.(idx) in
    sum := !sum + if d1 = dig_arr.(opp_idx) then d1 else 0
  done;
  !sum

let result_p2 = match_opposite_digit aoc_input
