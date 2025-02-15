open Core

let file_name = "lib/day1/input.txt"
let aoc_input = In_channel.read_all file_name

let lst = String.to_list aoc_input |> List.map ~f:(fun c -> int_of_char c - int_of_char '0')

let rec loop acc =function 
  | [] -> acc
  | _::[] -> acc
  | a::b::tail -> if a = b then loop (acc+a) (b::tail) else loop acc (b::tail)

let res = loop 0 lst
let res = if (List.hd_exn lst = List.last_exn lst) then res + List.hd_exn lst else res

let result_p1=res