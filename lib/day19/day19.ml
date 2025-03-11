open Core

let is_example = true

let filename =
  if is_example then "lib/day19/example.txt" else "lib/day19/input.txt"

let aoc_input = In_channel.read_lines filename

let grid =
  aoc_input |> List.map ~f:String.to_list |> List.map ~f:Array.of_list
  |> Array.of_list

let printline n =
  for i = 0 to Array.length grid.(0) - 1 do
    Printf.printf "%c" grid.(n).(i)
  done

let printgrid () =
  for i = 0 to Array.length grid - 1 do
    printline i;
    print_endline ""
  done

let start_pos () =
  fst (Option.value_exn (Array.findi grid.(0) ~f:(fun i c -> Char.equal c '|')))

let result_p1 = 0
let result_p2 = 0
