open Core
open Re

let is_example = false
let filename = "lib/day12/input.txt"
let aoc_input = In_channel.read_lines filename

let regex =
  compile
    (seq
       [
         group (rep1 digit);
         (* First number *)
         str " <-> ";
         (* Literal " <-> " *)
         group (rep1 (alt [ digit; seq [ str ", "; digit ] ]))
         (* List of numbers *);
       ])

let parse_line line =
  match exec_opt regex line with
  | Some groups ->
      let id = Re.Group.get groups 1 |> Int.of_string in
      let neighbors_str = Re.Group.get groups 2 in
      let neighbors =
        String.split neighbors_str ~on:','
        |> List.map ~f:(fun s -> String.strip s |> Int.of_string)
      in
      Some (id, neighbors)
  | None -> None

let make_node_set_from line =
  match parse_line line with
  | None -> failwith "Input problem"
  | Some (id, neighbours) ->
      let node_set = Set.add (Set.empty (module Int)) id in
      List.fold neighbours ~init:node_set ~f:(fun set el -> Set.add set el)

(* let solve_p1 () = 
let rec loop acc lines=
  match lines with 
  | [] -> acc
  | line :: rest ->
    let node_set = make_node_set_from line in
    let acc = 




let result_p1 = 0
let result_p2 = 0 *)
