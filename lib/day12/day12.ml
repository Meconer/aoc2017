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

let add_node_set list_of_node_sets node_set_to_add =
  let node_sets = node_set_to_add :: list_of_node_sets in
  let rec loop acc list =
    match list with
    | [] -> acc
    | first_set :: tail -> 
      let rec inner_loop added not_added f_set rest =
        match rest with 
        | [] -> added,not_added
        | set_to_test:: tl2 ->
          if Set.are_disjoint f_set set_to_test then
          inner_loop added (set_to_test::not_added) f_set tl2
          else
          inner_loop (Set.inter added set_to_test) not_added f_set tl2
          in
          
          let added,not_added = inner_loop first_set [] first_set tail 
          
  in
  loop [] node_sets

let solve_p1 () =
  let rec loop acc lines =
    match lines with
    | [] -> acc
    | line :: rest ->
        let node_set = make_node_set_from line in
        let acc = add_node_set acc node_set in
        loop acc rest
  in
  loop [] aoc_input

let result_p1 = 0
let result_p2 = 0
