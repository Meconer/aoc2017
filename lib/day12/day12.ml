open Core
open Re

let is_example = false
let filename = "lib/day12/input.txt"
let aoc_input = In_channel.read_lines filename

let parse_line s =
  let n1, conns = Scanf.sscanf s "%d <-> %s" (fun n s -> (n, s)) in
  (n1, conns)

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

let () =
  let test_lines =
    [
      "0 <-> 412, 480, 777, 1453";
      "1 <-> 23";
      "12 <-> 99, 200, 4500";
      "5 <-> 8, 9";
      "Invalid line";
    ]
  in

  List.iter test_lines ~f:(fun line ->
      match parse_line line with
      | Some (id, neighbors) ->
          printf "ID: %d, Neighbors: %s\n" id
            (String.concat ~sep:", " (List.map ~f:Int.to_string neighbors))
      | None -> printf "No match for: %s\n" line)

let result_p1 = 0
let result_p2 = 0
