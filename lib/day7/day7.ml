open Core

let is_example = true

let filename =
  if is_example then "lib/day7/example.txt" else "lib/day6/input.txt"

type node_t = { n : string; weight : int; subs : node_t list }

let aoc_input = In_channel.read_lines filename

let pattern =
  let open Re in
  Re.(
    compile
      (seq
         [
           group (rep1 alnum);
           (* Capture the name *)
           str " (";
           group (rep1 digit);
           (* Capture the weight *)
           str ")";
           opt
             (seq
                [
                  (* Optionally match "-> children" *)
                  str " -> ";
                  group (rep1 (alt [ alpha; space; char ',' ]))
                  (* Capture children list *);
                ]);
         ]))

(* Function to parse a line *)
let parse_line line =
  match Re.exec_opt pattern line with
  | Some groups ->
      let name = Re.Group.get groups 1 in
      let weight = int_of_string (Re.Group.get groups 2) in
      let children =
        if Re.Group.test groups 3 then
          Some
            (Re.Group.get groups 3 |> String.split ~on:','
           |> List.map ~f:String.strip)
        else None
      in
      Some (name, weight, children)
  | None -> None

let nodes = ref (Map.empty (module String) )

List.iter aoc_input ~f:


let result_p1 = 0
let result_p2 = 0
