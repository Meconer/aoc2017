open Core

let is_example = false

let filename =
  if is_example then "lib/day7/example.txt" else "lib/day7/input.txt"

type node_t = { name : string; weight : int; subs : string list option }

let aoc_input = In_channel.read_lines filename

let pattern =
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

let has_parent_set = ref (Set.empty (module String))
let node_names = ref (Set.empty (module String))

let () =
  List.iter aoc_input ~f:(fun line ->
      let node_name, _, subs = Option.value_exn (parse_line line) in
      node_names := Set.add !node_names node_name;
      match subs with
      | None -> ()
      | Some subs ->
          List.iter subs ~f:(fun s ->
              has_parent_set := Set.add !has_parent_set s))

let root_node = Set.diff !node_names !has_parent_set
let result_p1 = List.hd_exn (Set.to_list root_node)
let result_p2 = ""
