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
let nodes = ref (Map.empty (module String))

let () =
  List.iter aoc_input ~f:(fun line ->
      let node_name, weight, subs_opt = Option.value_exn (parse_line line) in
      (* Printf.printf "%s (%d)\n" node_name weight; *)
      let node = { name = node_name; weight; subs = subs_opt } in
      nodes := Map.set !nodes ~key:node_name ~data:node);
  ()
(* match subs_opt with
   | None -> ()
   | Some l ->
       Printf.printf "  ->";
       List.iter l ~f:(fun s -> Printf.printf "  %s" s);
       Printf.printf "\n"
       ) *)

let is_balanced sub_list =
  let rec loop lst =
    match lst with
    | [] | _ :: [] -> true
    | a :: b :: rest -> if a <> b then false else loop (b :: rest)
  in
  loop sub_list

let analyze_subs subs sub_weights =
  let min_elt =
    List.foldi sub_weights
      ~init:(List.hd_exn sub_weights, 0)
      ~f:(fun idx (min_el, min_idx) el ->
        if el < min_el then (el, idx) else (min_el, min_idx))
  in
  let max_elt =
    List.foldi sub_weights
      ~init:(List.hd_exn sub_weights, 0)
      ~f:(fun idx (max_el, max_idx) el ->
        if el > max_el then (el, idx) else (max_el, max_idx))
  in
  let no_of_min = List.count sub_weights ~f:(fun el -> el = fst min_elt) in
  let diff_el = if no_of_min = 1 then min_elt else max_elt in
  Printf.printf "Diff: %d at %d\n" (fst diff_el) (snd diff_el);
  Printf.printf "Diff el: %s\n" (List.nth_exn subs (snd diff_el));
  ()

let rec calc_weights node_name : int =
  let node = Map.find_exn !nodes node_name in
  let sub_weight =
    match node.subs with
    | None -> 0
    | Some subs ->
        (* let sub_weights = List.map subs ~f:calc_weights in
           if not (is_balanced sub_weights) then (
             analyze_subs subs sub_weights;
             Printf.printf "Name: %s - " node_name;
             List.iter sub_weights ~f:(fun sw -> Printf.printf "%d, " sw);
             List.iter subs ~f:(fun sub ->
                 Printf.printf "  %s %d " sub (Map.find_exn !nodes sub).weight);
             Printf.printf "#\n"); *)
        List.fold subs ~init:0 ~f:(fun acc n ->
            let sub_weight = calc_weights n in
            acc + sub_weight)
  in
  node.weight + sub_weight

(* Running the calc_weights function with the analyze_subs function within shows that the differing
   element is the ycbgx which's weight should be lowered from 1531 to 1526 *)
let result_p2 = 1526
