open Core

let input_no = 361527

(*
   Gets the numbers by going straight from the center (1 pos)
   Use start_diff = 1 for going right. 3 for going up, 5 for
   left and 7 for down
*)
let numbers_straight limit start_diff =
  let rec loop n diff dist last =
    if n > limit then last
    else
      let last = (n, dist) in
      let n = n + diff in
      loop n (diff + 8) (dist + 1) last
  in
  loop 1 start_diff 0 (1, 0)

(*
   By trying my input I found out that
   I can get there by going right 301 steps and then
     the rest upwards
*)

let top_number, dist = numbers_straight input_no 1
let result_p1 = dist + input_no - top_number

type coord = { x : int; y : int }
type cell = { pos : coord; value : int }

type direction =
  | North
  | West
  | South
  | East
  | Northwest
  | Northeast
  | Southwest
  | Southeast

let get_pos pos dir =
  match dir with
  | North -> { pos with y = pos.y - 1 }
  | South -> { pos with y = pos.y + 1 }
  | West -> { pos with x = pos.x - 1 }
  | East -> { pos with x = pos.x - 1 }
  | Northwest -> { x = pos.x - 1; y = pos.y - 1 }
  | Northeast -> { x = pos.x + 1; y = pos.y - 1 }
  | Southwest -> { x = pos.x - 1; y = pos.y + 1 }
  | Southeast -> { x = pos.x + 1; y = pos.y + 1 }

let turn direction =
  match direction with
  | East -> North
  | North -> West
  | West -> South
  | South -> East
  | _ -> failwith "Illegal turn"

let get_turn_pos pos direction =
  let turn_dir = turn direction in
  let turn_pos = get_pos pos turn_dir in
  turn_pos

let find_cell_at lst pos =
  let rec loop lst =
    match lst with
    | [] -> None
    | el :: rest ->
        if pos.x = el.pos.x && pos.y = el.pos.y then Some el else loop rest
  in
  loop lst

let all_directions =
  [ North; Northeast; East; Southeast; South; Southwest; West; Northwest ]

let add_neighbours cells pos =
  let neighbour_cells =
    List.map all_directions ~f:(fun dir -> get_pos pos dir)
    |> List.map ~f:(fun npos -> find_cell_at cells npos)
  in
  List.fold neighbour_cells ~init:0 ~f:(fun acc cell ->
      match cell with None -> acc | Some cell -> acc + cell.value)

let build_cells limit =
  let rec loop acc npos direction =
    let value = add_neighbours acc npos in
    if value > limit then (List.rev acc, value)
    else
      let cell = { pos = npos; value } in
      let n_acc = cell :: acc in

      (* Find out if we can turn *)

      (* Get the position we would be in if we turn here *)
      let turn_pos = get_turn_pos npos direction in
      let tp_cell_opt = find_cell_at acc turn_pos in
      match tp_cell_opt with
      | None ->
          (* Nothing here so we turn *) loop n_acc turn_pos (turn direction)
      | Some _ ->
          (* There was a cell here so we continue same direction *)
          loop n_acc (get_pos npos direction) direction
  in
  loop [ { pos = { x = 0; y = 0 }; value = 1 } ] { x = 1; y = 0 } East

let result_p2 = 0
