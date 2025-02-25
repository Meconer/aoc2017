open Core

let is_example = false
let filename = "lib/day11/input.txt"
let aoc_input = In_channel.read_all filename

type dir = N | NE | SE | S | SW | NW
type pos_t = { x : int; y : int }

let dir_of_str s =
  match s with
  | s when String.equal s "n" -> N
  | s when String.equal s "ne" -> NE
  | s when String.equal s "se" -> SE
  | s when String.equal s "s" -> S
  | s when String.equal s "sw" -> SW
  | s when String.equal s "nw" -> NW
  | _ -> failwith "illegal dir string"

let move pos dir =
  match dir with
  | N -> { pos with y = pos.y + 2 }
  | NE -> { x = pos.x + 2; y = pos.y + 1 }
  | SE -> { x = pos.x + 2; y = pos.y - 1 }
  | S -> { pos with y = pos.y - 2 }
  | SW -> { x = pos.x - 2; y = pos.y - 1 }
  | NW -> { x = pos.x - 2; y = pos.y + 1 }

let dist p1 p2 =
  let dx = abs (p1.x - p1.y) in
  let dy = abs (p1.y - p2.y) in
  (dx + dy) / 2

let get_dist_of_moves s =
  let move_list = String.split s ~on:',' |> List.map ~f:dir_of_str in
  let rec loop pos moves =
    match moves with [] -> pos | dir :: rest -> loop (move pos dir) rest
  in
  loop { x = 0; y = 0 } move_list

let result_p1 = 0
let result_p2 = 0
