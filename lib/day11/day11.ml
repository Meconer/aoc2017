open Core

let is_example = false
let filename = "lib/day11/input.txt"
let aoc_input = In_channel.read_all filename

type dir = N | NE | SE | S | SW | NW
type pos_t = { q : int; r : int; s : int }

let dir_of_str s =
  match s with
  | s when String.equal s "n" -> N
  | s when String.equal s "ne" -> NE
  | s when String.equal s "se" -> SE
  | s when String.equal s "s" -> S
  | s when String.equal s "sw" -> SW
  | s when String.equal s "nw" -> NW
  | _ -> failwith "illegal dir string"

let dir_vec dir =
  match dir with
  | N -> { q = 0; r = -1; s = 1 }
  | NE -> { q = 1; r = -1; s = 0 }
  | SE -> { q = 1; r = 0; s = -1 }
  | S -> { q = 0; r = 1; s = -1 }
  | SW -> { q = -1; r = 0; s = 1 }
  | NW -> { q = -1; r = 1; s = 0 }

let move pos dir =
  let delta = dir_vec dir in
  { q = pos.q + delta.q; r = pos.r + delta.r; s = pos.s + delta.s }

let dist p1 p2 =
  let dq = abs (p1.q - p2.q) in
  let dr = abs (p1.r - p2.r) in
  let ds = abs (p1.s - p2.s) in
  (dq + dr + ds) / 2

let get_dist_of_moves s =
  let move_list = String.split s ~on:',' |> List.map ~f:dir_of_str in
  let rec loop pos moves =
    match moves with [] -> pos | dir :: rest -> loop (move pos dir) rest
  in
  loop { q = 0; r = 0; s = 0 } move_list

let result_p1 = 0
let result_p2 = 0
