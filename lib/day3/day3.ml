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

type coord = {x:int;y:int}
type cell = {pos:coord;value:int}
type direction = Up|Left|Down|Right

let get_pos pos dir =
  match dir with
  | Up -> {pos with y = pos.y-1}
  | Down -> {pos with y = pos.y+1}
  | Left -> {pos with x = pos.x-1}
  | Right -> {pos with x = pos.x-1}

let get_turn_pos pos direction =
  let turn_dir = match direction with 
  | Right -> Up
  | Up -> Left
  | Left -> Down
  | Down -> Right in
  let turn_pos = get_pos pos turn_dir in
  turn_pos


let build_cells limit =
  let rec loop acc pos direction =
    let turn_pos = get_turn_pos pos direction in
    let tp_cell_opt = find_cell_at acc turn_pos in


let result_p2 = 0
