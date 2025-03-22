open Core

let is_example = false

let filename =
  if is_example then "lib/day22/example.txt" else "lib/day22/input.txt"

type dir_t = Up | Down | Left | Right

let char_of_dir dir =
  match dir with Up -> '^' | Down -> 'v' | Left -> '<' | Right -> '>'

module Pos = struct
  module T = struct
    type t = { x : int; y : int }

    let compare t1 t2 =
      let cmp = Int.compare t1.x t2.x in
      if cmp <> 0 then cmp else Int.compare t1.y t2.y

    let sexp_of_t t : Sexp.t = List [ Int.sexp_of_t t.x; Int.sexp_of_t t.y ]
    let hash t = Hashtbl.hash (t.x, t.y)
    let equal t1 t2 = compare t1 t2 = 0

    let move pos dir =
      match dir with
      | Up -> { x = pos.x; y = pos.y - 1 }
      | Down -> { x = pos.x; y = pos.y + 1 }
      | Left -> { x = pos.x - 1; y = pos.y }
      | Right -> { x = pos.x + 1; y = pos.y }
  end

  include T
  include Comparator.Make (T)
end

let aoc_input = In_channel.read_lines filename

let build_grid lines =
  let height = List.length aoc_input in
  let width = String.length (List.hd_exn aoc_input) in
  let start_point = Pos.{ x = width / 2; y = height / 2 } in

  let viruses = Set.empty (module Pos) in
  let rec loop acc lines line_no =
    match lines with
    | [] -> acc
    | line :: rest ->
        let acc =
          String.foldi ~init:acc line ~f:(fun idx acc ch ->
              if Char.equal ch '#' then Set.add acc Pos.{ x = idx; y = line_no }
              else acc)
        in
        loop acc rest (line_no + 1)
  in
  let viruses = loop viruses lines 0 in
  (viruses, start_point)

let is_infected grid pos = Set.mem grid pos
let infect grid pos = Set.add grid pos
let clean grid pos = Set.remove grid pos

let turn_left dir =
  match dir with Up -> Left | Down -> Right | Right -> Up | Left -> Down

let turn_right dir =
  match dir with Up -> Right | Down -> Left | Right -> Down | Left -> Up

let find_min_max grid =
  let x_min, y_min, x_max, y_max = (ref 0, ref 0, ref 0, ref 0) in
  Set.iter grid ~f:(fun pos ->
      x_min := min !x_min Pos.(pos.x);
      x_max := max !x_max Pos.(pos.x);
      y_min := min !y_min Pos.(pos.y);
      y_max := max !y_max Pos.(pos.y));
  (!x_min, !x_max, !y_min, !y_max)

let print_grid grid carr_pos =
  let x_min, x_max, y_min, y_max = find_min_max grid in
  printf "xmin %d xmax %d ymin %d ymax %d\n" x_min x_max y_min y_max;
  for y = y_min to y_max do
    for x = x_min to x_max do
      let ch = if is_infected grid Pos.{ x; y } then '#' else '.' in
      let d1, d2 =
        if Pos.equal carr_pos Pos.{ x; y } then ('[', ']') else (' ', ' ')
      in
      printf "%c%c%c" d1 ch d2
    done;
    printf "\n"
  done

let solve_p1 input count =
  let grid, start_point = build_grid input in
  let infect_count = ref 0 in

  let rec iterate grid pos dir count =
    if count = 0 then !infect_count
    else
      let grid', dir =
        if not (is_infected grid pos) then (
          infect_count := !infect_count + 1;
          (infect grid pos, turn_left dir))
        else (clean grid pos, turn_right dir)
      in
      let pos = Pos.move pos dir in
      (* printf "%d, %d %c\n" pos.x pos.y (char_of_dir dir);
      print_grid grid' pos; *)
      iterate grid' pos dir (count - 1)
  in
  iterate grid start_point Up count

let result_p1 = solve_p1 aoc_input 10000
let result_p2 = 0
