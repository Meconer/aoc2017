open Core

let is_example = false
let example = "0: 3\n1: 2\n4: 4\n6: 4\n" |> String.split_lines
let filename = "lib/day13/input.txt"
let aoc_input = if is_example then example else In_channel.read_lines filename

let parse_line line =
  Scanf.sscanf line "%d: %d" (fun layer_no depth -> (layer_no, depth))

let max_layer, _ = parse_line (List.last_exn aoc_input)
let layer_arr = Array.create ~len:(max_layer + 1) 0

let build_arr layer_arr lines =
  let rec loop lines =
    match lines with
    | [] -> ()
    | line :: rest ->
        let layer_no, depth = parse_line line in
        Array.set layer_arr layer_no depth;
        loop rest
  in
  loop lines

let pos_of_layer_guard layer_no time =
  let depth_of_layer = layer_arr.(layer_no) in
  let cycle_length = 2 * (depth_of_layer - 1) in
  let ofs = time mod cycle_length in
  let half = cycle_length / 2 in
  let pos = if ofs > half then depth_of_layer - (ofs - half + 1) else ofs in
  pos

let () = build_arr layer_arr aoc_input

let solve_p1 () =
  let severity = ref 0 in
  for time = 0 to max_layer do
    if layer_arr.(time) > 0 then
      let layer_guard = pos_of_layer_guard time time in
      (* Printf.printf "Time: %d Pos: %d\n" time layer_guard; *)
      if layer_guard = 0 then severity := !severity + (layer_arr.(time) * time)
      else ()
  done;
  !severity

let result_p1 = solve_p1 ()

let solve_p2 () =
  let rec loop delay =
    let rec inner layer_no =
      if layer_no = max_layer + 1 then true
      else
        let time = layer_no + delay in
        if layer_arr.(layer_no) > 0 then
          let layer_guard = pos_of_layer_guard layer_no time in
          if layer_guard = 0 then false else inner (layer_no + 1)
        else inner (layer_no + 1)
    in
    let found = inner 0 in
    if found then delay else loop (delay + 1)
  in
  let found_delay = loop 0 in
  found_delay

let result_p2 = solve_p2 ()
