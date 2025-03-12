open Core

let is_example = false

let filename =
  if is_example then "lib/day20/example.txt" else "lib/day20/input.txt"

type vector_t = { x : int; y : int; z : int }
type particle_t = { pos : vector_t; vel : vector_t; acc : vector_t }

let vec_eq vec1 vec2 = vec1.x = vec2.x && vec1.y = vec2.y && vec1.z = vec2.z

let particle_of_line line =
  Scanf.sscanf line "p=< %d,%d,%d>, v=< %d,%d,%d>, a=< %d,%d,%d>"
    (fun px py pz vx vy vz ax ay az ->
      {
        pos = { x = px; y = py; z = pz };
        vel = { x = vx; y = vy; z = vz };
        acc = { x = ax; y = ay; z = az };
      })

let aoc_input = In_channel.read_lines filename
let manh part = abs part.x + abs part.y + abs part.z
let particles = List.map aoc_input ~f:particle_of_line

let tick particles =
  List.map particles ~f:(fun p ->
      let vel =
        { x = p.vel.x + p.acc.x; y = p.vel.y + p.acc.y; z = p.vel.z + p.acc.z }
      in
      let pos =
        { x = p.pos.x + vel.x; y = p.pos.y + vel.y; z = p.pos.z + vel.z }
      in
      { pos; vel; acc = p.acc })

let get_lowest_acc particles =
  let accels = List.mapi particles ~f:(fun i part -> (i, manh part.acc)) in
  let sorted =
    List.sort accels ~compare:(fun a b -> Int.compare (snd a) (snd b))
  in
  fst (List.hd_exn sorted)

let result_p1 = get_lowest_acc particles

let remove_colliding particles =
  let rec loop acc particles =
    match particles with
    | [] -> acc
    | hd :: rest ->
        if List.exists rest ~f:(fun part -> vec_eq hd.pos part.pos) then
          loop acc
            (List.filter rest ~f:(fun part -> not (vec_eq part.pos hd.pos)))
        else loop (hd :: acc) rest
  in
  loop [] particles

let solve_p2 particles =
  let rec loop particles last_count no_change_count =
    if no_change_count > 100 then last_count
    else
      let particles = tick particles in
      let particles = remove_colliding particles in
      let count = List.length particles in
      if count <> last_count then loop particles count 0
      else loop particles last_count (no_change_count + 1)
  in
  loop particles (List.length particles) 0

let result_p2 = solve_p2 particles
