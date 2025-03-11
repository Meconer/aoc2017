open Core

let is_example = false

let filename =
  if is_example then "lib/day20/example.txt" else "lib/day20/input.txt"

type vector_t = { x : int; y : int; z : int }
type particle_t = { pos : vector_t; vel : vector_t; acc : vector_t }

let particle_of_line line =
  Scanf.sscanf line "p=< %d,%d,%d>, v=< %d,%d,%d>, a=< %d,%d,%d>"
    (fun px py pz vx vy vz ax ay az ->
      {
        pos = { x = px; y = py; z = pz };
        vel = { x = vx; y = vy; z = vz };
        acc = { x = ax; y = ay; z = az };
      })

let aoc_input = In_channel.read_lines filename
let manh_dist p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y) + abs (p1.z - p2.z)

let particles = List.map aoc_input ~f:particle_of_line

let tick particles =
  List.map particles ~f:(fun p -> let vel = {x= p.vel.x + p.acc.x; y=p.vel.y + p.acc.y; z=p.vel.z + p.acc.z} in
  let pos = {x= p.pos.x + vel.x; y= p.pos.y + vel.y; z=p.pos.z + vel.z} in
  {}
  )

let result_p1, result_p2 = (0, 0)
