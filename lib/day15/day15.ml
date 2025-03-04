let is_example = false
let aoc_input = if is_example then (65, 8921) else (618, 814)

let gen_val_A prev_val =
  let factor = 16807 in
  let divisor = 2147483647 in
  prev_val * factor mod divisor

let rec gen_val_A_p2 prev_val =
  let factor = 16807 in
  let divisor = 2147483647 in
  let next_val = prev_val * factor mod divisor in
  if next_val mod 4 = 0 then next_val else gen_val_A_p2 next_val

let gen_val_B prev_val =
  let factor = 48271 in
  let divisor = 2147483647 in
  prev_val * factor mod divisor

let rec gen_val_B_p2 prev_val =
  let factor = 48271 in
  let divisor = 2147483647 in
  let next_val = prev_val * factor mod divisor in
  if next_val mod 8 = 0 then next_val else gen_val_B_p2 next_val

let solve_p1 () =
  let rec loop acc val_A val_B rounds =
    if rounds = 0 then acc
    else
      let val_A = gen_val_A val_A in
      let val_B = gen_val_B val_B in
      let acc =
        if val_A land 0xFFFF = val_B land 0xFFFF then acc + 1 else acc
      in
      loop acc val_A val_B (rounds - 1)
  in
  loop 0 (fst aoc_input) (snd aoc_input) 40000000

let solve_p2 () =
  let rec loop acc val_A val_B rounds =
    if rounds = 0 then acc
    else
      let val_A = gen_val_A_p2 val_A in
      let val_B = gen_val_B_p2 val_B in
      let acc =
        if val_A land 0xFFFF = val_B land 0xFFFF then acc + 1 else acc
      in
      loop acc val_A val_B (rounds - 1)
  in
  loop 0 (fst aoc_input) (snd aoc_input) 5000000

let result_p1 = solve_p1 ()
let result_p2 = solve_p2 ()
