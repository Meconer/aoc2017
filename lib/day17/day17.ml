open Core

let is_example = false
let fw_jump = if is_example then 3 else 343

let solve_p1 () =
  let circular_array = Array.create ~len:2018 0 in
  let current_size = ref 1 in
  let current_pos = ref 0 in
  let rec loop i =
    if i = 2018 then ()
    else (
      current_pos := ((!current_pos + fw_jump) mod !current_size) + 1;
      for j = !current_size downto !current_pos + 1 do
        circular_array.(j) <- circular_array.(j - 1)
      done;
      circular_array.(!current_pos) <- i;
      incr current_size;
      loop (i + 1))
  in
  loop 1;
  circular_array.(!current_pos + 1)

let solve_p2 () =
  let current_size = ref 1 in
  let val_at_pos_1 = ref 0 in
  let current_pos = ref 0 in
  let rec loop i =
    if i = 50_000_000 then ()
    else (
      current_pos := ((!current_pos + fw_jump) mod !current_size) + 1;
      if !current_pos = 1 then val_at_pos_1 := i;
      incr current_size;
      loop (i + 1))
  in
  loop 1;
  !val_at_pos_1

let result_p1 = solve_p1 ()
let result_p2 = solve_p2 ()
