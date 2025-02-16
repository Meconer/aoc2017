let cell_no = 361527

let numbers_straight limit start_diff =
  let rec loop n diff =
    if n > limit then n
    else if n = 1 then loop (n + diff) (diff + 8)
    else
      let n = n + diff in
      Printf.printf "%d\n" n;
      loop n (diff + 8)
  in
  loop 1 start_diff

let result_p1 = 0
let result_p2 = 0
