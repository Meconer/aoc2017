open Core

let is_example = false

let filename =
  if is_example then "lib/day18p2/example.txt" else "lib/day18/input.txt"

type instr_t = Snd | Set | Add | Mul | Mod | Rcv | Jgz
type arg = Reg of string | Val of int
type instr = { instr : instr_t; arg1 : string; arg2 : arg option }

let string_of_instr instr =
  match instr.instr with
  | Snd -> "snd"
  | Set -> "set"
  | Add -> "add"
  | Mul -> "mul"
  | Jgz -> "jgz"
  | Rcv -> "rcv"
  | Mod -> "mod"

let registers = Array.create ~len:2 (Hashtbl.create (module String))
let _ = Hashtbl.set registers.(0) ~key:"p" ~data:0
let _ = Hashtbl.set registers.(1) ~key:"p" ~data:1
let latest_sound = ref 0

let get_arg arg cpu =
  match arg with
  | Some (Reg r) -> Hashtbl.find_or_add registers.(cpu) r ~default:(fun () -> 0)
  | Some (Val v) -> v
  | None -> failwith "Should be a second arg here"

let set_arg arg value cpu =
  match arg with
  | Reg r -> Hashtbl.set registers.(cpu) ~key:r ~data:value
  | Val _ -> failwith "Cannot set value"

let parse_instr line =
  let parts = String.split line ~on:' ' in
  let instr =
    match List.hd_exn parts with
    | "snd" -> Snd
    | "set" -> Set
    | "add" -> Add
    | "mul" -> Mul
    | "mod" -> Mod
    | "rcv" -> Rcv
    | "jgz" -> Jgz
    | _ -> failwith "Invalid instruction"
  in
  let arg1 = List.nth_exn parts 1 in
  let arg2_opt =
    if List.length parts = 3 then Some (List.nth_exn parts 2) else None
  in

  let arg2 =
    match arg2_opt with
    | None -> None
    | Some arg -> (
        match int_of_string_opt arg with
        | Some n -> Some (Val n)
        | None -> Some (Reg arg))
  in
  { instr; arg1; arg2 }

let aoc_input = In_channel.read_lines filename

let do_set instr cpu =
  Hashtbl.set registers.(cpu) ~key:instr.arg1 ~data:(get_arg instr.arg2 cpu)

let do_add instr cpu =
  let curr_val =
    Hashtbl.find_or_add registers.(cpu) instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set registers.(cpu) ~key:instr.arg1
    ~data:(curr_val + get_arg instr.arg2 cpu)

let do_mul instr cpu =
  let curr_val =
    Hashtbl.find_or_add registers.(cpu) instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set registers.(cpu) ~key:instr.arg1
    ~data:(curr_val * get_arg instr.arg2 cpu)

let queue = Array.create ~len:2 ([] : int list)

let do_snd instr cpu =
  let receiver = 1 - cpu in
  let send_val = Hashtbl.find_exn registers.(cpu) instr.arg1 in
  queue.(receiver) <- queue.(receiver) @ [ send_val ]

let do_mod instr cpu =
  let curr_val =
    Hashtbl.find_or_add registers.(cpu) instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set registers.(cpu) ~key:instr.arg1
    ~data:(curr_val mod get_arg instr.arg2 cpu)

let do_rcv instr cpu =
  if List.is_empty queue.(cpu) then None
  else
    let received_val = List.hd_exn queue.(cpu) in
    Hashtbl.set registers.(cpu) ~key:instr.arg1 ~data:received_val;
    Some received_val

let do_jgz instr cpu =
  let curr_val =
    Hashtbl.find_or_add registers.(cpu) instr.arg1 ~default:(fun _ -> 0)
  in
  if curr_val = 0 then None else Some (get_arg instr.arg2 cpu)

let do_instr instr cpu =
  match instr.instr with
  | Set ->
      do_set instr cpu;
      1
  | Add ->
      do_add instr cpu;
      1
  | Mul ->
      do_mul instr cpu;
      1
  | Mod ->
      do_mod instr cpu;
      1
  | Rcv -> (
      let res = do_rcv instr cpu in
      match res with None -> 0 | Some _ -> 1)
  | Jgz -> (
      let res = do_jgz instr cpu in
      match res with None -> 1 | Some v -> v)
  | Snd ->
      do_snd instr cpu;
      1

let instructions = List.map aoc_input ~f:parse_instr |> Array.of_list

let solve_p2 () =
  let rec loop ip =
    let instr = instructions.(ip) in
    let res = do_instr instr in
    match res with
    | None -> loop (ip + 1)
    | Some ret_val -> (
        match instr.instr with
        | Jgz -> loop (ip + ret_val)
        | Rcv -> if ret_val = 0 then loop (ip + 1) else ret_val
        | _ -> loop (ip + 1))
  in
  loop 0

let result_p2 = solve_p2 ()
