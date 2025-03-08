open Core

let is_example = true

let filename =
  if is_example then "lib/day18/example.txt" else "lib/day18/input.txt"

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

let registers = ref (Hashtbl.create (module String))
let latest_sound = ref 0

let get_arg arg =
  match arg with
  | Some (Reg r) -> Hashtbl.find_or_add !registers r ~default:(fun () -> 0)
  | Some (Val v) -> v
  | None -> failwith "Should be a second arg here"

let set_arg arg value =
  match arg with
  | Reg r -> Hashtbl.set !registers ~key:r ~data:value
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

let do_set instr =
  Hashtbl.set !registers ~key:instr.arg1 ~data:(get_arg instr.arg2)

let do_add instr =
  let curr_val =
    Hashtbl.find_or_add !registers instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set !registers ~key:instr.arg1 ~data:(curr_val + get_arg instr.arg2)

let do_mul instr =
  let curr_val =
    Hashtbl.find_or_add !registers instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set !registers ~key:instr.arg1 ~data:(curr_val * get_arg instr.arg2)

let do_snd instr = latest_sound := Hashtbl.find_exn !registers instr.arg1

let do_mod instr =
  let curr_val =
    Hashtbl.find_or_add !registers instr.arg1 ~default:(fun _ -> 0)
  in
  Hashtbl.set !registers ~key:instr.arg1 ~data:(curr_val mod get_arg instr.arg2)

let do_rcv instr =
  let curr_val =
    Hashtbl.find_or_add !registers instr.arg1 ~default:(fun _ -> 0)
  in
  if curr_val = 0 then None else Some !latest_sound

let do_jgz instr =
  let curr_val =
    Hashtbl.find_or_add !registers instr.arg1 ~default:(fun _ -> 0)
  in
  if curr_val = 0 then None else Some (get_arg instr.arg2)

let do_instr instr =
  Printf.printf "%s\n" (string_of_instr instr);
  match instr.instr with
  | Set ->
      do_set instr;
      None
  | Add ->
      do_add instr;
      None
  | Mul ->
      do_mul instr;
      None
  | Mod ->
      do_mod instr;
      None
  | Rcv -> do_rcv instr
  | Jgz -> do_jgz instr
  | Snd ->
      do_snd instr;
      None

let instructions = List.map aoc_input ~f:parse_instr |> Array.of_list

let rec solve_p1 () =
  let rec loop ip =
    let instr = instructions.(ip) in
    let res = do_instr instr in
    match res with
    | None -> loop ip + 1
    | Some ret_val -> (
        match instr.instr with
        | Jgz -> loop (ip + ret_val)
        | Rcv -> if ret_val = 0 then loop (ip + 1) else ret_val
        | _ -> loop (ip + 1))
  in
  loop 0

let result_p1 = 0
let result_p2 = 0
