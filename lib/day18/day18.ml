open Core

let is_example = true

let filename =
  if is_example then "lib/day18/example.txt" else "lib/day18/input.txt"

type instr_t = Snd | Set | Add | Mul | Mod | Rcv | Jgz
type arg = Reg of string | Val of int
type instr = { instr : instr_t; arg1 : string; arg2 : arg option }

let registers = ref (Hashtbl.create (module String))

let get_arg arg =
  match arg with
  | Reg r -> Hashtbl.find_or_add !registers r ~default:(fun () -> 0)
  | Val v -> v

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

let do_instr instr =
  match


let instructions = List.map aoc_input ~f:parse_instr
let result_p1 = 0
let result_p2 = 0
