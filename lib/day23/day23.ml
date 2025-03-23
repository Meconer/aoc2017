open Core

let is_example = false

let filename =
  if is_example then "lib/day23/example.txt" else "lib/day23/input.txt"

type instr_t = Set | Sub | Add | Mul | Jnz
type arg = Reg of char | Val of int
type instr = { instr : instr_t; arg1 : arg; arg2 : arg option }

let string_of_instr instr =
  match instr.instr with
  | Set -> "set"
  | Sub -> "sub"
  | Add -> "add"
  | Mul -> "mul"
  | Jnz -> "jgz"

let registers = Hashtbl.create (module Char)
let reg_names = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]
let () = List.iter reg_names ~f:(fun r -> Hashtbl.set registers ~key:r ~data:0)

let get_arg arg =
  match arg with
  | Some (Reg r) -> Hashtbl.find_or_add registers r ~default:(fun () -> 0)
  | Some (Val v) -> v
  | None -> failwith "Should be a second arg here"

let set_arg arg value =
  match arg with
  | Reg r -> Hashtbl.set registers ~key:r ~data:value
  | Val _ -> failwith "Cannot set value"

let get_val argstr =
  let val_opt = int_of_string_opt argstr in
  match val_opt with
  | Some n -> n
  | None ->
      let reg = String.get argstr 0 in
      Hashtbl.find_or_add registers reg ~default:(fun () -> 0)

let parse_instr line =
  printf "PI: %s" line;
  let parts = String.split line ~on:' ' in
  let instr =
    match List.hd_exn parts with
    | "set" -> Set
    | "sub" -> Sub
    | "add" -> Add
    | "mul" -> Mul
    | "jnz" -> Jnz
    | _ -> failwith "Invalid instruction"
  in
  let arg1_str = List.nth_exn parts 1 in
  let arg1 =
    match int_of_string_opt arg1_str with
    | None -> Reg (String.get arg1_str 0)
    | Some n -> Val n
  in

  let arg2_opt =
    if List.length parts = 3 then Some (List.nth_exn parts 2) else None
  in

  let arg2 =
    match arg2_opt with
    | None -> None
    | Some arg -> (
        match int_of_string_opt arg with
        | Some n -> Some (Val n)
        | None ->
            let r = String.get arg 0 in
            Some (Reg r))
  in
  { instr; arg1; arg2 }

let aoc_input = In_channel.read_lines filename

let do_set instr =
  match instr.arg1 with
  | Reg reg -> Hashtbl.set registers ~key:reg ~data:(get_arg instr.arg2)
  | Val _ -> failwith "Cannot set a value"

let do_add instr =
  let reg1 =
    match instr.arg1 with
    | Reg reg -> reg
    | Val _ -> failwith "Must be a register"
  in
  let curr_val = Hashtbl.find_exn registers reg1 in
  Hashtbl.set registers ~key:reg1 ~data:(curr_val + get_arg instr.arg2)

let do_sub instr =
  let reg1 =
    match instr.arg1 with
    | Reg reg -> reg
    | Val _ -> failwith "Must be a register"
  in
  let curr_val = Hashtbl.find_exn registers reg1 in
  Hashtbl.set registers ~key:reg1 ~data:(curr_val - get_arg instr.arg2)

let mul_counter = ref 0

let do_mul instr =
  mul_counter := !mul_counter + 1;
  let reg1 =
    match instr.arg1 with
    | Reg reg -> reg
    | Val _ -> failwith "Must be a register"
  in
  let curr_val = Hashtbl.find_exn registers reg1 in
  Hashtbl.set registers ~key:reg1 ~data:(curr_val * get_arg instr.arg2)

let do_jnz instr =
  let curr_val, reg =
    match instr.arg1 with
    | Reg reg -> (Hashtbl.find_exn registers reg, reg)
    | Val v -> (v, '!')
  in
  Printf.printf "jnz %c (%d)\n" reg curr_val;
  if curr_val <> 0 then None else Some (get_arg instr.arg2)

let do_instr instr =
  match instr.instr with
  | Set ->
      do_set instr;
      1
  | Add ->
      do_add instr;
      1
  | Sub ->
      do_sub instr;
      1
  | Mul ->
      do_mul instr;
      1
  | Jnz -> (
      let res = do_jnz instr in
      match res with
      | None -> 1
      | Some v ->
          (* Printf.printf "jnz ret %d\n" v; *)
          v)

let instructions = List.map aoc_input ~f:parse_instr |> Array.of_list

let print_regs () =
  List.iter reg_names ~f:(fun reg ->
      printf "%c: %d | " reg (Hashtbl.find_exn registers reg));
  printf "\n";
  ()

let solve_p1 () =
  let rec loop ip =
    if ip < 0 || ip >= Array.length instructions then ()
    else
      let instr = instructions.(ip) in
      Printf.printf "ip: %d, %s\n" ip (string_of_instr instr);
      print_regs ();
      let res = do_instr instr in
      loop (ip + res)
  in
  loop 0

let result_p1 = 0
let result_p2 = 0
