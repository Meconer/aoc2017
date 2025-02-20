open Core

let is_example = false

let filename =
  if is_example then "lib/day8/example.txt" else "lib/day8/input.txt"

let aoc_input = In_channel.read_lines filename

type op_t = Inc | Dec
type cond_t = Gt | Lt | Gte | Lte | Eq | Ne

type expr_t = {
  op_reg : string;
  op : op_t;
  op_val : int;
  cond_reg : string;
  cond : cond_t;
  cond_val : int;
}

let op_of_s s =
  match s with
  | s when String.equal s "inc" -> Inc
  | s when String.equal s "dec" -> Dec
  | _ -> failwith "Illegal operation"

let cond_of_s s =
  match s with
  | s when String.equal s "<" -> Lt
  | s when String.equal s ">" -> Gt
  | s when String.equal s "<=" -> Lte
  | s when String.equal s ">=" -> Gte
  | s when String.equal s "!=" -> Ne
  | s when String.equal s "==" -> Eq
  | _ -> failwith "Illegal condition"

let regs = ref (Map.empty (module String))
let max_val = ref 0

let parse_line line =
  let r =
    Scanf.sscanf line "%s %s %d if %s %s %d"
      (fun op_reg op_s op_val cond_reg cond_s cond_val ->
        let op = op_of_s op_s in
        let cond = cond_of_s cond_s in
        { op_reg; op; op_val; cond_reg; cond; cond_val })
  in
  r

let do_cond v1 cond v2 =
  match cond with
  | Lt -> v1 < v2
  | Gt -> v1 > v2
  | Lte -> v1 <= v2
  | Gte -> v1 >= v2
  | Eq -> v1 = v2
  | Ne -> v1 <> v2

let solve_p1 lines =
  let rec loop lines =
    match lines with
    | [] -> ()
    | line :: rest ->
        Printf.printf "%s\n" line;
        let expr = parse_line line in
        let cond_reg_val =
          match Map.find !regs expr.cond_reg with None -> 0 | Some v -> v
        in
        let cond_res = do_cond cond_reg_val expr.cond expr.cond_val in
        if cond_res then (
          let op_reg_val =
            match Map.find !regs expr.op_reg with None -> 0 | Some v -> v
          in
          let res =
            match expr.op with
            | Inc -> op_reg_val + expr.op_val
            | Dec -> op_reg_val - expr.op_val
          in
          regs := Map.set !regs ~key:expr.op_reg ~data:res;
          if res > !max_val then max_val := res;
          loop rest)
        else loop rest
  in
  let () = loop lines in
  let max_elt =
    Map.fold !regs ~init:0 ~f:(fun ~key:_k ~data:v acc ->
        if v > acc then v else acc)
  in
  max_elt

let result_p1 = solve_p1 aoc_input
let result_p2 = !max_val
