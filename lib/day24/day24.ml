open Core

let is_example = false

let filename =
  if is_example then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename

let parse_line line =
  let parts = String.split line ~on:'/' |> List.map ~f:int_of_string in
  let components = (List.hd_exn parts, List.nth_exn parts 1) in
  components

let remove comps comp =
  List.filter comps ~f:(fun c -> fst c <> fst comp || snd c <> snd comp)

let print_comps hdr comps =
  printf "%s\n" hdr;
  List.iter comps ~f:(fun c -> printf "(%d/%d) " (fst c) (snd c));
  printf "\n"

let solve_p1 () =
  let comps = List.map aoc_input ~f:parse_line in
  (* print_comps "start " comps; *)
  (* Out_channel.flush stdout; *)
  (* let _ = In_channel.input_char In_channel.stdin in *)
  let rec loop strength comps ports =
    (* print_comps "comps" comps; *)
    let matching_comps =
      List.filter comps ~f:(fun comp -> fst comp = ports || snd comp = ports)
    in
    (* print_comps "matching" matching_comps; *)
    (* Out_channel.flush stdout; *)
    (* let _ = In_channel.input_char In_channel.stdin in *)

    if List.is_empty matching_comps then strength
    else
      let strengths =
        List.map matching_comps ~f:(fun comp ->
            let strength' = strength + fst comp + snd comp in
            let comps' = remove comps comp in
            let ports' = if fst comp = ports then snd comp else fst comp in
            (* printf "acc %d, ports %d " acc' ports'; *)
            (* print_comps "comps'" comps'; *)
            loop strength' comps' ports')
      in
      match List.max_elt strengths ~compare:Int.compare with
      | None -> 0
      | Some s -> s
  in
  loop 0 comps 0

let solve_p2 () =
  let comps = List.map aoc_input ~f:parse_line in
  let longest = ref 0 in
  let max_strength = ref 0 in
  let rec loop strength length comps ports =
    let matching_comps =
      List.filter comps ~f:(fun comp -> fst comp = ports || snd comp = ports)
    in

    if List.is_empty matching_comps then (strength, length)
    else
      let strengths_lens =
        List.map matching_comps ~f:(fun comp ->
            let strength' = strength + fst comp + snd comp in
            let length' = length + 1 in
            let comps' = remove comps comp in
            let ports' = if fst comp = ports then snd comp else fst comp in
            loop strength' length' comps' ports')
      in
      List.iter strengths_lens ~f:(fun s_l ->
          let s, l = s_l in
          if l > !longest then (
            longest := l;
            max_strength := s);
          if l = !longest then max_strength := max !max_strength s);
      (!max_strength, !longest)
  in
  loop 0 0 comps 0

let result_p1 = solve_p1 ()
let result_p2 = fst (solve_p2 ())
