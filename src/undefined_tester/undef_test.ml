open Mlang

(** Printing utils *)
module Printer = struct
  (** Pretty prints a list of elements followed by a new line.
      If the list is empty, prints nothing. *)
  let pp_list pp fmt l =
    Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt "\n") pp fmt l;
    if l <> [] then Format.fprintf fmt "\n"

  let pp_lit fmt = function
    | Irj_ast.I i -> Format.pp_print_int fmt i
    | F f -> Format.fprintf fmt "%.6f" f

  let pp_var_value fmt ((v, _), (l, _)) = Format.fprintf fmt "%s/%a" v pp_lit l

  let pp_calc_err fmt (s, _) = Format.pp_print_string fmt s

  let pp_rappel _ _ = failwith "TODO"

  let pp_irj_file fmt (irj_file : Irj_ast.irj_file) =
    Format.fprintf fmt
      "#NOM\n\
       %s\n\
       #ENTREES-PRIMITIF\n\
       %a#CONTROLES-PRIMITIF\n\
       %a#RESULTATS-PRIMITIF\n\
       %a"
      irj_file.nom (pp_list pp_var_value) irj_file.prim.entrees
      (pp_list pp_calc_err) irj_file.prim.controles_attendus
      (pp_list pp_var_value) irj_file.prim.resultats_attendus;
    match irj_file.rapp with
    | None ->
        Format.fprintf fmt
          "#ENTREES-CORRECTIF\n#CONTROLES-CORRECTIF\n#RESULTATS-CORRECTIF\n##\n"
    | Some rapp ->
        Format.fprintf fmt
          "#ENTREES-CORRECTIF\n\
           %a#CONTROLES-CORRECTIF\n\
           %a#RESULTATS-CORRECTIF\n\
           %a##\n"
          (pp_list pp_rappel) rapp.entrees_rappels (pp_list pp_calc_err)
          rapp.controles_attendus (pp_list pp_var_value) rapp.resultats_attendus
end

module Cli = struct
  open Cmdliner

  let files =
    Arg.(
      non_empty & pos_all file []
      & info [] ~docv:"FILES" ~doc:"M files to be compiled")

  let irj_file =
    Arg.(value & opt string "" & info [ "irj-file" ] ~doc:"Input irj file")

  let output = Arg.(value & opt string "" & info [ "o" ] ~doc:"Output irj file")

  let t f = Term.(const f $ files $ irj_file $ output)

  let info =
    let doc =
      "From a M project & an irj file, generates an irj file where all the \
       undefined inputs are set to zero."
    in
    Cmd.info "undef_test"
      ~version:
        (match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v)
      ~doc
end

(** Returns the list of input variables of the given source file and
    adds it to the accumulator in argument. *)
let input_variables_of_source_file (acc : string list) (sf : Mast.source_file) :
    string list =
  List.fold_left
    (fun acc (sfi, _) ->
      match sfi with
      | Mast.VariableDecl (InputVar (iv, _)) -> fst iv.input_name :: acc
      | _ -> acc)
    acc sf

(** Returns the list of input variables of the given program. *)
let input_variables (prog : Mast.program) : string list =
  List.fold_left input_variables_of_source_file [] prog

(** Returns a map linking each input of the given irj file to its value. *)
let irj_inputs_map (irj_file : Irj_ast.irj_file) : Irj_ast.literal StrMap.t =
  List.fold_left
    (fun map ((name, _), (lit, _)) -> StrMap.add name lit map)
    StrMap.empty irj_file.prim.entrees

let fill_irj_map_with_zeros ~irj_inputs ~prog_inputs : Irj_ast.literal StrMap.t
    =
  List.fold_left
    (fun acc prog_input ->
      match StrMap.find prog_input irj_inputs with
      | _ -> acc
      | exception Not_found -> StrMap.add prog_input (Irj_ast.I 0) acc)
    StrMap.empty prog_inputs

let irj_map_to_input_list map =
  StrMap.fold (fun s l acc -> ((s, Pos.no_pos), (l, Pos.no_pos)) :: acc) map []
  |> List.rev

let write_irj_file ~(output_file : string) ~(irj_file : Irj_ast.irj_file) : unit
    =
  let chan = open_out output_file in
  let fmt = Format.formatter_of_out_channel chan in
  let () = try Printer.pp_irj_file fmt irj_file with _ -> () in
  close_out chan

let run ~(irj_file : Irj_ast.irj_file) ~(mlang_prog : Mast.program)
    ~(output_file : string) : unit =
  let prog_inputs = input_variables mlang_prog
  and irj_inputs = irj_inputs_map irj_file in
  let zeroed_inputs_map = fill_irj_map_with_zeros ~irj_inputs ~prog_inputs in
  let new_inputs =
    irj_map_to_input_list irj_inputs @ irj_map_to_input_list zeroed_inputs_map
  in
  let new_irj_file =
    { irj_file with prim = { irj_file.prim with entrees = new_inputs } }
  in
  write_irj_file ~output_file ~irj_file:new_irj_file;
  ()

let parse_and_run (mlang_files : string list) (irj_file_name : string)
    (output_file : string) =
  let irj_file = Irj_file.parse_file irj_file_name
  and mlang_prog =
    List.map
      (fun file ->
        let filebuf, input =
          if file <> "" then
            let input = open_in file in
            (Lexing.from_channel input, input)
          else failwith "You have to specify at least one file!"
        in
        let filebuf =
          {
            filebuf with
            lex_curr_p = { filebuf.lex_curr_p with pos_fname = file };
          }
        in
        let res = Mparser.source_file Mlexer.token filebuf in
        close_in input;
        res)
      mlang_files
  in
  run ~irj_file ~mlang_prog ~output_file

let () =
  let code =
    Cmdliner.Cmd.eval @@ Cmdliner.Cmd.v Cli.info (Cli.t parse_and_run)
  in
  exit code
