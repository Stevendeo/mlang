(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)
open Cmdliner

let get (type t) (v : [ `Uninit | `Init of t ] ref) : t =
  match !v with `Uninit -> failwith "Uninitialized option" | `Init v -> v

let set (type t) (v : t) (r : [ `Uninit | `Init of t ] ref) : unit =
  r := `Init v

let smart_ref (type t) (default : t option) : (t -> unit) * (unit -> t) =
  let v = match default with None -> `Uninit | Some v -> `Init v in
  let res = ref v in
  let get () = get res and set t = set t res in
  (set, get)

let set_cfiles_dir, cfiles_dir = smart_ref None

let set_config_file, config_file = smart_ref None

let set_bin, bin = smart_ref (Some None)

let set_output_dir, output_dir = smart_ref (Some "output")

let set_graph_filename, graph_filename = smart_ref (Some ".depgraph")

let set_debug, debug = smart_ref (Some 0)

let set_pedantic, pedantic = smart_ref (Some 1)

let set_cc, cc = smart_ref (Some "gcc")

(* -- Cmdliner -- *)

let arg_cfiles_dir =
  Arg.(
    value & opt (some string) None & info [ "file-dir"; "F" ] ~doc:"C files dir")

let arg_config_file =
  Arg.(
    value & opt (some string) None & info [ "config"; "C" ] ~doc:"Config file")

let arg_bin =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"EXEC_FILENAME" [ "bin"; "B" ] ~doc:"Generates the binary")

let arg_output_dir =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "O" ] ~doc:"The compilation directory")

let arg_graph_filename =
  Arg.(
    value
    & opt (some string) None
    & info [ "graph"; "G" ] ~doc:"The dependency graph filename")

let arg_debug =
  Arg.(
    value & opt (some int) None & info [ "debug"; "D" ] ~doc:"The debug level")

let arg_pedantic =
  Arg.(
    value
    & opt (some int) None
    & info [ "pedantic"; "P" ] ~doc:"The pedantic level of the compiler")

let arg_cc =
  Arg.(
    value & opt (some string) None & info [ "cc" ] ~doc:"The compiler to use")

let init_vars cfiles_dir config_file (bin : string option) output_dir dep_graph
    debug pedantic cc =
  Option.iter set_config_file config_file;
  Option.iter set_cfiles_dir cfiles_dir;
  set_bin bin;
  Option.iter set_output_dir output_dir;
  Option.iter set_graph_filename dep_graph;
  Option.iter set_debug debug;
  Option.iter set_pedantic pedantic;
  Option.iter set_cc cc

let lcc_term =
  Term.(
    const init_vars $ arg_cfiles_dir $ arg_config_file $ arg_bin
    $ arg_output_dir $ arg_graph_filename $ arg_debug $ arg_pedantic $ arg_cc)

let info =
  let doc = "Lazy C compiler" in
  let man = [] in
  let exits = Cmd.Exit.defaults in
  Cmd.info "lcc" ~version:"0.0.1" ~doc ~exits ~man

let read_args () =
  match Cmdliner.Cmd.eval_value @@ Cmdliner.Cmd.v info lcc_term with
  | Ok (`Help | `Version) -> exit 0
  | Ok _ -> ()
  | Error _e -> failwith "Cli failed"
