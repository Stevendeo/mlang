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

let cfiles_dir_ref = ref `Uninit

let config_file_ref = ref `Uninit

let bin_ref = ref (`Init None)

let get (type t) (v : [ `Uninit | `Init of t ] ref) : t =
  match !v with `Uninit -> failwith "Uninitialized option" | `Init v -> v

let set (type t) (v : t) (r : [ `Uninit | `Init of t ] ref) : unit =
  r := `Init v

let set_cfiles_dir (f : string) = set f cfiles_dir_ref

let set_config_file f = set f config_file_ref

let cfiles_dir () = get cfiles_dir_ref

let config_file () = get config_file_ref

let set_bin f = set (Some f) bin_ref

let bin () = get bin_ref

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
    & info [ "bin"; "B" ] ~doc:"Generates the binary")

let init_vars cfiles_dir config_file bin =
  Option.iter set_config_file config_file;
  Option.iter set_cfiles_dir cfiles_dir;
  Option.iter set_bin bin

let lcc_term = Term.(const init_vars $ arg_cfiles_dir $ arg_config_file $ arg_bin)

let info =
  let doc = "Lazy C compiler" in
  let man = [] in
  let exits = Cmd.Exit.defaults in
  Cmd.info "lcc" ~version:"0.0.1" ~doc ~exits ~man

let read_args () =
  match Cmdliner.Cmd.eval_value @@ Cmdliner.Cmd.v info lcc_term with
  | Ok _v -> ()
  | Error _e -> failwith "Cli failed"
