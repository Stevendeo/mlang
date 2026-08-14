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
(** Usage:

    $ lcc -F [FILEDIR] -C [CONFIGFILE]

    Can be configured with additional environment variables.
    - [OUTPUT_DIR]: the dir to write the .o files (default: output). Generated
      if it does not exist (default: output).
    - [DEPGRAPH_FILENAME]: the file in which is serialized the dependency graph.
      Written in [OUTPUT_DIR] (default: .depgraph).
    - [DEBUG]: displays debug messages (default: 0).
    - [PEDANTIC]: makes gcc pedantic (default: 1).
    - [CC]: the C compiler to use (default: gcc).

    How to compile:

    $ bash build.sh

    TODOs:
    - logs in files;
    - versioning depgraph files or stop using Marshal (that may deserialize
      something badly and make the script fail even badlier). *)

open Utils

(** Handles the configuration file. The configuration file syntax is the
    following:
    - "# External dependencies"
    - A list of pairs "file:command" where 'file' is the name of the external
      dependency as it would appear in the C file including it, and 'command' is
      a command returning the version of the file, which will be used to check
      if it changed between two compilations. *)
module Config = struct
  let header = "# External dependencies"

  (** The regexp for reading the external dependencies pairs. *)
  let ext_dep_regexp = Str.regexp {|^\(.*\):\(.*\)$|}

  (** Reads [config_file] and builds the external depenency list of the project.
  *)
  let read ~config_file =
    let chan = open_in config_file in
    let rec empty_header_then_deps () =
      match input_line chan with
      | "" -> empty_header_then_deps ()
      | l ->
          if l <> header then (
            Log.err "[Error] File should start with %s, not %S" header l;
            raise (Failure "Config.read"))
          else edeps []
    and edeps acc =
      match input_line chan with
      | exception End_of_file -> acc
      | "" -> edeps acc
      | l ->
          if Str.string_match ext_dep_regexp l 0 then
            edeps ((Str.matched_group 1 l, Str.matched_group 2 l) :: acc)
          else (
            Log.err
              "[Error] Invalid external dependency line %s. Expected : \
               'filename':'command'"
              l;
            raise (Failure "Config.read"))
    in
    try
      let edeps = empty_header_then_deps () in
      close_in chan;
      edeps
    with exn ->
      close_in chan;
      raise exn
end

(** Compiles the file specified in the [config_file]. *)
let compile ~cfiles_dir ~config_file =
  Log.log "Starting compilation...";
  let old = Dep_graph.read () in
  Log.debug "Old graph: %a" Dep_graph.pp old;
  let new_ =
    let ext_dep = Config.read ~config_file in
    Dep_graph.make ~cfiles_dir ~ext_dep
  in
  Log.debug "New graph: %a" Dep_graph.pp new_;
  let m : Dep_graph.compiled_status StrMap.t =
    Dep_graph.compile ~cfiles_dir ~old ~new_
  in
  let newly_compiled, ofiles =
    StrMap.fold
      (fun k (b : Dep_graph.compiled_status) (acc_comp, ofiles) ->
        let acc_comp = if b.recompiled then k :: acc_comp else acc_comp
        and ofiles =
          if Filename.extension k = ".c" then b.ofile :: ofiles else ofiles
        in
        (acc_comp, ofiles))
      m ([], [])
  in
  let () =
    if newly_compiled = [] then
      Log.log "Nothing changed. Not recompiling project."
    else (
      Log.log "Compilation over.";
      Log.log "Files compiled: %a"
        (pp_list ~sep:", " ~pp:Format.pp_print_string)
        newly_compiled;
      Dep_graph.write new_)
  in
  let () =
    match Cli.bin () with
    | None -> ()
    | Some dest ->
        Log.log "Generating binary %S" dest;
        let res : string = Utils.generate_binary ~dest ~ofiles in
        Log.debug "%s" res;
        Log.log "Binary %S generated" dest
  in
  ()

(** Checks the cfiles dir exists. Also, creates the output dir if it does not
    exist. *)
let init ~cfiles_dir =
  (* Checking existence of cfiles_dir *)
  let () =
    match Sys.is_directory cfiles_dir with
    | exception Sys_error _ ->
        Format.ksprintf failwith "Directory %S does not exist" cfiles_dir
    | true -> ()
    | false -> Format.ksprintf failwith "File %S is not a directory" cfiles_dir
  in
  (* Checking existence of output dir *)
  let () =
    match Sys.is_directory @@ Cli.output_dir () with
    | exception Sys_error _ -> Sys.mkdir (Cli.output_dir ()) 0o777
    | true -> ()
    | false ->
        Format.ksprintf failwith "File %S is not a directory"
          (Cli.output_dir ())
  in
  ()

let main () =
  let () = Cli.read_args () in
  let cfiles_dir = Cli.cfiles_dir () and config_file = Cli.config_file () in
  init ~cfiles_dir;
  compile ~cfiles_dir ~config_file

let () = main ()
