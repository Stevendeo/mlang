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
open Utils

type compiled_status = { recompiled : bool; ofile : string }

type file =
  (* Files that needs to be compiled.  *)
  | Mlang_gen of {
      mname : string;
      (* The base name of the file *)
      mhash : Digest.t;
      (* Its content's digest *)
      mdeps : string list; (* Its dependencies *)
    }
  (* External dependencies, no need to compile them *)
  | Ext_dep of {
      edname : string;
      (* The basename of the dependency *)
      edvers : string; (* The dependency verison *)
    }

type t = {
  graph : file StrMap.t;
  (* map of file basenames to their file representation *)
  cfiles : string list;
  (* The list of files to compile *)
  ext_dep : (string * string) list; (* name * command to get version *)
}

exception MissingFileDeclaration of string

let empty = { graph = StrMap.empty; cfiles = []; ext_dep = [] }

(** The regexp that matches the following substrings: #include<str>
    #include"str" #include<str" #include"str>

    Why the last two? Because we will compile the C files eventually and invalid
    C intructions will be rejected, so why bother. TODO: make it better if you
    want. *)
let magic_regexp = Str.regexp {|^.*#include \(<\|"\)\(.*\)\(>\|"\)|}

let get_cfiles_of_dir cfiles_dir =
  let files = Sys.readdir cfiles_dir in
  Array.fold_left
    (fun acc f ->
      if f = "" then acc
      else
        match (Filename.extension f, f.[0]) with
        | (".c" | ".h"), ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') -> f :: acc
        | _ -> acc)
    [] files

(** Pretty prints a file. For debug only. *)
let pp_file fmt (f : file) =
  match f with
  | Mlang_gen { mdeps; mhash; _ } ->
      Format.fprintf fmt "M(%s)[%a]" (Digest.to_hex mhash)
        (pp_list ~sep:";@," ~pp:Format.pp_print_string)
        mdeps
  | Ext_dep { edvers; _ } -> Format.fprintf fmt "E(%s)" edvers

(** Pretty prints a graph. For debug only. *)
let pp fmt (t : t) =
  Format.fprintf fmt
    "Files to compile: [%a]@;External dependencies: [%a]@;Graph: %a@;"
    (pp_list ~sep:";" ~pp:Format.pp_print_string)
    t.cfiles
    (pp_list ~sep:";" ~pp:(fun fmt (v, _) -> Format.pp_print_string fmt v))
    t.ext_dep
    (pp_str_map ~sep:(",", "@,") ~pp:pp_file)
    t.graph

(** Checks if a line is a C include. If so, returns the file included.
    Otherwise, returns [None]. *)
let line_states_it_depends_on l =
  if Str.string_match magic_regexp l 0 then Some (Str.matched_group 2 l)
  else None

(** Returns the list of dependencies of a given file. *)
let file_states_it_depends_on f =
  let i = open_in f in
  let rec loop acc =
    match input_line i with
    | exception End_of_file ->
        close_in i;
        acc
    | l -> (
        match line_states_it_depends_on l with
        | None -> loop acc
        | Some f -> loop (f :: acc))
  in
  loop []

(** Adds a file to the graph. The file must have been declared in either the
    field [mlang_generated] or the [ext_dep one]; otherwise, raises
    [MissingFileDeclaration]. If it already belongs to the graph, does nothing.
*)
let rec add_file_to_graph ~cfiles_dir t filename =
  if StrMap.mem filename t.graph then (* Already treated *)
    t
  else if List.mem filename t.cfiles then
    (* File to compile: calculating its digest & dependencies. *)
    let cfile = Filename.concat cfiles_dir filename in
    let hash = Digest.file cfile in
    let deps = file_states_it_depends_on cfile in
    let t =
      {
        t with
        graph =
          StrMap.add filename
            (Mlang_gen { mname = filename; mhash = hash; mdeps = deps })
            t.graph;
      }
    in
    (* Recursively adds its dependencies to the graph. *)
    List.fold_left (add_file_to_graph ~cfiles_dir) t deps
  else
    match List.find (fun f -> filename = fst f) t.ext_dep with
    | _, cmd ->
        (* This is an external dependency. Running the command version to add
          it to the graph. *)
        let edvers = run_command cmd |> String.trim in
        {
          t with
          graph =
            StrMap.add filename (Ext_dep { edname = filename; edvers }) t.graph;
        }
    | exception Not_found ->
        (* File is neither a mlang file nor an external dependency. *)
        raise (MissingFileDeclaration filename)

(** Returns the name of the compilation output file. *)
let output_file_name cfile =
  Filename.concat (Cli.output_dir ()) (Filename.chop_extension cfile ^ ".o")

(** Intermediary function; From an [old] dependency map corresponding to an old
    compilation, and a [new_] dependency map built from a configuration file,
    compiles a graph node (that should come from [new_]). The [compiled] map
    stores for each file basename a boolean stating the files depending on it
    will need to be recompiled ([true]) or do not need recompilation ([false]).
    If the node is an external dependency, checks if the version is the same
    than in [old]. If so, maps it in [compiled] to [false], otherwise to [true].
    If the node is an mlang generated file, compiles all its dependencies &
    checks if one needed to be recompiled: if so, maps it in [compiled] to
    [true], otherwise to [false]. *)
let rec compile_node_ ~cfiles_dir ~(old : t) ~(new_ : t)
    (compiled : compiled_status StrMap.t) :
    file -> compiled_status StrMap.t * bool = function
  | Ext_dep { edname; edvers } ->
      let should_recompile =
        match StrMap.find edname old.graph with
        | exception Not_found ->
            Log.warn "External dependency %S not found in old graph" edname;
            true
        | Mlang_gen _ ->
            Log.warn "External dependency %S defined as mlang file in old graph"
              edname;
            true
        | Ext_dep { edvers = edvers'; _ } -> edvers <> edvers'
      in
      (compiled, should_recompile)
  | Mlang_gen { mname; mhash; mdeps } -> (
      let ofile = output_file_name mname in
      let compile () =
        if Filename.extension mname = ".h" then begin
          Log.debug "Skipping header file %S" mname;
          (compiled, true)
        end
        else begin
          Log.debug "Compiling mlang generated file %S" mname;
          Log.debug "Dependencies: %i" (List.length mdeps);
          let (res : string) =
            Utils.compile_file ~cfiles_dir
              ~cfile:(Filename.concat cfiles_dir mname)
              ~ofile
          in
          Log.log "Result: %s" res;
          (StrMap.add mname { recompiled = true; ofile } compiled, true)
        end
      in
      let dont_recompile () =
        Log.debug "Not compiling mlang generated file %S" mname;
        (StrMap.add mname { recompiled = false; ofile } compiled, false)
      in
      match StrMap.find mname compiled with
      | b -> (compiled, b.recompiled)
      | exception Not_found -> (
          (* Compiles dependencies *)
          let compiled, should_recompile =
            List.fold_left
              (fun (set, should_recomp_acc) dep ->
                Log.debug "Compile dependency %S" dep;
                let set, should_recomp =
                  compile_node_ ~cfiles_dir ~old ~new_ set
                    (StrMap.find dep new_.graph)
                in
                (set, should_recomp_acc || should_recomp))
              (compiled, false) mdeps
          in
          (* TODO: recompile here *)
          match StrMap.find mname old.graph with
          | Ext_dep _ | (exception Not_found) -> compile ()
          | Mlang_gen { mhash = mhash'; _ }
            when mhash <> mhash' || should_recompile
                 || not (Sys.file_exists ofile) ->
              compile ()
          | Mlang_gen _ -> dont_recompile ()))

(** Compiles the mlang_generated files of a graph. *)
let compile ~cfiles_dir ~old ~new_ =
  List.fold_left
    (fun compiled d ->
      let compiled, _ =
        compile_node_ ~cfiles_dir ~old ~new_ compiled (StrMap.find d new_.graph)
      in
      compiled)
    StrMap.empty new_.cfiles

(** From a list of mlang files and external dependencies, returns the graph with
    all the mlang files and its dependencies. Fails with
    [MissingFileDeclaration] if an mlang file depends on a file that is neither
    in [files_of_dir] nor [ext_dep]. *)
let make ~cfiles_dir ~ext_dep =
  let cfiles = get_cfiles_of_dir cfiles_dir in
  let empty_graph = { graph = StrMap.empty; cfiles; ext_dep } in
  List.fold_left (add_file_to_graph ~cfiles_dir) empty_graph cfiles

(* -- Graph serialization -- *)

let lazy_compile_version () = Digest.file Sys.argv.(0)

(** Writes a graph under the following readable format: # Version <digest of the
    binary> # Graph <filename1>:<file_kind1>:<payload1> ... # Cfiles <file1> ...
    # External dependencies <(external dependency1 * command to get version1)>
    ... . *)
let read, write =
  let exception Invalid_kind in
  let exception Stop in
  let version_header = "# Version"
  and graph_header = "# Graph"
  and cfiles_header = "# C files"
  and extdep_header = "# External dependencies" in
  let write_version oc =
    output_line oc @@ Digest.to_hex @@ lazy_compile_version ()
  and read_version ic = input_line ic |> Digest.from_hex
  and write_file oc filename file =
    let line =
      match file with
      | Mlang_gen { mname; mhash; mdeps } ->
          Format.asprintf "mlang-file:%s:%s:%s:[%a]" mname filename
            (Digest.to_hex mhash)
            (Format.pp_print_list
               ~pp_sep:(fun fmt _ -> Format.fprintf fmt ";")
               Format.pp_print_string)
            mdeps
      | Ext_dep { edname; edvers } ->
          Format.sprintf "ext-dep:%s:%s:%s" filename edname edvers
    in
    output_line oc line
  and read_file =
    let read_mlang_gen l =
      Scanf.sscanf l "%[^:]:%[^:]:%[^:]:%[^:]:[%[^]]]"
        (fun kind filename mname mhash mdeps ->
          if kind = "mlang-file" then
            ( filename,
              Mlang_gen
                {
                  mname;
                  mhash = Digest.from_hex mhash;
                  mdeps = String.split_on_char ';' mdeps;
                } )
          else raise Invalid_kind)
    and read_ext_dep l =
      Scanf.sscanf l "%[^:]:%[^:]:%[^:]:%[^:]"
        (fun kind filename edname edvers ->
          if kind = "ext-dep" then (filename, Ext_dep { edname; edvers })
          else raise Invalid_kind)
    in
    fun l ->
      try read_mlang_gen l
      with Invalid_kind | End_of_file -> (
        try read_ext_dep l
        with Invalid_kind | End_of_file ->
          Format.ksprintf failwith "Reading file, cannot decode %S" l)
  and write_cfile oc = output_line oc
  and read_cfile l = l
  and write_ext_dep oc (f, cmd) = output_line oc @@ Format.sprintf "%s:%s" f cmd
  and read_ext_dep l =
    try Scanf.sscanf l "%[^:]:%s" (fun i j -> (i, j))
    with End_of_file ->
      Format.ksprintf failwith "Reading ext-dep, cannot decode %S" l
  in
  let write oc g =
    output_line oc version_header;
    write_version oc;
    output_line oc graph_header;
    StrMap.iter (write_file oc) g.graph;
    output_line oc cfiles_header;
    List.iter (write_cfile oc) g.cfiles;
    output_line oc extdep_header;
    List.iter (write_ext_dep oc) g.ext_dep
  and read ic =
    while input_line ic <> version_header do
      ()
    done;
    if read_version ic <> lazy_compile_version () then begin
      Log.warn "Newer version of lazy compile: ignoring old data.";
      empty
    end
    else begin
      while input_line ic <> graph_header do
        ()
      done;
      Log.debug "Reading graph...@.";
      let graph =
        let res = ref StrMap.empty in
        let () =
          try
            while true do
              let l = input_line ic in
              Log.debug "Line %S@." l;
              if l = cfiles_header then raise Stop;
              let fname, f = read_file l in
              res := StrMap.add fname f !res
            done
          with Stop -> ()
        in
        !res
      in
      Log.debug "Reading cfiles...@.";
      let cfiles =
        let res = ref [] in
        let () =
          try
            while true do
              let l = input_line ic in
              Log.debug "Line %S@." l;
              if l = extdep_header then raise Stop;
              let f = read_cfile l in
              res := f :: !res
            done
          with Stop -> ()
        in
        !res
      in
      Log.debug "Reading external dependencies...@.";
      let ext_dep =
        let res = ref [] in
        let () =
          try
            while true do
              let l = try input_line ic with End_of_file -> raise Stop in
              Log.debug "Line %S@." l;
              let f = read_ext_dep l in
              res := f :: !res
            done
          with Stop -> ()
        in
        !res
      in
      { graph; cfiles; ext_dep }
    end
  in
  (read, write)

let read () =
  match
    open_in (Filename.concat (Cli.output_dir ()) (Cli.graph_filename ()))
  with
  | exception Sys_error _ -> empty
  | c -> (
      let clean () = close_in c in
      try
        let res = read c in
        clean ();
        res
      with e ->
        clean ();
        raise e)

let write t =
  let c =
    open_out (Filename.concat (Cli.output_dir ()) (Cli.graph_filename ()))
  in
  let clean () = close_out c in
  try
    let res = write c t in
    clean ();
    res
  with e ->
    clean ();
    raise e
