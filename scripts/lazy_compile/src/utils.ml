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
module Env = struct
  (** Returns the value of an env variable [k]. If absent, returns [default]. *)
  let getenv ~default k =
    match Sys.getenv k with v -> v | exception Not_found -> default

  (** The output dir *)
  let output_dir = getenv ~default:"output" "OUTPUT_DIR"

  (** The graph filename *)
  let graph_filename = getenv ~default:".depgraph" "DEPGRAPH_FILENAME"

  (** If set to something else than "0", display debug messages.*)
  let debug = getenv ~default:"0" "DEBUG"

  let pedantic = getenv ~default:"1" "PEDANTIC"

  let cc = getenv ~default:"gcc" "CC"
end

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(** Pretty prints a list. *)
let pp_list ~sep ~pp fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt sep) pp fmt l

(** Pretty prints a string map. *)
let pp_str_map ~sep ~pp fmt m =
  let skb, sl = sep in
  StrMap.iter
    (fun k b ->
      Format.fprintf fmt "%s%t%a%t" k
        (fun fmt -> Format.fprintf fmt skb)
        pp b
        (fun fmt -> Format.fprintf fmt sl))
    m

module Log = struct
  let dbg = int_of_string_opt Env.debug

  let log : 'a. ('a, Format.formatter, unit) format -> 'a =
   fun ppf -> Format.(fprintf std_formatter ("[APP] " ^^ ppf ^^ "@."))

  let err : 'a. ('a, Format.formatter, unit) format -> 'a =
   fun ppf -> Format.(fprintf err_formatter ("[ERR] " ^^ ppf ^^ "@."))

  let warn : 'a. ('a, Format.formatter, unit) format -> 'a =
   fun ppf ->
    match dbg with
    | Some i when i >= 1 ->
        Format.(fprintf std_formatter ("[WRN] " ^^ ppf ^^ "@."))
    | _ -> Format.(ifprintf std_formatter ppf)

  let debug : 'a. ('a, Format.formatter, unit) format -> 'a =
   fun ppf ->
    match dbg with
    | Some i when i >= 2 ->
        Format.(fprintf std_formatter ("[DBG] " ^^ ppf ^^ "@."))
    | _ -> Format.(ifprintf std_formatter ppf)
end

(** Runs a command and returns its output as a string *)
let run_command (cmd : string) : string =
  Log.debug "%s" cmd;
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(** Compiles [cfile]. *)
let compile_file ~cfiles_dir ~cfile ~ofile =
  let pedantic = if Env.pedantic = "0" then "" else "--pedantic " in
  let cmd =
    Format.sprintf "%s -std=c89 -I%s %s -O2 -c %s -o %s" Env.cc cfiles_dir
      pedantic cfile ofile
  in
  Log.log "Compiling file %S..." cfile;
  let res = run_command cmd in
  Log.log "Compilation of file %S complete-> %S" cfile ofile;
  res

let generate_binary ~dest ~ofiles =
  let pedantic = if Env.pedantic = "0" then "" else "--pedantic " in
  let cmd =
    Format.asprintf "%s -std=c89 %s -O2 %a -o %s -lm" Env.cc pedantic
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt " ")
         Format.pp_print_string)
      ofiles dest
  in
  Log.log "Compiling binary %S..." dest;
  let res = run_command cmd in
  Log.log "%s" res;
  Log.log "Compilation of binary %S complete" dest;
  res
