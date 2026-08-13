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

(** Collections *)

module StrSet : Set.S with type elt = string

module StrMap : Map.S with type key = string

(** Pretty printers *)

val pp_list :
  sep:(unit, Format.formatter, unit) format ->
  pp:(Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit

val pp_str_map :
  sep:
    (unit, Format.formatter, unit) format
    * (unit, Format.formatter, unit) format ->
  pp:(Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a StrMap.t ->
  unit

val run_command : string -> string
(** Runs a command and outputs its result as a string *)

val compile_file : cfiles_dir:string -> cfile:string -> ofile:string -> string
(** Compiles a [cfile] into a .o file ([ofile]). Includes [cfiles_dir] to the
    compilation options. *)

val generate_binary : dest:string -> ofiles:string list -> string
(** From a list of .o files, generates a binary [dest]. *)

(** Different logs helpers, using [Env.debug] to select which are active. *)
module Log : sig
  val log : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout *)

  val err : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stderr *)

  val warn : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout if debug >= 1 *)

  val debug : ('a, Format.formatter, unit) format -> 'a
  (** Prints in stdout if debug >= 2 *)
end
