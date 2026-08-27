(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2024 - 2026 DGFiP - INRIA                               *)
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

open Core

val format_value_typ : Pp.t -> value_typ -> unit

val format_literal : Pp.t -> literal -> unit

val format_case : (Pp.t -> 'v -> unit) -> Pp.t -> 'v case -> unit

val format_atom : (Pp.t -> 'v -> unit) -> Pp.t -> 'v atom -> unit

val format_loop_variables :
  (Pp.t -> 'v -> unit) -> Pp.t -> 'v loop_variables -> unit

val format_unop : Pp.t -> unop -> unit

val format_binop : Pp.t -> binop -> unit

val format_comp_op : Pp.t -> comp_op -> unit

val format_access : (Pp.t -> 'v -> unit) -> Pp.t -> 'v access -> unit

val format_set_value : (Pp.t -> 'v -> unit) -> Pp.t -> 'v set_value -> unit

val format_func : Pp.t -> func -> unit

val format_expression : (Pp.t -> 'v -> unit) -> Pp.t -> 'v expression -> unit

val format_print_arg : (Pp.t -> 'v -> unit) -> Pp.t -> 'v print_arg -> unit

val format_formula : (Pp.t -> 'v -> unit) -> Pp.t -> 'v formula -> unit

val format_instruction :
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('v, 'e) instruction ->
  unit

val format_instructions :
  (Pp.t -> 'v -> unit) ->
  (Pp.t -> 'e -> unit) ->
  Pp.t ->
  ('v, 'e) m_instruction list ->
  unit
