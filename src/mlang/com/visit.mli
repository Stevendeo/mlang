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

module Map : sig
  type ('v, 'w, 'e, 'f) mapper = {
    instr :
      ('v, 'w, 'e, 'f) mapper ->
      ('v, 'e) m_instruction ->
      ('w, 'f) m_instruction;
    switch :
      ('v, 'w, 'e, 'f) mapper -> 'v switch_expression -> 'w switch_expression;
    formula :
      ('v, 'w, 'e, 'f) mapper -> 'v formula Pos.marked -> 'w formula Pos.marked;
    formula_decl :
      ('v, 'w, 'e, 'f) mapper -> 'v formula_decl -> 'w formula_decl;
    print_arg :
      ('v, 'w, 'e, 'f) mapper ->
      'v print_arg Pos.marked ->
      'w print_arg Pos.marked;
    set_value : ('v, 'w, 'e, 'f) mapper -> 'v set_value -> 'w set_value;
    loop_variables :
      ('v, 'w, 'e, 'f) mapper ->
      'v loop_variables Pos.marked ->
      'w loop_variables Pos.marked;
    loop_variable :
      ('v, 'w, 'e, 'f) mapper -> 'v loop_variable -> 'w loop_variable;
    loop_range : ('v, 'w, 'e, 'f) mapper -> 'v loop_range -> 'w loop_range;
    expr : ('v, 'w, 'e, 'f) mapper -> 'v m_expression -> 'w m_expression;
    access : ('v, 'w, 'e, 'f) mapper -> 'v m_access -> 'w m_access;
    case : ('v, 'w, 'e, 'f) mapper -> 'v case -> 'w case;
    atom : ('v, 'w, 'e, 'f) mapper -> 'v atom Pos.marked -> 'w atom Pos.marked;
    varspace : ('v, 'w, 'e, 'f) mapper -> var_space -> var_space;
    var : 'v -> 'w;
    err : 'e -> 'f;
  }
  (** Mapper type *)

  val default : ('a -> 'b) -> ('c -> 'd) -> ('a, 'b, 'c, 'd) mapper
  (** [default var err] returns a default mapper, which recurses on everything.
      specify [var] and [err] for how the mapper should transform them. Use as a
      basis for special mappers. *)
end

module Fold : sig
  type ('a, 'v, 'e) folder = {
    instr : ('a, 'v, 'e) folder -> ('v, 'e) m_instruction -> 'a -> 'a;
    switch : ('a, 'v, 'e) folder -> 'v switch_expression -> 'a -> 'a;
    formula : ('a, 'v, 'e) folder -> 'v formula Pos.marked -> 'a -> 'a;
    formula_decl : ('a, 'v, 'e) folder -> 'v formula_decl -> 'a -> 'a;
    print_arg : ('a, 'v, 'e) folder -> 'v print_arg Pos.marked -> 'a -> 'a;
    set_value : ('a, 'v, 'e) folder -> 'v set_value -> 'a -> 'a;
    loop_variables :
      ('a, 'v, 'e) folder -> 'v loop_variables Pos.marked -> 'a -> 'a;
    loop_variable : ('a, 'v, 'e) folder -> 'v loop_variable -> 'a -> 'a;
    loop_range : ('a, 'v, 'e) folder -> 'v loop_range -> 'a -> 'a;
    expr : ('a, 'v, 'e) folder -> 'v m_expression -> 'a -> 'a;
    access : ('a, 'v, 'e) folder -> 'v m_access -> 'a -> 'a;
    case : ('a, 'v, 'e) folder -> 'v case -> 'a -> 'a;
    atom : ('a, 'v, 'e) folder -> 'v atom Pos.marked -> 'a -> 'a;
    varspace : ('a, 'v, 'e) folder -> var_space -> 'a -> 'a;
  }
  (** Folder type *)

  val default : ('a, 'v, 'e) folder
  (** A default folder. Folds on everything. Use as a basis for special folders.
  *)
end

val expr_map_var : ('v -> 'w) -> 'v expression -> 'w expression

val m_expr_map_var : ('v -> 'w) -> 'v m_expression -> 'w m_expression

val instr_map_var :
  ('v -> 'w) -> ('e -> 'f) -> ('v, 'e) instruction -> ('w, 'f) instruction

val m_instr_map_var :
  ('v -> 'w) -> ('e -> 'f) -> ('v, 'e) m_instruction -> ('w, 'f) m_instruction

type var_usage = Read | Write | Info | DeclRef | ArgRef | DeclLocal | Macro

val expr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) -> 'v expression -> 'a -> 'a

val m_expr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  'v m_expression ->
  'a ->
  'a

val instr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  ('v, 'e) instruction ->
  'a ->
  'a

val m_instr_fold_var :
  (var_usage -> var_space -> 'v option -> 'a -> 'a) ->
  ('v, 'e) m_instruction ->
  'a ->
  'a
