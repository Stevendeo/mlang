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

(** Environment utilities for printing a M target in C. They generate utils for
    the C generation modules as well as gathering some information about which
    variables are used. *)

type t = Types.env

val empty_env :
  dgfip_flags:Dgfip_options.flags -> prog:Mir.program -> name:string -> t
(** Creates a fresh environment. The [name] is used to name the quit label. *)

val gen_def_ptr : env:t -> Com.var_space -> Com.Var.t -> string
(** [gen_def_ptr ~env vs var]

    Returns a pointer to the definition value of [var] in varspace [vs]. *)

val gen_val_ptr : env:t -> Com.var_space -> Com.Var.t -> string
(** [gen_def_ptr ~env vs var]

    Returns a pointer to the value of [var] in varspace [vs]. *)

val gen_def : env:t -> Com.var_space -> Com.Var.t -> string
(** [gen_def_ptr ~env vs var]

    Returns the definition variable of [var] in varspace [vs]. *)

val gen_val : env:t -> Com.var_space -> Com.Var.t -> string
(** [gen_val ~env vs var]

    Returns the variable to the value of [var] in varspace [vs]. *)

val fresh_scope : env:t -> Com.Var.t -> Types.id_scope
(** Creates a fresh scopeand adds it to the *)

val add_sanitizer : env:t -> (unit -> unit) -> int
(** Registers a sanitizer. A sanitizer is called when it is popped from the
    scope stack, whether manually with [pop_scope], or when calling [sanitize].
*)

val pop_scope : env:t -> scope_id:string -> unit
(** Removes the cope in argument if it is at the top of the stack. Fails
    otherwise. *)

val pop_sanitizer : env:t -> sid:int -> unit
(** Pops a sanitizer from its id and calls it if it is at the top of the stack.
*)

val goto_current_label : env:t -> string option
(** Generates a goto instruction to the current label in the stack. Returns None
    if the stack is empty or if the top of the stack is a sanitizer. *)

val goto_label_from : env:t -> scope_id:string -> string option
(** Generates a goto instruction to the label whose scope is given as argument.
    Fails if the scope is not in the stack. *)

val sanitize : env:t -> up_to:[< `Bottom | `Id of string | `NextId ] -> unit
(** Calls sanitizers on the stack:
    - if [up_to = `Bottom], calls all the sanitizers;
    - if [up_to = `Id i], calls the sanitizers up to scope with id [i];
    - id [up_to = `NextId], calls the sanitizers up to the closest id. *)

val goto_quit_label : env:t -> string
(** Generates a goto instruction to the quit label of the target. *)
