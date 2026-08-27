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

type id = int

(** Data on a TGV variable. *)
type tgv = {
  table : id Array.t option;
      (** The array of cells if the variable is a table. *)
  alias : string Pos.marked option;  (** Input variable have an alias *)
  descr : string Pos.marked;
      (** Description taken from the variable declaration *)
  attrs : int Pos.marked StrMap.t;
      (** Variable's attributes and their values *)
  cat : CatVar.t;  (** Category *)
  is_given_back : bool;  (** Is the variable 'restituee'? *)
  typ : value_typ option;  (** Optional variable type *)
  table_cell : (id * int) option;
      (** Says if the variable is a table cell, ie TAB0 from the table TAB.
          Payload is the name of the table variable *)
}
(** Exhaustive data on a TGV variable. *)

(** Where can the variable be found? *)
and scope =
  | Tgv of tgv  (** This variable belongs to the TGV. *)
  | Temp of id Array.t option
      (** This variable is temporary, maybe an array. *)
  | Ref  (** This references another variable. *)

and t = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  id : id;
      (** A unique identifier, enforced by [Var.new_{tgv,temp,ref,arg,res}] *)
  loc : loc;
  scope : scope;  (** Data on the variable. *)
}

(** {2 Getters and setters} *)

val tgv : t -> tgv
(** Returns the tgv data of a variable; fails if it is not a TGV variable. *)

val name : t -> string Pos.marked
(** Returns a markzs variable name. *)

val name_str : t -> string
(** Same as [name] without the mark. *)

val get_table : t -> id Array.t option
(** Returns the table represented by the variable, if relevant. Returns [None]
    on references. *)

val get_table_cell : t -> (id * int) option
(** [get_table_cell var] returns from which array this variable is a cell of. *)

val set_table_cell : t -> id:id -> idx:int -> t
(** [set_table_cell var id idx] sets the variable as a cell of id at index idx.
*)

val is_table : t -> bool
(** Returns true if the variable represents a table. *)

val set_table : t -> id Array.t option -> t
(** Sets a table to the given variable. *)

val cat_var_loc : t -> CatVar.loc
(** Returns the category of a TGV variable; fails if it is not a TGV variable.
*)

val size : t -> int
(** Returns the size of a variable: the size of the array if it is a table; 1
    otherwise. *)

val alias : t -> string Pos.marked option
(** Returns the variable's alias if any, otherwise returns None. Aliases only
    are set for input variables. *)

val alias_str : t -> string
(** Same as [alias] without the mark, or "" if no alias was given. *)

val descr : t -> string Pos.marked
(** Returns the description of the variable; fails if it is not a TGV variable.
*)

val descr_str : t -> string
(** Same as [descr] without the mark. *)

val attrs : t -> int Pos.marked StrMap.t
(** Returns the attributes of a TGV variable; fails if it is not a TGV variable.
*)

val cat : t -> CatVar.t
(** Returns the category of a TGV variable; fails if it is not a TGV variable.
*)

val typ : t -> value_typ option
(** Returns the type of a TGV variable; fails if it is not a TGV variable. *)

val is_given_back : t -> bool
(** Returns [true] if TGV variable in argument is 'restituee'; fails if it is
    not a TGV variable. *)

val loc_tgv : t -> loc_tgv
(** Returns the loc data of a TGV variable; fails if it not a TGV variable. *)

val loc_cat_idx : t -> int

val set_loc_tgv_idx : t -> CatVar.data -> int -> t

val set_loc_tmp_idx : t -> int -> t

val loc_idx : t -> int

val set_loc_idx : t -> int -> t

val loc_tab_idx : t -> int

val set_loc_tab_idx : t -> int -> t

val is_tgv : t -> bool
(** Returns true if it is a TGV variable. *)

val is_temp : t -> bool
(** Returns true if it is a temporary variable. *)

val is_ref : t -> bool
(** Returns true if it is a reference. *)

val new_tgv :
  name:string Pos.marked ->
  table:id Array.t option ->
  is_given_back:bool ->
  alias:string Pos.marked option ->
  descr:string Pos.marked ->
  attrs:int Pos.marked StrMap.t ->
  cat:CatVar.t ->
  typ:value_typ option ->
  table_cell:(id * int) option ->
  t
(** Creates a new tgv variable with a unique id. *)

val new_temp : name:string Pos.marked -> table:id Array.t option -> t
(** Creates a new temporary variable with a unique id. *)

val new_ref : name:string Pos.marked -> t
(** Creates a new reference with a unique id. *)

val new_arg : name:string Pos.marked -> t
(** Creates a new temporary variable for a function's argument. *)

val new_res : name:string Pos.marked -> t
(** Creates a new temporary variable for a function's result. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val compare : t -> t -> int

module Set : SetExt.T with type elt = t

module Map : sig
  include MapExt.T with type key = t
end

(* val compare_name_ref : (string -> string -> int) ref

     val compare_name : string -> string -> int*)
