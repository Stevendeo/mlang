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

(** High level representation of categories of variable. In practice, there only
    are two types of categories: * "calculee" (Input) variables, with several
    sub categories; * "saisie" (Computed) variable, that can either be base or
    non-base.

    Such representation allows to manipulate sets of variable (for example in
    iterations). *)

type t =
  | Input of StrSet.t  (** An input variable category with its sub categories *)
  | Computed of { is_base : bool }
      (** A computed var with a flag marking if it is a base varible. *)

val all_inputs : t
(** A category containing all inputs. *)

val is_input : t -> bool
(** Returns [true] if the category in argument is an input variable category. *)

val is_computed : t -> bool
(** Returns [true] if the category in argument is a computed variable category.
*)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val compare_t : t -> t -> int

module Set : SetExt.T with type elt = t

module Map : sig
  include MapExt.T with type key = t

  val from_string_list : string Pos.marked list Pos.marked -> Pos.t t
  (** [from_string_list l]

      From a category identifier [l] seen as a list of strings (for example
      [["saisie"; "revenu"; "corrective"]] for the category
      ["saisie revenu corrective"]), returns a map binding all [CatVar.t] values
      refered by [l] to the location of [l]. *)
end

type loc = LocComputed | LocBase | LocInput

val pp_loc : Format.formatter -> loc -> unit

module LocSet : SetExt.T with type elt = loc

module LocMap : MapExt.T with type key = loc

type data = {
  id : t;  (** The category *)
  id_str : string;  (** A unique string identifier for the category *)
  id_int : int;  (** A unique int identifier *)
  loc : loc;  (** Quick access to the category kind *)
  pos : Pos.t;  (** Variable category declaration *)
  attributs : Pos.t StrMap.t;  (** Attributes of the category *)
}
(** Data for a given category. Defined in the validator. *)
