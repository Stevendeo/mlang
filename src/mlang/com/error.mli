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

type typ = Anomaly | Discordance | Information

val compare_typ : typ -> typ -> int

type t = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  famille : string Pos.marked;
  code_bo : string Pos.marked;
  sous_code : string Pos.marked;
  libelle : string Pos.marked;
  is_isf : string Pos.marked;
  typ : typ;
}

val pp_descr : Pp.t -> t -> unit

val pp : Pp.t -> t -> unit

val error_compare : t -> t -> int

module Set : SetExt.T with type elt = t

module Map : sig
  include MapExt.T with type key = t
end
