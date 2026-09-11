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

type t

val empty : unit -> t

val raise : t -> Com.Error.t -> string option -> bool
(** Adds the anomaly to the context and returns [true] if the said anomaly is
    blocking, [false] otherwise. *)

val clean : t -> unit
(** Cleans the context from its unfinalized and unarchived anomalies. *)

val clean_finalized : t -> unit
(** Cleans the context from its finalized anomalies. *)

val finalize : mode_corr:bool -> t -> unit
(** Moves the raised anomalies to the finalized anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)

val export : mode_corr:bool -> t -> unit
(** Moves the finalized anomalies to the exported anomalies (and the archived
    anomalies if [mode_corr] is [true]). *)

val nb_anomalies : t -> int
(** Returns the amount of [raise]d anomalies. *)

val nb_discordances : t -> int
(** Returns the amount of [raise]d discordances. *)

val nb_informatives : t -> int
(** Returns the amount of [raise]d informations. *)

val nb_bloquantes : t -> int
(** Returns the amount of [raise]d blocking anomalies. *)

val exported : t -> (Com.Error.t * string option) list
