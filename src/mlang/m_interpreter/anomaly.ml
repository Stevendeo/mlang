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

open M_ir

type t = {
  mutable anos : (Com.Error.t * string option) list;
      (** Errors raised during the interpretation. *)
  mutable nb_anos : int;  (** Number of anomalies in the previous error list.*)
  mutable nb_discos : int;
      (** Number of discordances in the previous error list. *)
  mutable nb_infos : int;
      (** Number of informations in the previous error list. *)
  mutable nb_bloquantes : int;  (** Number of blocking anomalies. *)
  mutable archived_anos : StrSet.t;  (** Archived anomalies *)
  mutable finalized_anos : (Com.Error.t * string option) list;
      (** Finalized errors. *)
  mutable exported_anos : (Com.Error.t * string option) list;
      (** Exported errors. *)
}

let empty () =
  {
    anos = [];
    nb_anos = 0;
    nb_discos = 0;
    nb_infos = 0;
    nb_bloquantes = 0;
    archived_anos = StrSet.empty;
    finalized_anos = [];
    exported_anos = [];
  }

let raise (ctx : t) (err : M_ir.Com.Error.t) (v_opt : string option) =
  (match err.typ with
  | Com.Error.Anomaly -> ctx.nb_anos <- ctx.nb_anos + 1
  | Com.Error.Discordance -> ctx.nb_discos <- ctx.nb_discos + 1
  | Com.Error.Information -> ctx.nb_infos <- ctx.nb_infos + 1);
  let is_blocking =
    err.typ = Com.Error.Anomaly && Pos.unmark err.is_isf = "N"
  in
  ctx.nb_bloquantes <- (ctx.nb_bloquantes + if is_blocking then 1 else 0);
  ctx.anos <- ctx.anos @ [ (err, v_opt) ];
  is_blocking

let clean (ctx : t) =
  ctx.anos <- [];
  ctx.nb_anos <- 0;
  ctx.nb_discos <- 0;
  ctx.nb_infos <- 0;
  ctx.nb_bloquantes <- 0

let clean_finalized (ctx : t) = ctx.finalized_anos <- []

let finalize ~mode_corr ctx =
  let mem (ano : Com.Error.t) anos =
    List.fold_left
      (fun res ((a : Com.Error.t), _) ->
        res || Pos.unmark a.name = Pos.unmark ano.name)
      false anos
  in
  if mode_corr then
    let rec merge_anos () =
      match ctx.anos with
      | [] -> ()
      | ((ano : Com.Error.t), arg) :: discos ->
          let cont =
            if not (mem ano ctx.finalized_anos) then (
              ctx.finalized_anos <- ctx.finalized_anos @ [ (ano, arg) ];
              ano.typ <> Com.Error.Anomaly)
            else true
          in
          ctx.anos <- discos;
          if cont then merge_anos ()
    in
    merge_anos ()
  else
    let not_in_old_anos (err, _) =
      let name = Pos.unmark err.Com.Error.name in
      not (StrSet.mem name ctx.archived_anos)
    in
    ctx.finalized_anos <-
      (let rec merge_anos old_anos new_anos =
         match (old_anos, new_anos) with
         | [], anos | anos, [] -> anos
         | _ :: old_tl, a :: new_tl -> a :: merge_anos old_tl new_tl
       in
       let new_anos = List.filter not_in_old_anos ctx.anos in
       merge_anos ctx.finalized_anos new_anos);
    let add_ano res (err, _) = StrSet.add (Pos.unmark err.Com.Error.name) res in
    ctx.archived_anos <- List.fold_left add_ano ctx.archived_anos ctx.anos

let export ~mode_corr ctx =
  if mode_corr then
    let rec merge_anos () =
      match ctx.finalized_anos with
      | [] -> ()
      | ((ano : Com.Error.t), arg) :: fins ->
          if not (StrSet.mem (Pos.unmark ano.name) ctx.archived_anos) then (
            ctx.archived_anos <-
              StrSet.add (Pos.unmark ano.name) ctx.archived_anos;
            ctx.exported_anos <- ctx.exported_anos @ [ (ano, arg) ]);
          ctx.finalized_anos <- fins;
          merge_anos ()
    in
    merge_anos ()
  else (
    ctx.exported_anos <- ctx.exported_anos @ ctx.finalized_anos;
    ctx.finalized_anos <- [])

let nb_anomalies ctx = ctx.nb_anos

let nb_discordances ctx = ctx.nb_discos

let nb_informatives ctx = ctx.nb_infos

let nb_bloquantes ctx = ctx.nb_bloquantes

let exported ctx = ctx.exported_anos
