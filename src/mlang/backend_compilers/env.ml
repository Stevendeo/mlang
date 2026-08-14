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

open Types
module VID = Dgfip_varid

type env = Types.env

type t = env

let uses_def ~env = function
  | `Base -> env.uses_base_def <- true
  | `Calculee -> env.uses_calculee_def <- true
  | `Saisie -> env.uses_saisie_def <- true
  | `Temp | `Ref -> ()

let uses_val ~env = function
  | `Base -> env.uses_base_val <- true
  | `Calculee -> env.uses_calculee_val <- true
  | `Saisie -> env.uses_saisie_val <- true
  | `Temp | `Ref -> ()

let gen_def_ptr ~env m_sp var =
  let s, kdef = VID.gen_def_ptr m_sp var in
  if m_sp = None then uses_def kdef ~env;
  s

let gen_val_ptr ~env m_sp var =
  let s, kval = VID.gen_val_ptr m_sp var in
  if m_sp = None then uses_val kval ~env;
  s

let gen_def ~env m_sp var =
  let s, kdef = VID.gen_def m_sp var in
  if m_sp = None then uses_def kdef ~env;
  s

let gen_val ~env m_sp var =
  let s, kval = VID.gen_val m_sp var in
  if m_sp = None then uses_val kval ~env;
  s

(* let varspace ~env vs = *)
(*   if vs = None then env.uses_curr_varspace <- true; *)
(*   VID.gen_var_space vs *)

(* Returns an empty initial environment. *)
let empty_env ~dgfip_flags ~prog ~name =
  {
    dgfip_flags;
    prog;
    scopes = [];
    quit_label = Format.sprintf "label_%s" name;
    uses_quit_label = false;
    uses_curr_varspace = false;
    uses_saisie_def = false;
    uses_calculee_def = false;
    uses_base_def = false;
    uses_saisie_val = false;
    uses_calculee_val = false;
    uses_base_val = false;
  }

let label_id_of_var_name vname = Format.sprintf "label_%s" vname

let label_id_of_var (v : Com.Var.t) = label_id_of_var_name (Pos.unmark v.name)

(* Given an iterator variable, returns a unique scope identifier.
   Simply uses the variable name given we can't have nested iterators with
   the same variable name. This scope identifier also is the M identifier
   that can be used to reference nested scopes. *)
(* let scope_of_var (v : Com.Var.t) : string = Pos.unmark v.name *)

(* Creates a fresh scope label from an iterator variable and adds it to
   the environment. *)
let fresh_scope =
  let cpt = ref 0 in
  fun ~env (var : Com.Var.t) ->
    let sid = Format.sprintf "%s_%i" (label_id_of_var var) !cpt in
    let new_scope = { sid; used_scope = false } in
    incr cpt;
    env.scopes <- Id new_scope :: env.scopes;
    new_scope

(* Adds a sanitizer to the stack of scopes. *)
let add_sanitizer =
  let cpt = ref 0 in
  fun ~env f ->
    let san_id = !cpt in
    incr cpt;
    env.scopes <- Sanitize { san_id; san = f } :: env.scopes;
    san_id

let pop_scope ~env ~scope_id =
  match env.scopes with
  | [] -> Format.ksprintf failwith "Pushing non existent scope %s" scope_id
  | Id { sid = i; _ } :: tl ->
      if i = scope_id then env.scopes <- tl
      else
        Format.ksprintf failwith "Pushing scope %s while top scope is %s"
          scope_id i
  | Sanitize { san_id; _ } :: _ ->
      Format.ksprintf failwith
        "Pushing scope %s, but top of stack is Sanitizer %i" scope_id san_id

let pop_sanitizer ~env ~sid =
  match env.scopes with
  | [] -> Format.ksprintf failwith "Pushing non existent Sanitizr %i" sid
  | Id { sid = i; _ } :: _ ->
      Format.ksprintf failwith "Pushing sanitizer %i met scope %s" sid i
  | Sanitize { san_id; san } :: tl ->
      if sid = san_id then begin
        san ();
        env.scopes <- tl
      end
      else
        Format.ksprintf failwith "Pushing sanitizer %i while expected %i" sid
          san_id

let get_current_label env =
  List.find_map (function Id i -> Some i | _ -> None) env.scopes

let goto_current_label ~env =
  match get_current_label env with
  | None -> None
  | Some i ->
      i.used_scope <- true;
      Some (Format.sprintf "goto %s" i.sid)

(* Given a scope identifier, returns its C label name. *)
let get_label_from env scope_id =
  List.find_map
    (function
      | Id i
        when Strings.starts_with ~prefix:(label_id_of_var_name scope_id) i.sid
        ->
          Some i
      | _ -> None)
    env.scopes

let goto_label_from ~env ~scope_id =
  match get_label_from env scope_id with
  | None -> None
  | Some i ->
      i.used_scope <- true;
      Some (Format.sprintf "goto %s" i.sid)

(* Sanitize the current scope, i.e. calls all the sanitizers stored
   in the scope stack. If an id is given [up_to], sanitize up to
   the given scope. *)
let sanitize ~env ~up_to =
  let stop_at_id =
    match up_to with
    | `Bottom -> fun _ -> false
    | `NextId -> fun _ -> true
    | `Id i ->
        let prefix = label_id_of_var_name i in
        fun i' -> Strings.starts_with ~prefix i'
  in
  let rec loop = function
    | [] -> (
        match up_to with
        | `Bottom -> ()
        | `NextId -> failwith "Sanitizing outside scope"
        | `Id u ->
            Format.ksprintf failwith "Sanitizing outside scope up to %s" u)
    | Sanitize { san; _ } :: tl ->
        san ();
        loop tl
    | Id i :: tl -> if not (stop_at_id i.sid) then loop tl
  in
  loop env.scopes

let goto_quit_label ~env =
  env.uses_quit_label <- true;
  Format.sprintf "goto %s" env.quit_label
