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
open Comcom

type id = int

let id_cpt = ref 0

let new_id () =
  let id = !id_cpt in
  incr id_cpt;
  id

type tgv = {
  table : id Array.t option;
  alias : string Pos.marked option;  (** Input variable have an alias *)
  descr : string Pos.marked;
      (** Description taken from the variable declaration *)
  attrs : int Pos.marked StrMap.t;
  cat : CatVar.t;
  is_given_back : bool;
  typ : value_typ option;
  table_cell : (id * int) option;
}

and scope = Tgv of tgv | Temp of id Array.t option | Ref

and t = {
  name : string Pos.marked;  (** The position is the variable declaration *)
  id : id;
  loc : loc;
  scope : scope;
}

let tgv v =
  match v.scope with
  | Tgv s -> s
  | _ ->
      let msg = Pp.spr "%s is not a TGV variable" (Pos.unmark v.name) in
      Errors.raise_error msg

let name v = v.name

let name_str v = Pos.unmark v.name

let get_table v =
  match v.scope with Tgv tgv -> tgv.table | Temp table -> table | Ref -> None

let get_table_cell v =
  match v.scope with Tgv tgv -> tgv.table_cell | _ -> None

let set_table_cell v ~id:tabvar ~idx:index =
  let tgv = tgv v in
  let tgv = { tgv with table_cell = Some (tabvar, index) } in
  let scope = Tgv tgv in
  { v with scope }

let is_table v = get_table v <> None

let set_table v table =
  match v.scope with
  | Tgv tgv -> { v with scope = Tgv { tgv with table } }
  | Temp _ -> { v with scope = Temp table }
  | Ref -> v

let cat_var_loc v =
  match v.scope with
  | Tgv tgv -> (
      match tgv.cat with
      | CatVar.Input _ -> CatVar.LocInput
      | Computed { is_base } when is_base -> CatVar.LocBase
      | Computed _ -> CatVar.LocComputed)
  | Temp _ | Ref -> failwith "not a TGV variable"

let size v = match get_table v with None -> 1 | Some tab -> Array.length tab

let alias v = match v.scope with Tgv s -> s.alias | _ -> None

let alias_str v =
  match v.scope with
  | Tgv s -> Option.fold ~none:"" ~some:Pos.unmark s.alias
  | _ -> ""

let descr v = (tgv v).descr

let descr_str v = Pos.unmark (tgv v).descr

let attrs v = (tgv v).attrs

let cat v = (tgv v).cat

let typ v = (tgv v).typ

let is_given_back v = (tgv v).is_given_back

let loc_tgv v =
  match v.loc with
  | LocTgv (_, l) -> l
  | _ ->
      let msg = Pp.spr "%s is not a TGV variable" (Pos.unmark v.name) in
      Errors.raise_error msg

let loc_cat_idx v =
  match v.loc with
  | LocTgv (_, tgv) -> tgv.loc_cat_idx
  | LocTmp (_, tmp) -> tmp.loc_cat_idx
  | LocRef (_, li) -> li

let set_loc_tgv_idx v (cv : CatVar.data) i =
  match v.loc with
  | LocTgv (id, tgv) ->
      let loc_cat = cv.loc in
      let loc_cat_str = cv.id_str in
      let tgv = { tgv with loc_cat; loc_cat_str; loc_cat_idx = i } in
      { v with loc = LocTgv (id, tgv) }
  | LocTmp (id, _) | LocRef (id, _) ->
      Errors.raise_error (Pp.spr "%s has not a TGV location" id)

let set_loc_tmp_idx v i =
  match v.loc with
  | LocTmp (id, tmp) ->
      let tmp = { tmp with loc_cat_idx = i } in
      { v with loc = LocTmp (id, tmp) }
  | LocTgv (id, _) | LocRef (id, _) ->
      Errors.raise_error (Pp.spr "%s has not a TGV location" id)

let loc_idx v =
  match v.loc with
  | LocTgv (_, tgv) -> tgv.loc_idx
  | LocTmp (_, tmp) -> tmp.loc_idx
  | LocRef (_, li) -> li

let set_loc_idx v loc_idx =
  let loc =
    match v.loc with
    | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_idx })
    | LocTmp (id, tmp) -> LocTmp (id, { tmp with loc_idx })
    | LocRef (id, _) -> LocRef (id, loc_idx)
  in
  { v with loc }

let loc_tab_idx v =
  match v.loc with
  | LocTgv (_, tgv) -> tgv.loc_tab_idx
  | LocTmp (_, tmp) -> tmp.loc_tab_idx
  | LocRef (id, _) ->
      let msg = Pp.spr "variable %s cannot be a table" id in
      Errors.raise_error msg

let set_loc_tab_idx v loc_tab_idx =
  let loc =
    match v.loc with
    | LocTgv (id, tgv) -> LocTgv (id, { tgv with loc_tab_idx })
    | LocTmp (id, tmp) -> LocTmp (id, { tmp with loc_tab_idx })
    | LocRef (id, _) ->
        let msg = Pp.spr "variable %s cannot be a table" id in
        Errors.raise_error msg
  in
  { v with loc }

let is_tgv v = match v.scope with Tgv _ -> true | _ -> false

let is_temp v = match v.scope with Temp _ -> true | _ -> false

let is_ref v = v.scope = Ref

let init_loc loc_cat_id =
  {
    loc_cat = CatVar.LocInput;
    loc_idx = 0;
    loc_tab_idx = -1;
    loc_cat_id;
    loc_cat_str = "";
    loc_cat_idx = 0;
  }

let new_tgv ~(name : string Pos.marked) ~(table : id Array.t option)
    ~(is_given_back : bool) ~(alias : string Pos.marked option)
    ~(descr : string Pos.marked) ~(attrs : int Pos.marked StrMap.t)
    ~(cat : CatVar.t) ~(typ : value_typ option)
    ~(table_cell : (id * int) option) : t =
  {
    name;
    id = new_id ();
    loc = LocTgv (Pos.unmark name, init_loc cat);
    scope =
      Tgv { table; alias; descr; attrs; cat; is_given_back; typ; table_cell };
  }

let new_temp ~(name : string Pos.marked) ~(table : id Array.t option) : t =
  let loc =
    LocTmp
      (Pos.unmark name, { loc_idx = -1; loc_tab_idx = -1; loc_cat_idx = -1 })
  in
  { name; id = new_id (); loc; scope = Temp table }

let new_ref ~(name : string Pos.marked) : t =
  let loc = LocRef (Pos.unmark name, -1) in
  { name; id = new_id (); loc; scope = Ref }

let new_arg ~(name : string Pos.marked) : t = new_temp ~name ~table:None

let new_res ~(name : string Pos.marked) : t = new_temp ~name ~table:None

let int_of_scope = function Tgv _ -> 0 | Temp _ -> 1 | Ref -> 2

let compare (var1 : t) (var2 : t) =
  let c = compare (int_of_scope var1.scope) (int_of_scope var2.scope) in
  if c <> 0 then c
  else
    let c = compare (Pos.unmark var1.name) (Pos.unmark var2.name) in
    if c <> 0 then c else compare var1.id var2.id

let pp fmt (v : t) = Format.fprintf fmt "(%d)%s" v.id (Pos.unmark v.name)

type t_var = t

let pp_var = pp

let compare_var v0 v1 = compare v0 v1

module Set = struct
  include SetExt.Make (struct
    type t = t_var

    let compare = compare_var
  end)

  let pp ?(sep = ", ") ?(pp_elt = pp_var) (_ : unit) (fmt : Format.formatter)
      (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module Map = struct
  include MapExt.Make (struct
    type t = t_var

    let compare = compare_var
  end)

  let pp ?(sep = "; ") ?(pp_key = pp_var) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

(* let compare_name_ref = ref (fun _ _ -> assert false)

let compare_name n0 n1 = !compare_name_ref n0 n1*)
