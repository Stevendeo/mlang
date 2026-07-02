type t = Input of StrSet.t | Computed of { is_base : bool }

let all_inputs = Input (StrSet.one "*")

let is_input = function Input _ -> true | _ -> false

let is_computed = function Computed _ -> true | _ -> false

let pp fmt = function
  | Input id ->
      let pp fmt set = StrSet.iter (Format.fprintf fmt " %s") set in
      Format.fprintf fmt "saisie%a" pp id
  | Computed id ->
      Format.fprintf fmt "calculee%s" (if id.is_base then " base" else "")

let compare a b =
  match (a, b) with
  | Input _, Computed _ -> 1
  | Computed _, Input _ -> -1
  | Input id0, Input id1 -> StrSet.compare id0 id1
  | Computed c0, Computed c1 -> compare c0.is_base c1.is_base

type cat_var_t = t

let cat_var_pp = pp

let cat_var_compare = compare

module Set = struct
  include SetExt.Make (struct
    type t = cat_var_t

    let compare = cat_var_compare
  end)

  let pp ?(sep = ", ") ?(pp_elt = cat_var_pp) (_ : unit)
      (fmt : Format.formatter) (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module Map = struct
  include MapExt.Make (struct
    type t = cat_var_t

    let compare = cat_var_compare
  end)

  let pp ?(sep = "; ") ?(pp_key = cat_var_pp) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map

  let from_string_list = function
    | Pos.Mark ([ Pos.Mark ("*", _) ], id_pos) ->
        one all_inputs id_pos
        |> add (Computed { is_base = false }) id_pos
        |> add (Computed { is_base = true }) id_pos
    | Pos.Mark ([ Pos.Mark ("saisie", _); Pos.Mark ("*", _) ], id_pos) ->
        one all_inputs id_pos
    | Pos.Mark (Pos.Mark ("saisie", _) :: id, id_pos) ->
        one (Input (StrSet.from_marked_list id)) id_pos
    | Pos.Mark (Pos.Mark ("calculee", _) :: id, id_pos) -> (
        match id with
        | [] -> one (Computed { is_base = false }) id_pos
        | [ Pos.Mark ("base", _) ] -> one (Computed { is_base = true }) id_pos
        | [ Pos.Mark ("*", _) ] ->
            one (Computed { is_base = false }) id_pos
            |> add (Computed { is_base = true }) id_pos
        | _ -> Errors.raise_spanned_error "invalid variable category" id_pos)
    | Pos.Mark (_, id_pos) ->
        Errors.raise_spanned_error "invalid variable category" id_pos
end

type loc = LocComputed | LocBase | LocInput

let pp_loc oc = function
  | LocInput -> Pp.fpr oc "input"
  | LocComputed -> Pp.fpr oc "computed"
  | LocBase -> Pp.fpr oc "base"

module LocSet = struct
  include SetExt.Make (struct
    type t = loc

    let compare = Stdlib.compare
  end)

  let pp ?(sep = ", ") ?(pp_elt = pp_loc) (_ : unit) (fmt : Format.formatter)
      (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module LocMap = struct
  include MapExt.Make (struct
    type t = loc

    let compare = Stdlib.compare
  end)

  let pp ?(sep = "; ") ?(pp_key = pp_loc) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type data = {
  id : t;
  id_str : string;
  id_int : int;
  loc : loc;
  pos : Pos.t;
  attributs : Pos.t StrMap.t;
}
