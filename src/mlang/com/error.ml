type typ = Anomaly | Discordance | Information

let compare_typ e1 e2 =
  match (e1, e2) with
  | Anomaly, (Discordance | Information) -> -1
  | (Discordance | Information), Anomaly -> 1
  | Information, Discordance -> -1
  | Discordance, Information -> 1
  | _ -> 0

type t = {
  name : string Pos.marked;
  famille : string Pos.marked;
  code_bo : string Pos.marked;
  sous_code : string Pos.marked;
  libelle : string Pos.marked;
  is_isf : string Pos.marked;
  typ : typ;
}

let pp_descr fmt err =
  Pp.fpr fmt "%s:%s:%s:%s:%s" (Pos.unmark err.famille) (Pos.unmark err.code_bo)
    (Pos.unmark err.sous_code) (Pos.unmark err.libelle) (Pos.unmark err.is_isf)

let pp fmt err = Pp.fpr fmt "%s:%a" (Pos.unmark err.name) pp_descr err

let compare (err1 : t) (err2 : t) = compare err1.name err2.name

type error_t = t

let error_pp = pp

let error_compare = compare

module Set = struct
  include SetExt.Make (struct
    type t = error_t

    let compare = error_compare
  end)

  let pp ?(sep = ", ") ?(pp_elt = error_pp) (_ : unit) (fmt : Format.formatter)
      (set : t) : unit =
    pp ~sep ~pp_elt () fmt set
end

module Map = struct
  include MapExt.Make (struct
    type t = error_t

    let compare = error_compare
  end)

  let pp ?(sep = "; ") ?(pp_key = error_pp) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end
