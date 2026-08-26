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

module VID = Dgfip_varid

type dflag = Def | Val | VarInfo | VarSpace

type local_var =
  | Anon (* inlined sub-expression, not intended for reuse *)
  | Refered of int
(* declared local variable, either M local or locally bound in the constructors
   below *)

(** Contructors used for building C instructions. *)
type t =
  | True
  | False
  | Lit of float
  | M of Com.var_space * Com.Var.t * dflag
  | Local of local_var
  | And of t * t
  | Or of t * t
  | Not of t
  | Minus of t
  | Plus of t * t
  | Sub of t * t
  | Mult of t * t
  | Div of t * t
  | Modulo of t * t
  | Comp of string * t * t
  | Fun of string * t list
  | Varinfo of Dgfip_varid.varinfo
  | Varinfo_tab of Dgfip_varid.varinfo * t * t
  | Varinfo_field of t * t * string
  | Varspace_current of Com.var_space
  | Varspace_of of Com.var_space * Dgfip_varid.varinfo
  | Typ of Com.value_typ
  | Instr of string
  | Direct of t
  | Ite of t * t * t
  | It0 of t * t
  | Let_local of local_var * t * t

module Print = struct
  let dflag ppf = function
    | Def -> Pp.string ppf "def"
    | Val -> Pp.string ppf "val"
    | VarInfo -> Pp.string ppf "varinfo"
    | VarSpace -> Pp.string ppf "varspace"

  let local_var ppf = function
    | Anon -> Pp.string ppf "_anon_"
    | Refered index -> Pp.fpr ppf "l%d" index

  let varinfo ppf (var : VID.varinfo) = Pp.string ppf (Com.Var.name_str var)

  let var_space ppf = function
    | None -> Pp.string ppf "@default"
    | Some (name, _) -> Pp.fpr ppf "@%s" (Com.get_var_name (Pos.unmark name))

  let rec t ppf = function
    | True -> Pp.string ppf "True"
    | False -> Pp.string ppf "False"
    | Lit value -> Pp.fpr ppf "Lit %g" value
    | M (space, var, kind) ->
        Pp.fpr ppf "M(%a%a, %a)" varinfo var var_space space dflag kind
    | Local local -> Pp.fpr ppf "Local(%a)" local_var local
    | And (left, right) -> node2 ppf "And" left right
    | Or (left, right) -> node2 ppf "Or" left right
    | Not expr -> node1 ppf "Not" expr
    | Minus expr -> node1 ppf "Minus" expr
    | Plus (left, right) -> node2 ppf "Plus" left right
    | Sub (left, right) -> node2 ppf "Sub" left right
    | Mult (left, right) -> node2 ppf "Mult" left right
    | Div (left, right) -> node2 ppf "Div" left right
    | Modulo (left, right) -> node2 ppf "Modulo" left right
    | Comp (op, left, right) ->
        Pp.fpr ppf "@[<hv 2>Comp(%s,@ %a,@ %a)@]" op t left t right
    | Fun (name, args) ->
        Pp.fpr ppf "@[<hv 2>Fun(%s,@ [%a])@]" name (Pp.list ",@ " t) args
    | Varinfo var -> Pp.fpr ppf "Varinfo(%a)" varinfo var
    | Varinfo_tab (var, def, value) ->
        Pp.fpr ppf "@[<hv 2>Varinfo_tab(%a,@ %a,@ %a)@]" varinfo var t def t
          value
    | Varinfo_field (def, value, field) ->
        Pp.fpr ppf "@[<hv 2>Varinfo_field(%a,@ %a,@ %s)@]" t def t value field
    | Varspace_current space ->
        Pp.fpr ppf "Varspace_current(%a)" var_space space
    | Varspace_of (space, var) ->
        Pp.fpr ppf "@[<hv 2>Varspace_of(%a,@ %a)@]" var_space space varinfo var
    | Typ typ -> Pp.fpr ppf "Typ(%a)" Com.Pretty.format_value_typ typ
    | Instr text -> Pp.fpr ppf "Instr(%S)" text
    | Direct expr -> node1 ppf "Direct" expr
    | Ite (cond, then_, else_) ->
        Pp.fpr ppf "@[<hv 2>Ite(%a,@ %a,@ %a)@]" t cond t then_ t else_
    | It0 (cond, then_) -> node2 ppf "It0" cond then_
    | Let_local (local, bound, body) ->
        Pp.fpr ppf "@[<hv 2>Let_local(%a =@ %a,@ %a)@]" local_var local t bound
          t body

  and node1 ppf name expr = Pp.fpr ppf "@[<hv 2>%s(%a)@]" name t expr

  and node2 ppf name left right =
    Pp.fpr ppf "@[<hv 2>%s(%a,@ %a)@]" name t left t right

  let to_string expr = Pp.spr "%a" t expr
end

let irdata = Direct (Instr "irdata")

let dflag_id = function Def -> 0 | Val -> 1 | VarInfo -> 2 | VarSpace -> 3

let typ_id = function
  | Com.Boolean -> 0
  | DateYear -> 1
  | DateDayMonthYear -> 2
  | DateMonth -> 3
  | Integer -> 4
  | Real -> 5

let compare_dflag d d' = Int.compare (dflag_id d) (dflag_id d')

let compare_typ t t' = Int.compare (typ_id t) (typ_id t')

let compare_local_var lv lv' =
  match (lv, lv') with
  | Anon, Anon -> 0
  | Refered i, Refered i' -> Int.compare i i'
  | Anon, _ -> 1
  | _, Anon -> -1

(** Operator to aggregate comparisons *)
let ( >< ) i j = if i <> 0 then i else j

let compare_varspace =
  Option.compare (fun (Pos.Mark (i, _), _) (Pos.Mark (j, _), _) ->
      String.compare (Com.get_var_name i) (Com.get_var_name j))

let rec compare (c1 : t) (c2 : t) : int =
  match (c1, c2) with
  | True, True | False, False -> 0
  | Lit f, Lit f' -> Float.compare f f'
  | M (vs, v, d), M (vs', v', d') ->
      Int.compare v.id v'.id >< compare_dflag d d' >< compare_varspace vs vs'
  (* | Local lv, Local lv' -> compare_local_var lv lv' *)
  | And (e1, e2), And (e1', e2')
  | Or (e1, e2), Or (e1', e2')
  | Plus (e1, e2), Plus (e1', e2')
  | Sub (e1, e2), Sub (e1', e2')
  | Mult (e1, e2), Mult (e1', e2')
  | Div (e1, e2), Div (e1', e2')
  | Modulo (e1, e2), Modulo (e1', e2') ->
      compare e1 e1' >< compare e2 e2'
  | Not e, Not e' | Minus e, Minus e' -> compare e e'
  | Comp (s, e1, e2), Comp (s', e1', e2') ->
      String.compare s s' >< compare e1 e1' >< compare e2 e2'
  | Fun (s, l), Fun (s', l') -> String.compare s s' >< List.compare compare l l'
  | Varinfo v, Varinfo v' -> Int.compare v.id v'.id
  | Varinfo_tab (vi, d, v), Varinfo_tab (vi', d', v') ->
      Int.compare vi.id vi'.id >< compare d d' >< compare v v'
  | Varinfo_field (d, v, s), Varinfo_field (d', v', s') ->
      compare d d' >< compare v v' >< String.compare s s'
  | Varspace_current vs, Varspace_current vs' -> compare_varspace vs vs'
  | Varspace_of (vs, vi), Varspace_of (vs', vi') ->
      compare_varspace vs vs' >< Int.compare vi.id vi'.id
  | Typ t, Typ t' -> compare_typ t t'
  | Instr s, Instr s' -> String.compare s s'
  | Direct t, Direct t' -> compare t t'
  | Ite (c, t, e), Ite (c', t', e') ->
      compare c c' >< compare t t' >< compare e e'
  | It0 (c, t), It0 (c', t') -> compare c c' >< compare t t'
  | Let_local (lv, e1, e2), Let_local (lv', e1', e2') ->
      compare_local_var lv lv' >< compare e1 e1' >< compare e2 e2'
  | True, _ -> 1
  | _, True -> -1
  | False, _ -> 1
  | _, False -> -1
  | Lit _, _ -> 1
  | _, Lit _ -> -1
  | M _, _ -> 1
  | _, M _ -> -1
  | Local _, _ -> 1
  | _, Local _ -> -1
  | And _, _ -> 1
  | _, And _ -> -1
  | Or _, _ -> 1
  | _, Or _ -> -1
  | Not _, _ -> 1
  | _, Not _ -> -1
  | Minus _, _ -> 1
  | _, Minus _ -> -1
  | Plus _, _ -> 1
  | _, Plus _ -> -1
  | Sub _, _ -> 1
  | _, Sub _ -> -1
  | Mult _, _ -> 1
  | _, Mult _ -> -1
  | Div _, _ -> 1
  | _, Div _ -> -1
  | Modulo _, _ -> 1
  | _, Modulo _ -> -1
  | Comp _, _ -> 1
  | _, Comp _ -> -1
  | Fun _, _ -> 1
  | _, Fun _ -> -1
  | Varinfo _, _ -> 1
  | _, Varinfo _ -> -1
  | Varinfo_tab _, _ -> 1
  | _, Varinfo_tab _ -> -1
  | Varinfo_field _, _ -> 1
  | _, Varinfo_field _ -> -1
  | Varspace_current _, _ -> 1
  | _, Varspace_current _ -> -1
  | Varspace_of _, _ -> 1
  | _, Varspace_of _ -> -1
  | Typ _, _ -> 1
  | _, Typ _ -> -1
  | Instr _, _ -> 1
  | _, Instr _ -> -1
  | Direct _, _ -> 1
  | _, Direct _ -> -1
  | Ite _, _ -> 1
  | _, Ite _ -> -1
  | It0 _, _ -> 1
  | _, It0 _ -> -1
(* | Let_local _, _ -> 1 | _, Let_local _ -> -1  *)

(** smart constructors *)

let anon = Anon

let locals_from_m =
  let counter = ref 0 in
  let fresh_id () =
    let v = !counter in
    counter := !counter + 1;
    v
  in
  fun () ->
    let lvar_id = fresh_id () in
    (Refered (-(2 * lvar_id)), Refered (-((2 * lvar_id) + 1)))

let new_local : unit -> local_var =
  let c = ref 0 in
  fun () ->
    let i = !c in
    incr c;
    Refered i
