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
(** Here are all the types a value can have. Date types don't seem to be used at
    all though. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

type loc_tgv = {
  loc_cat : CatVar.loc;
  loc_idx : int;
  loc_tab_idx : int;
  loc_cat_id : CatVar.t;
  loc_cat_str : string;
  loc_cat_idx : int;
}

type loc_tmp = { loc_idx : int; loc_tab_idx : int; loc_cat_idx : int }

type loc =
  | LocTgv of string * loc_tgv
  | LocTmp of string * loc_tmp
  | LocRef of string * int

type event_field = { name : string Pos.marked; index : int; is_var : bool }

type ('n, 'v) event_value = Numeric of 'n | RefVar of 'v

module DomainId = StrSet

module DomainIdSet = struct
  include SetSetExt.Make (DomainId)

  module type T =
    SetSetExt.T with type base_elt = string and type elt = DomainId.t

  let pp ?(sep1 = ", ") ?(sep2 = " ") ?(pp_elt = Format.pp_print_string)
      (_ : unit) (fmt : Format.formatter) (setSet : t) : unit =
    pp ~sep1 ~sep2 ~pp_elt () fmt setSet
end

module DomainIdMap = struct
  include MapExt.Make (DomainId)

  module type T = MapExt.T with type key = DomainId.t

  let pp ?(sep = ", ") ?(pp_key = DomainId.pp ()) ?(assoc = " => ")
      (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (map : 'a t) : unit =
    pp ~sep ~pp_key ~assoc pp_val fmt map
end

type 'a domain = {
  dom_id : DomainId.t Pos.marked;
  dom_names : Pos.t DomainIdMap.t;
  dom_by_default : bool;
  dom_min : DomainIdSet.t;
  dom_max : DomainIdSet.t;
  dom_rov : IntSet.t;
  dom_data : 'a;
  dom_used : int Pos.marked option;
}

type rule_domain_data = { rdom_computable : bool }

type rule_domain = rule_domain_data domain

type verif_domain_data = {
  vdom_auth : Pos.t CatVar.Map.t;
  vdom_verifiable : bool;
}

type verif_domain = verif_domain_data domain

type variable_space = {
  vs_id : int;
  vs_name : string Pos.marked;
  vs_cats : CatVar.loc Pos.marked CatVar.LocMap.t;
  vs_by_default : bool;
}

type literal = Float of float | Undefined

type origin = string Pos.marked option

type literal_with_orig = { lit : literal; origin : origin }

(** Unary operators *)
type unop = Not | Minus

(** Binary operators *)
type binop = And | Or | Add | Sub | Mul | Div | Mod

(** Comparison operators *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero ? *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than zero ? *)
  | Multimax  (** ??? *)
  | Supzero  (** ??? *)
  | VerifNumber
  | ComplNumber
  | NbEvents
  | Func of string

type var_name_generic = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

(** A variable is either generic (with loop parameters) or normal *)
type var_name = Normal of string | Generic of var_name_generic

type m_var_name = var_name Pos.marked

type var_space = (m_var_name * int) option

type 'v var_id = var_space * 'v

type 'v access =
  | VarAccess of 'v var_id
  | TabAccess of 'v var_id * 'v m_expression
  | FieldAccess of var_space * 'v m_expression * string Pos.marked * int

and 'v m_access = 'v access Pos.marked

and 'v case = CDefault | CValue of literal | CVar of 'v m_access

and 'v atom = AtomVar of 'v | AtomLiteral of literal_with_orig

and 'v loop_range =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

and 'v loop_variable = char Pos.marked * 'v loop_range list

and 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

and 'v set_value =
  | FloatValue of float Pos.marked
  | VarValue of 'v m_access
  | IntervalValue of int Pos.marked * int Pos.marked

and 'v expression =
  | TestInSet of bool * 'v m_expression * 'v set_value list
      (** Test if an expression is in a set of value (or not in the set if the
          flag is set to [false]) *)
  | Unop of unop * 'v m_expression
  | Comparison of comp_op Pos.marked * 'v m_expression * 'v m_expression
  | Binop of binop Pos.marked * 'v m_expression * 'v m_expression
  | Conditional of 'v m_expression * 'v m_expression * 'v m_expression option
  | FuncCall of func Pos.marked * 'v m_expression list
  | FuncCallLoop of
      func Pos.marked * 'v loop_variables Pos.marked * 'v m_expression
  | Literal of literal_with_orig
  | Var of 'v access
  | Loop of 'v loop_variables Pos.marked * 'v m_expression
      (** The loop is prefixed with the loop variables declarations *)
  | NbCategory of Pos.t CatVar.Map.t
  | Attribut of 'v m_access * string Pos.marked
  | Size of 'v m_access
  | Type of 'v m_access * value_typ Pos.marked
  | SameVariable of 'v m_access * 'v m_access
  | InDomain of 'v m_access * Pos.t CatVar.Map.t
  | NbAnomalies
  | NbDiscordances
  | NbInformatives
  | NbBloquantes

and 'v m_expression = 'v expression Pos.marked

type const = { id : string; value : literal; pos : Pos.t }

type 'v dep =
  | Tab of 'v * 'v m_expression
  | V of 'v
  | LiteralDep of literal
  | Const of const

let get_used_variables (e : 'v expression) : 'v dep list =
  let rec get_used_variables_ (e : 'v expression) (acc : 'v dep list) =
    match e with
    | TestInSet (_, Mark (e, _), _) | Unop (_, Mark (e, _)) ->
        get_used_variables_ e acc
    | Comparison (_, Mark (e1, _), Mark (e2, _))
    | Binop (_, Mark (e1, _), Mark (e2, _)) ->
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        acc
    | Conditional (Mark (e1, _), Mark (e2, _), e3) -> (
        let acc = get_used_variables_ e1 acc in
        let acc = get_used_variables_ e2 acc in
        match e3 with
        | None -> acc
        | Some (Mark (e3, _)) -> get_used_variables_ e3 acc)
    | FuncCall (_, args) ->
        List.fold_left
          (fun acc arg -> get_used_variables_ (Pos.unmark arg) acc)
          acc args
    | Loop (_, Mark (e, _)) -> get_used_variables_ e acc
    | FuncCallLoop (_, _, Mark (e, _)) -> get_used_variables_ e acc
    | Var var
    | Size (Mark (var, _))
    | Attribut (Mark (var, _), _)
    | InDomain (Mark (var, _), _)
    | Type (Mark (var, _), _) ->
        get_used_variables_access var acc
    | Literal { lit; origin = Some (Mark (id, pos)) } ->
        Const { id; value = lit; pos } :: acc
    | Literal { lit; origin = None } -> LiteralDep lit :: acc
    | SameVariable (Mark (l, _), Mark (r, _)) ->
        let acc = get_used_variables_access l acc in
        get_used_variables_access r acc
    | NbCategory _ | NbAnomalies | NbDiscordances | NbInformatives -> acc
    | NbBloquantes -> acc
  and get_used_variables_access var acc =
    match var with
    | TabAccess ((_, v), m_i) -> Tab (v, m_i) :: acc
    | VarAccess (_, v) -> V v :: acc
    | FieldAccess (_, Mark (v, _), _, _) -> get_used_variables_ v acc
  in
  get_used_variables_ e []

let mk_lit_with_orig lit origin = { lit; origin }

let mk_lit ?from_const lit = Literal (mk_lit_with_orig lit from_const)

let mk_atomlit ?from_const lit = AtomLiteral (mk_lit_with_orig lit from_const)

type print_std = StdOut | StdErr

type print_info = Name | Alias

type 'v print_arg =
  | PrintString of string
  | PrintAccess of print_info * 'v m_access
  | PrintIndent of 'v m_expression
  | PrintExpr of 'v m_expression * int * int

type 'v formula_decl =
  | VarDecl of 'v access Pos.marked * 'v m_expression
  | EventFieldRef of 'v m_expression * string Pos.marked * int * 'v

type 'v formula =
  | SingleFormula of 'v formula_decl
  | MultipleFormulaes of 'v loop_variables Pos.marked * 'v formula_decl

type stop_kind =
  | SKApplication (* Leave the whole application *)
  | SKTarget (* Leave the current target *)
  | SKFun (* Leave the current function *)
  | SKId of string option
(* Leave the iterator with the selected var
   (or the current if [None]) *)

type 'v switch_expression =
  | SEValue of 'v m_expression
  | SESameVariable of 'v m_access

type ('v, 'e) instruction =
  | Affectation of 'v formula Pos.marked
  | IfThenElse of
      'v m_expression
      * ('v, 'e) m_instruction list
      * ('v, 'e) m_instruction list
  | WhenDoElse of
      ('v m_expression * ('v, 'e) m_instruction list * Pos.t) list
      * ('v, 'e) m_instruction list Pos.marked
  | ComputeDomain of string Pos.marked list Pos.marked * var_space
  | ComputeChaining of string Pos.marked * var_space
  | ComputeVerifs of
      string Pos.marked list Pos.marked * 'v m_expression * var_space
  | ComputeTarget of string Pos.marked * 'v m_access list * var_space
  | VerifBlock of ('v, 'e) m_instruction list
  | Print of print_std * 'v print_arg Pos.marked list
  | Iterate of
      'v
      * 'v m_access list
      * (Pos.t CatVar.Map.t * 'v m_expression * var_space) list
      * ('v, 'e) m_instruction list
    (* iterer variable <v> : <explicit vars> / <category filters> in (...) *)
  | Iterate_values of
      'v
      * ('v m_expression * 'v m_expression * 'v m_expression) list
      * ('v, 'e) m_instruction list
    (* iterer variable <v> : <intervals> in (...) *)
  | Restore of
      'v m_access list
      * ('v * Pos.t CatVar.Map.t * 'v m_expression * var_space) list
      * 'v m_expression list
      * ('v * 'v m_expression) list
      * ('v, 'e) m_instruction list
  | ArrangeEvents of
      ('v * 'v * 'v m_expression) option
      * ('v * 'v m_expression) option
      * 'v m_expression option
      * ('v, 'e) m_instruction list
  | Switch of
      ('v switch_expression * ('v case list * ('v, 'e) m_instruction list) list)
  | RaiseError of 'e Pos.marked * string Pos.marked option
  | CleanErrors
  | CleanFinalizedErrors
  | ExportErrors
  | FinalizeErrors
  | Stop of stop_kind

and ('v, 'e) m_instruction = ('v, 'e) instruction Pos.marked

type ('v, 'e) target = {
  target_name : string Pos.marked;
  target_file : string option;
  target_apps : string Pos.marked StrMap.t;
  target_args : 'v list;
  target_result : 'v option;
  target_tmp_vars : 'v StrMap.t;
  target_nb_tmps : int;
  target_sz_tmps : int;
  target_nb_refs : int;
  target_prog : ('v, 'e) m_instruction list;
  target_stoppable : bool;
}

let target_is_function t = t.target_result <> None

let get_var_name v = match v with Normal s -> s | Generic s -> s.base

let get_normal_var = function Normal name -> name | Generic _ -> assert false

let function_arity = function
  | SumFunc -> None
  | AbsFunc -> Some 1
  | MinFunc -> Some 2
  | MaxFunc -> Some 2
  | GtzFunc -> Some 1
  | GtezFunc -> Some 1
  | NullFunc -> Some 1
  | ArrFunc -> Some 1
  | InfFunc -> Some 1
  | PresentFunc -> Some 1
  | Multimax -> Some 2
  | Supzero -> Some 1
  | VerifNumber -> Some 0
  | ComplNumber -> Some 0
  | NbEvents -> Some 0
  | Func _ -> None

let value_typ_id = function
  | Boolean -> 0
  | DateYear -> 1
  | DateDayMonthYear -> 2
  | DateMonth -> 3
  | Integer -> 4
  | Real -> 5

let compare_value_typ (t : value_typ) (t' : value_typ) =
  Int.compare (value_typ_id t) (value_typ_id t')

let compare_var_space (vs : var_space) (vs' : var_space) =
  Option.compare
    (fun (n, id) (n', id') ->
      let str =
        String.compare
          (get_var_name @@ Pos.unmark n)
          (get_var_name @@ Pos.unmark n')
      in
      if str = 0 then Int.compare id id' else str)
    vs vs'
