(** Here are all the types a value can have. Used for the M function [type]. *)
type value_typ =
  | Boolean
  | DateYear
  | DateDayMonthYear
  | DateMonth
  | Integer
  | Real

(** {2 Variables} *)

(** Note: TGV stands for "Tableau Général des Variables". This is where the
    variables are declared. *)

type loc_tgv = {
  loc_cat : CatVar.loc;  (** Its category kind *)
  loc_idx : int;  (** TODO *)
  loc_tab_idx : int;  (** TODO *)
  loc_cat_id : CatVar.t;  (** Full details on its category *)
  loc_cat_str : string;  (** String representation of its category *)
  loc_cat_idx : int;  (** The variable's identifier in its category. *)
}
(** Location of a TGV variable in the compiler's context. *)

type loc_tmp = {
  loc_idx : int;  (** TODO *)
  loc_tab_idx : int;  (** TODO *)
  loc_cat_idx : int;  (** The variable's identifier in its category *)
}
(** Location of a temporary variable in the compiler's context. *)

(** The different kinds of variables; the 'string' is the variable's name. *)
type loc =
  | LocTgv of string * loc_tgv  (** A TGV variable *)
  | LocTmp of string * loc_tmp  (** A temporary variable *)
  | LocRef of string * int  (** A reference (the integer is the 'loc_idx') *)

type event_field = {
  name : string Pos.marked;  (** The field name *)
  index : int;  (** Position of the event field. Set in the validator. *)
  is_var : bool;  (** Is a reference to a variable or a value? *)
}
(** Data on an event field. *)

type ('n, 'v) event_value =
  | Numeric of 'n
  | RefVar of 'v  (** The values of an event. *)

module DomainId : StrSet.T

module DomainIdSet :
  SetSetExt.T with type base_elt = string and type elt = DomainId.t

module DomainIdMap : MapExt.T with type key = DomainId.t

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

type variable_space = {
  vs_id : int;
  vs_name : string Pos.marked;
  vs_cats : CatVar.loc Pos.marked CatVar.LocMap.t;
  vs_by_default : bool;
}

type verif_domain = verif_domain_data domain

(** {2 Values and expressions} *)

(* Careful, the link may be unstable! *)
(** For a complete documentation on values and simple expressions semantics,
    check {{:../../../../../syntax.html#valeurs}the full documentation}. *)

(** A literal can either be a float value or undefined. *)
type literal = Float of float | Undefined

type origin = string Pos.marked option

type literal_with_orig = { lit : literal; origin : origin }
(** As constants are replaced by their literal value at some point in the code
    expansion, this allows to track whether the literal was written as-if, or if
    it originated from a constant. *)

(** Unary operators *)
type unop = Not | Minus

(** Binary operators. *)
type binop = And | Or | Add | Sub | Mul | Div | Mod

(** Comparison operators. Comparison always return 0 or 1 when the compared
    values are both defined; otherwise, it returns 'undefined'.

    Note that this is even true when comparing undefined with undefined: the
    proper way to test if a value is undefined is through the function
    'PresentFunc' (present). *)
type comp_op = Gt | Gte | Lt | Lte | Eq | Neq

(** Functions callable in M. There is only a subset of the whole M functions;
    some are translated into dedicated expressions at parsing (champ_evenement
    for example is directly transformed into an {!access} to a variable). *)
type func =
  | SumFunc  (** Sums the arguments *)
  | AbsFunc  (** Absolute value *)
  | MinFunc  (** Minimum of a list of values *)
  | MaxFunc  (** Maximum of a list of values *)
  | GtzFunc  (** Greater than zero (strict) ? *)
  | GtezFunc  (** Greater or equal than zero ? *)
  | NullFunc  (** Equal to zero *)
  | ArrFunc  (** Round to nearest integer *)
  | InfFunc  (** Truncate to integer *)
  | PresentFunc  (** Different than undefined *)
  | Multimax  (** Max of a subset of a table *)
  | Supzero  (** Identity if > 0, undefined otherwise *)
  | VerifNumber  (** -- unsupported -- *)
  | ComplNumber  (** -- unsupported -- *)
  | NbEvents  (** Total number of events *)
  | Func of string  (** A M function *)

(** The M language has an extremely odd way to specify looping. Rather than
    having first-class local mutable variables whose value change at each loop
    iteration, the M language prefers to use the changing loop parameter to
    instantiate the variable names inside the loop. For instance,

    {v somme(i=1..9:Xi) v}

    should evaluate to the sum of variables [X1], [X2], etc. Parameters can be
    number or characters and there can be multiple of them. We have to store all
    this information. *)

type var_name_generic = { base : string; parameters : char list }
(** For generic variables, we record the list of their lowercase parameters *)

(** A variable is either generic (with loop parameters) or normal *)
type var_name = Normal of string | Generic of var_name_generic

type m_var_name = var_name Pos.marked

type var_space = (m_var_name * int) option
(** The prefix of a variable that defines its space. No space is equivalent to
    the default space. *)

type 'v var_id = var_space * 'v

(** A generic representation of an access to a variable, read or write. *)
type 'v access =
  | VarAccess of 'v var_id  (** Simple variable occurence *)
  | TabAccess of 'v var_id * 'v m_expression  (** Access to a cell of a table *)
  | FieldAccess of var_space * 'v m_expression * string Pos.marked * int
      (** Call to 'champ_evenement' *)

and 'v m_access = 'v access Pos.marked

and 'v case =
  | CDefault
  | CValue of literal
  | CVar of 'v m_access  (** Switch cases *)
(* TODO: add location *)

(** Values that can be substituted for loop parameters *)
and 'v atom = AtomVar of 'v | AtomLiteral of literal_with_orig

and 'v loop_range =
  | Single of 'v atom Pos.marked
  | Range of 'v atom Pos.marked * 'v atom Pos.marked
  | Interval of 'v atom Pos.marked * 'v atom Pos.marked

and 'v loop_variable = char Pos.marked * 'v loop_range list
(** A loop variable is the character that should be substituted in variable
    names inside the loop plus the set of value to substitute. *)

(** There are two kind of loop variables declaration, but they are semantically
    the same though they have different concrete syntax. *)
and 'v loop_variables =
  | ValueSets of 'v loop_variable list
  | Ranges of 'v loop_variable list

(** A set of values. *)
and 'v set_value =
  | FloatValue of float Pos.marked  (** A literal singleton *)
  | VarValue of 'v m_access  (** A variable singleton *)
  | IntervalValue of int Pos.marked * int Pos.marked  (** A literal interval *)

(** Basic M expressions. *)
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

val mk_atomlit : ?from_const:string Pos.marked -> literal -> 'v atom
(** [mk_atomtit ?constname lit] makes a Literal atom with the name of the const
    as origin if provided. *)

val mk_lit : ?from_const:string Pos.marked -> literal -> 'v expression
(** [mk_atomtit ?constname lit] makes a Literal expression with the name of the
    const as origin if provided. *)

(** {2 Printing in M} *)

(** The print function is treated as a dedicated instruction. *)

(** Where to print *)
type print_std = StdOut | StdErr

(** How to print a variable *)
type print_info = Name | Alias

(** Something to print *)
type 'v print_arg =
  | PrintString of string
  | PrintAccess of print_info * 'v m_access
  | PrintIndent of 'v m_expression
      (** Evaluates the expression and indents as much *)
  | PrintExpr of 'v m_expression * int * int
      (** Prints an expression with a minimal and maximal precision. *)

(** {2 Loops} *)

(** In the M language, you can define multiple variables at once. This is the
    way they do looping since the definition can depend on the loop variable
    value (e.g [Xi] can depend on [i]). *)

type 'v formula_decl =
  | VarDecl of 'v access Pos.marked * 'v m_expression
  | EventFieldRef of 'v m_expression * string Pos.marked * int * 'v

type 'v formula =
  | SingleFormula of 'v formula_decl
  | MultipleFormulaes of 'v loop_variables Pos.marked * 'v formula_decl

(** {2 Stopping} *)

type stop_kind =
  | SKApplication  (** Leave the whole application *)
  | SKTarget  (** Leave the current target *)
  | SKFun  (** Leave the current function *)
  | SKId of string option
      (** Leave the iterator with the selected var (or the current if [None]) *)

(** {2 Instructions} *)

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
  | Iterate_values of
      'v
      * ('v m_expression * 'v m_expression * 'v m_expression) list
      * ('v, 'e) m_instruction list
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
      (** [true] <=> the target can be stopped by a 'stop' instruction *)
}
(** A target is a list of instructions. They are very similar to rules, except
    targets are entrypoints of the M program. *)
(* TODO: target doc *)

(** {2 Utils} *)
(* TODO: doc *)

val target_is_function : ('v, 'e) target -> bool

val get_var_name : var_name -> string

val get_normal_var : var_name -> string

val function_arity : func -> int option
(** Returns the arity of a function, or None if there is no limit. *)

val compare_value_typ : value_typ -> value_typ -> int

val compare_var_space : var_space -> var_space -> int
