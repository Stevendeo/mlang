(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2022 - 2026 DGFiP - INRIA                               *)
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
module C = Constr

let fresh_c_local =
  let c = ref 0 in
  fun name ->
    let s = name ^ string_of_int !c in
    incr c;
    s

let generate_variable ?(def_flag = false) ?(trace_flag = false) ~env
    (m_sp_opt : Com.var_space) (var : Com.Var.t) =
  try
    if def_flag then Env.gen_def ~env m_sp_opt var
    else
      let access_val = Env.gen_val ~env m_sp_opt var in
      (* When the trace flag is present, we print the value of the
         non-temporary variable being used *)
      if trace_flag && not (Com.Var.is_temp var) then
        let vn = Pos.unmark var.Com.Var.name in
        let pos_tgv = VID.gen_pos_from_start var in
        Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv access_val
      else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark var.Com.Var.name))

type dflag = Constr.dflag =
  | Def
  | Val
  | VarInfo
  | VarSpace
  | Irdata
  | Ptr of dflag

type local_var = Constr.local_var

type stack_slot = { kind : dflag; depth : int }

type stack_assignment = { slot : stack_slot; subexpr : expr }

and local_stacks = {
  def_top : int;
  val_top : int;
  var_top : int;
  spa_top : int;
  var_substs : (int * (expr * dflag)) list;
}

and local_vars = (local_var * stack_assignment) list

and expr =
  | Dtrue
  | Dfalse
  | Dlit of float
  | Dvar of expr_var
  | Dvarinfo of varinfo_access
  | Dvarspace of Com.var_space * Com.Var.t option
    (* If var is a ref, using it to get the var space *)
  | Dand of expr list
  | Dor of expr list
  | Dunop of string * expr
  | Dbinop of string * expr * expr
  | Dfun of string * expr list
  | Dite of expr * expr * expr
  | Dtyp of Com.value_typ
  | Ddirect of expr

and varinfo_access =
  | VIvar of Com.Var.t
  | VItab of Com.Var.t * expr * expr (* variable, var_def, var_val *)
  | VIfield of expr * expr * string (* var_def, var_val, field *)

and expr_var =
  | Local of stack_slot
  | M of Com.var_space * Com.Var.t * dflag
  | Named of string * dflag

and t = { expr : expr; kind : dflag; spills : local_vars }

and builder = local_stacks -> local_vars -> t

let mk_t expr kind spills = { expr; kind; spills }

type stack_position = Not_to_stack | Must_be_pushed | On_top of dflag

(* let dflag_id = function Def -> 0 | Val -> 1 | VarInfo -> 2 | VarSpace -> 3 *)

let pp_local_var fmt = function
  | Constr.Anon -> Format.fprintf fmt "--anon--"
  | Refered v -> Format.fprintf fmt "var-%i" v

let is_always_true t = t.expr = Dtrue

(** local stacks operations *)
let cast (kind : dflag) (expr : expr) =
  match (expr, kind) with
  | Dtrue, Val -> Dlit 1.
  | Dfalse, Val -> Dlit 0.
  | Dlit 0., Def -> Dfalse
  | Dlit _, Def -> Dtrue
  | _, Def -> Dbinop ("!=", expr, Dlit 0.)
  | Dvarinfo _, VarInfo -> expr
  | Dvarspace _, VarSpace -> expr
  | Dvarinfo _, _ -> failwith "Invalid cast of varinfo"
  | _, VarInfo -> failwith "Invalid cast to varinfo"
  | Dvarspace _, _ -> failwith "Invalid cast of varspace"
  | _, VarSpace -> failwith "Invalid cast to varspace"
  | _, Irdata -> failwith "Invalid cast to irdata"
  | _, Ptr _ -> failwith "Invalid cast to ptr"
  | _, Val -> expr

let bump_stack (kind : dflag) (st : local_stacks) =
  match kind with
  | Def -> { st with def_top = st.def_top + 1 }
  | Val -> { st with val_top = st.val_top + 1 }
  | VarInfo -> { st with var_top = st.var_top + 1 }
  | VarSpace -> { st with spa_top = st.spa_top + 1 }
  | _ -> st

let add_substitution (st : local_stacks) (v : local_var) (kind : dflag)
    (expr : expr) =
  match v with
  | Anon -> st
  | Refered v -> { st with var_substs = (v, (expr, kind)) :: st.var_substs }

let stack_top (kind : dflag) (st : local_stacks) =
  match kind with
  | Def -> st.def_top
  | Val -> st.val_top
  | VarInfo -> st.var_top
  | VarSpace -> st.spa_top
  | Irdata | Ptr _ ->
      Errors.raise_error "Shouldn't count pointers for the stack."

let is_in_stack_scope ({ kind; depth } : stack_slot) (st : local_stacks) =
  depth < stack_top kind st

let is_on_top ({ kind; depth } : stack_slot) (st : local_stacks) =
  depth = stack_top kind st

let rec expr_position (expr : expr) (st : local_stacks) =
  match expr with
  | Dtrue | Dfalse | Dlit _ | Dvar (M _) | Dtyp _ -> Not_to_stack
  | Dvar (Local slot) ->
      if is_in_stack_scope slot st then Not_to_stack
      else if is_on_top slot st then On_top slot.kind
      else Must_be_pushed
  | Dvar (Named _) -> Not_to_stack
  | Dbinop ("/", e1, e2) -> begin
      (* avoid storage of division by zero. It assumes all division are
         guarded *)
      match (expr_position e1 st, expr_position e2 st) with
      | On_top s, _ | _, On_top s ->
          On_top s
          (* Needed to bumb the stack to avoid erasing subexpressions *)
      | _, _ -> Not_to_stack (* Either already stored, or duplicatable *)
    end
  | Ddirect _ -> Not_to_stack
  | Dbinop _ | Dand _ | Dor _ | Dunop _ | Dfun _ | Dite _ | Dvarinfo _
  | Dvarspace _ ->
      Must_be_pushed

(* allocate to local variable if necessary *)
let store_local (stacks : local_stacks) (ctx : local_vars) (v : local_var)
    (kind : dflag) (subexpr : expr) =
  (* we allocate only expression that are non-atomic *)
  match expr_position subexpr stacks with
  | Not_to_stack -> (add_substitution stacks v kind subexpr, ctx, subexpr)
  | On_top kind ->
      (* this happens when a subexpression is lifted through several
         constructors. e.g. [ 0 || (0 || e) ] where [e] is allocated and is
         trivially lifted from the or constructors. *)
      (add_substitution stacks v kind subexpr |> bump_stack kind, ctx, subexpr)
  | Must_be_pushed ->
      let depth = stack_top kind stacks in
      let stacks = bump_stack kind stacks in
      let slot = { kind; depth } in
      let assignment = { slot; subexpr } in
      let ctx = (v, assignment) :: ctx in
      (stacks, ctx, Dvar (Local slot))

(* the following functions resolve [constr] values by applying them to a given
   context (both stacks state and existing local allocations) *)

let collapse_builder (stacks : local_stacks) (ctx : local_vars)
    (builder : builder) =
  let t = builder stacks ctx in
  { t with spills = t.spills @ ctx }

(* eval and store with enforced kind *)
let push_with_kind (stacks : local_stacks) (ctx : local_vars) (kind : dflag)
    (builder : builder) =
  (* let expr, ekind, lv = builder stacks ctx in *)
  let t = builder stacks ctx in
  let expr = if kind = t.kind then t.expr else cast kind t.expr in
  let stacks, lv, expr = store_local stacks t.spills Constr.anon kind expr in
  (stacks, lv, expr)

(* eval and store without enforcing kind *)
let push (stacks : local_stacks) (ctx : local_vars) (builder : builder) =
  let t = builder stacks ctx in
  let stacks, lv, expr =
    store_local stacks t.spills Constr.anon t.kind t.expr
  in
  (stacks, lv, expr, t.kind)

module DE = Def_expr.Make (Constr)

let true_ _ _ = mk_t Dtrue Constr.Def []

let false_ _ _ = mk_t Dfalse Constr.Def []

let lit f _ _ = mk_t (Dlit f) Constr.Val []

let m_var (m_sp_opt : Com.var_space) (v : Com.Var.t) (df : dflag) _ _ =
  mk_t (Dvar (M (m_sp_opt, v, df))) df []

let local_var (lvar : local_var) (stacks : local_stacks) (ctx : local_vars) =
  match lvar with
  | Anon -> Errors.raise_error "Tried to access anonymous local variable"
  | Refered v -> (
      match List.assoc_opt v stacks.var_substs with
      | Some (e, kind) -> mk_t e kind []
      | None -> (
          match List.assoc_opt lvar ctx with
          | Some { slot; _ } -> mk_t (Dvar (Local slot)) slot.kind []
          | None ->
              Format.kasprintf Errors.raise_error
                "Local variable %i not found in context. Local vars: [%a]; \
                 stacks subst: [%a]"
                v
                (Format.pp_print_list
                   ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
                   pp_local_var)
                (List.map fst ctx)
                (Format.pp_print_list
                   ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
                   (fun fmt (i, _) -> Format.fprintf fmt "%i" i))
                stacks.var_substs))

let dand = function [] -> Dtrue | [ e ] -> e | l -> Dand l

let dor = function [] -> Dfalse | [ e ] -> e | l -> Dor l

let and_ e1 e2 stacks ctx =
  let stacks', lv1, e1 = push_with_kind stacks ctx Def e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ -> mk_t e2 Def lv2
  | _, Dtrue -> mk_t e1 Def lv1
  | Dfalse, _ | _, Dfalse -> mk_t Dfalse Def []
  | Dvar v1, Dvar v2 when v1 = v2 -> mk_t e1 Def lv1
  | Dand l1, Dand l2 -> mk_t (dand (l1 @ l2)) Def (lv2 @ lv1)
  | _, Dand l -> mk_t (dand (e1 :: l)) Def (lv2 @ lv1)
  | Dand l, _ -> mk_t (dand (l @ [ e2 ])) Def (lv2 @ lv1)
  | _, _ -> mk_t (dand [ e1; e2 ]) Def (lv2 @ lv1)

let or_ (e1 : builder) (e2 : builder) (stacks : local_stacks) (ctx : local_vars)
    =
  let stacks', lv1, e1 = push_with_kind stacks ctx Def e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ | _, Dtrue -> mk_t Dtrue Def []
  | Dfalse, _ -> mk_t e2 Def lv2
  | _, Dfalse -> mk_t e1 Def lv1
  | Dvar v1, Dvar v2 when v1 = v2 -> mk_t e1 Def lv1
  | Dor l1, Dor l2 -> mk_t (dor (l1 @ l2)) Def (lv2 @ lv1)
  | _, Dor l -> mk_t (dor (e1 :: l)) Def (lv2 @ lv1)
  | Dor l, _ -> mk_t (dor (l @ [ e2 ])) Def (lv2 @ lv1)
  | _, _ -> mk_t (dor [ e1; e2 ]) Def (lv2 @ lv1)

let not_ (e : builder) (stacks : local_stacks) (ctx : local_vars) =
  let _, lv, e = push_with_kind stacks ctx Def e in
  match e with
  | Dtrue -> mk_t Dfalse Def []
  | Dfalse -> mk_t Dtrue Def []
  | Dunop ("!", e) -> mk_t e Def lv
  | _ -> mk_t (Dunop ("!", e)) Def lv

let minus (e : builder) (stacks : local_stacks) (ctx : local_vars) =
  let _, lv, e = push_with_kind stacks ctx Val e in
  match e with
  | Dlit f -> mk_t (Dlit (-.f)) Val []
  | Dunop ("-", e) -> mk_t e Val lv
  | _ -> mk_t (Dunop ("-", e)) Val lv

let plus (e1 : builder) (e2 : builder) (stacks : local_stacks)
    (ctx : local_vars) =
  (* This optimisation causes some valuation to end at -0.0 where +0.0 was
     expected. Staying conservative for now *)
  let reduce_zero_add = false in
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ when reduce_zero_add -> mk_t e2 Val lv2
  | _, Dlit 0. when reduce_zero_add -> mk_t e1 Val lv1
  | Dlit f1, Dlit f2 -> mk_t (Dlit (f1 +. f2)) Val []
  | _ -> mk_t (Dbinop ("+", e1, e2)) Val (lv2 @ lv1)

let sub (e1 : builder) (e2 : builder) (stacks : local_stacks) (ctx : local_vars)
    =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ -> mk_t (Dunop ("-", e2)) Val lv2
  | _, Dlit 0. -> mk_t e1 Val lv1
  | Dlit f1, Dlit f2 -> mk_t (Dlit (f1 -. f2)) Val []
  | _ -> mk_t (Dbinop ("-", e1, e2)) Val (lv2 @ lv1)

let mult (e1 : builder) (e2 : builder) (stacks : local_stacks)
    (ctx : local_vars) =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | Dlit 1., _ -> mk_t e2 Val lv2
  | _, Dlit 1. -> mk_t e1 Val lv1
  | Dlit 0., _ | _, Dlit 0. -> mk_t (Dlit 0.) Val []
  | Dlit f1, Dlit f2 -> mk_t (Dlit (f1 *. f2)) Val []
  | _ -> mk_t (Dbinop ("*", e1, e2)) Val (lv2 @ lv1)

let div (e1 : builder) (e2 : builder) (stacks : local_stacks) (ctx : local_vars)
    =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | _, Dlit 1. -> mk_t e1 Val lv1
  | Dlit f1, Dlit f2 ->
      let f = f1 /. f2 in
      mk_t (Dlit f) Val []
  | _ -> mk_t (Dbinop ("/", e1, e2)) Val (lv2 @ lv1)

let modulo (e1 : builder) (e2 : builder) (stacks : local_stacks)
    (ctx : local_vars) =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  match (e1, e2) with
  | _, Dlit 1. -> mk_t e1 Val lv1
  | Dlit f1, Dlit f2 ->
      let f = mod_float f1 f2 in
      mk_t (Dlit f) Val []
  | _ -> mk_t (Dfun ("fmod", [ e1; e2 ])) Val (lv2 @ lv1)

let comp op (e1 : builder) (e2 : builder) (stacks : local_stacks)
    (ctx : local_vars) =
  let stacks', lv1, e1 = push_with_kind stacks ctx Val e1 in
  let _, lv2, e2 = push_with_kind stacks' ctx Val e2 in
  let comp (o : Com.comp_op) =
    match (e1, e2) with
    | Dlit f1, Dlit f2 ->
        if M_ir.Mir_number.RegularFloatNumber.compare o f1 f2 then Dtrue
        else Dfalse
    | Dvar v1, Dvar v2 ->
        if String.equal op "==" && v1 = v2 then Dtrue else Dbinop (op, e1, e2)
    | _ -> Dbinop (op, e1, e2)
  in
  let e =
    match op with
    | "==" -> comp Com.Eq
    | "!=" -> comp Com.Neq
    | "<=" -> comp Com.Lte
    | "<" -> comp Com.Lt
    | ">=" -> comp Com.Gte
    | ">" -> comp Com.Gt
    | _ -> assert false
  in
  mk_t e Def (lv2 @ lv1)

let fun_ (f : string) (args : builder list) (stacks : local_stacks)
    (ctx : local_vars) =
  let (_, lv), args =
    List.fold_left_map
      (fun (stacks, lv) e ->
        let stacks, lv', e, _ = push stacks ctx e in
        ((stacks, lv' @ lv), e))
      (stacks, []) args
  in
  (* TODO : distinguish kinds *)
  mk_t (Dfun (f, args)) Val lv

let varinfo v _ _ = mk_t (Dvarinfo (VIvar v)) VarInfo []

let varinfo_tab ~tab ~def ~value stacks ctx =
  let stacks, lv, def = push_with_kind stacks ctx Def def in
  let _stacks, lv', value = push_with_kind stacks ctx Val value in
  mk_t (Dvarinfo (VItab (tab, def, value))) VarInfo (lv @ lv')

let varinfo_field ~def ~value ~field stacks ctx =
  let stacks, lv, def = push_with_kind stacks ctx Def def in
  let _stacks, lv', value = push_with_kind stacks ctx Val value in
  mk_t (Dvarinfo (VIfield (def, value, field))) VarInfo (lv @ lv')

let varspace_current m _ _ = mk_t (Dvarspace (m, None)) VarSpace []

let varspace_of m v _ _ = mk_t (Dvarspace (m, Some v)) VarSpace []

let typ t _ _ = mk_t (Dtyp t) Def []

let named s kind _ _ = mk_t (Dvar (Named (s, kind))) kind []

let direct c stacks ctx =
  let t = c stacks ctx in
  mk_t (Ddirect t.expr) t.kind t.spills

let ite (c : builder) (t : builder) (e : builder) (stacks : local_stacks)
    (ctx : local_vars) =
  let stacks', lvc, c = push_with_kind stacks ctx Def c in
  let stacks', lvt, t, tkind = push stacks' ctx t in
  let _, lve, e, ekind = push stacks' ctx e in
  let ite_kind =
    if tkind = ekind then tkind
    else (* this will happen. Staying on the safe side *) Val
  in
  match (c, t, e) with
  | Dtrue, _, _ -> mk_t t tkind lvt
  | Dfalse, _, _ -> mk_t e ekind lve
  | _, Dtrue, Dtrue | _, Dfalse, Dfalse -> mk_t t tkind lvt
  | _, Dlit 1., Dlit 0. -> mk_t c Def lvc
  | _, Dlit 0., Dlit 1. -> mk_t (Dunop ("!", c)) Def lvc
  | _, Dlit f, Dlit f' when f = f' -> mk_t (Dlit f) ite_kind []
  | _ -> mk_t (Dite (c, t, e)) ite_kind (lve @ lvt @ lvc)

let it0 (c : builder) (t : builder) (stacks : local_stacks) (ctx : local_vars) =
  (* Version of [ite] where the else is zero with kind matching the then to
     avoid casting later *)
  let stacks', lvc, c = push_with_kind stacks ctx Def c in
  let _, lvt, t, tkind = push stacks' ctx t in
  let e =
    match tkind with
    | Def -> Dfalse
    | Val -> Dlit 0.
    | VarInfo -> failwith "Cannot make an if-then with a VarInfo"
    | VarSpace -> failwith "Cannot make an if-then with a VarSpace"
    | Irdata -> failwith "Cannot make an if-then with Irdata"
    | Ptr _ -> failwith "Cannot make an if-then with a pointer"
  in
  match (c, t) with
  | Dtrue, _ -> mk_t t tkind lvt
  | Dfalse, _ -> mk_t e tkind []
  | _, (Dlit 1. | Dtrue) -> mk_t c Def lvc
  | _, (Dlit 0. | Dfalse) -> mk_t t tkind []
  | _ -> mk_t (Dite (c, t, e)) tkind (lvt @ lvc)

let let_local (v : local_var) (bound : builder) (body : builder)
    (stacks : local_stacks) (ctx : local_vars) =
  let t = collapse_builder stacks ctx bound in
  let stacks, ctx, _ = store_local stacks t.spills v t.kind t.expr in
  collapse_builder stacks ctx body

let rec make_constr : Constr.t -> builder = function
  | True -> true_
  | False -> false_
  | Lit f -> lit f
  | M (vs, v, d) -> m_var vs v d
  | Named (id, kind) -> named id kind
  | Local lv -> local_var lv
  | And (e1, e2) -> and_ (make_constr e1) (make_constr e2)
  | Or (e1, e2) -> or_ (make_constr e1) (make_constr e2)
  | Not e -> not_ (make_constr e)
  | Minus e -> minus (make_constr e)
  | Plus (e1, e2) -> plus (make_constr e1) (make_constr e2)
  | Sub (e1, e2) -> sub (make_constr e1) (make_constr e2)
  | Mult (e1, e2) -> mult (make_constr e1) (make_constr e2)
  | Div (e1, e2) -> div (make_constr e1) (make_constr e2)
  | Modulo (e1, e2) -> modulo (make_constr e1) (make_constr e2)
  | Comp (s, e1, e2) -> comp s (make_constr e1) (make_constr e2)
  | Fun (s, l) -> fun_ s (List.map make_constr l)
  | Varinfo v -> varinfo v
  | Varinfo_tab (tab, d, v) ->
      varinfo_tab ~tab ~def:(make_constr d) ~value:(make_constr v)
  | Varinfo_field (d, v, field) ->
      varinfo_field ~def:(make_constr d) ~value:(make_constr v) ~field
  | Varspace_current vs -> varspace_current vs
  | Varspace_of (m, v) -> varspace_of m v
  | Typ t -> typ t
  | Direct e -> direct (make_constr e)
  | Ite (c, t, e) -> ite (make_constr c) (make_constr t) (make_constr e)
  | It0 (c, t) -> it0 (make_constr c) (make_constr t)
  | Let_local (lv, e, i) -> let_local lv (make_constr e) (make_constr i)

type 'expr set_var = { kind : dflag; id : string; expr : 'expr }

let mk_set_var kind id expr = { kind; id; expr }

type atomic_expression_composition = {
  set_vars : Constr.t option set_var list;
  def_test : DE.t;
  value_comp : Constr.t;
}

type expression_composition =
  | AtomicExpr of atomic_expression_composition
  | Cond of
      expression_composition * expression_composition * expression_composition
  | Let of {
      kind : dflag;
      vardef : string;
      varval : string;
      body : expression_composition;
      followup : expression_composition;
    }

let atomic e = AtomicExpr e

let make_generic_let =
  let fresh_name =
    let cpt = ref 0 in
    fun () ->
      let vardef = Format.sprintf "vardef%i" !cpt
      and varval = Format.sprintf "varval%i" !cpt in
      incr cpt;
      (vardef, varval)
  in
  fun f ->
    let vardef, varval = fresh_name () in
    f vardef varval

let make_let kind body followup =
  match body with
  | AtomicExpr e -> begin
      match followup e.def_test e.value_comp with
      | AtomicExpr e' ->
          atomic @@ { e' with set_vars = e.set_vars @ e'.set_vars }
      | _ ->
          make_generic_let (fun vardef varval ->
              Let
                {
                  kind;
                  vardef;
                  varval;
                  body;
                  followup =
                    followup
                      (DE.devar @@ Constr.Named (vardef, Def))
                      (Constr.Named (varval, kind));
                })
    end
  | _ ->
      make_generic_let (fun vardef varval ->
          Let
            {
              kind;
              vardef;
              varval;
              body;
              followup =
                followup
                  (DE.devar @@ Constr.Named (vardef, Def))
                  (Constr.Named (varval, kind));
            })

let make_lets (kind : dflag) (bodys : expression_composition list)
    (followup : (DE.t * Constr.t) list -> expression_composition) =
  let rec loop acc = function
    | [] -> followup (List.rev acc)
    | body :: tl -> make_let kind body (fun d v -> loop ((d, v) :: acc) tl)
  in
  loop [] bodys

let def_expr_to_constr e =
  let map = DE.get_assoc e in
  let rec loop = function
    | Def_expr.DEand [] -> Constr.True
    | DEand (hd :: tl) ->
        List.fold_left
          (fun acc e ->
            let c = loop e in
            Constr.And (acc, c))
          (loop hd) tl
    | DEor [] -> False
    | DEor (hd :: tl) ->
        List.fold_left
          (fun acc e ->
            let c = loop e in
            Constr.Or (acc, c))
          (loop hd) tl
    | DEnot e -> Not (loop e)
    | DEatom v -> Def_expr.AtomMap.find v map
  in
  let def_expr = DE.get_expr e in
  let def_expr =
    if Config.optim_shorten_def () then Def_expr.Shorten_def.apply def_expr
    else def_expr
  in
  loop def_expr

let build_transitive_composition ~safe_def
    ({ set_vars; def_test; value_comp } : atomic_expression_composition) :
    atomic_expression_composition =
  (* `safe_def` can be set on call when we are sure that `value_comp` will
     always happen to be zero when `def_test` ends up false. E.g. arithmetic
     operation have such semantic property (funny question is what's the
     causality ?). This allows to remove a check to the definition flag when we
     compute the value, avoiding a lot of unnecessary code. *)
  let value_comp =
    if safe_def then value_comp
    else It0 (def_expr_to_constr def_test, value_comp)
  in
  { set_vars; def_test; value_comp }

let dfun_with_ptr (f : string)
    (args : ptrdef:Constr.t -> ptrval:Constr.t -> Constr.t list) :
    atomic_expression_composition =
  let res = fresh_c_local "res" in
  let res_def = Pp.spr "%s_def" res in
  let res_val = Pp.spr "%s_val" res in
  let res_def_ptr = Pp.spr "&%s" res_def in
  let res_val_ptr = Pp.spr "&%s" res_val in
  let ptrdef = Constr.Named (res_def_ptr, Ptr Def) in
  let ptrval = Constr.Named (res_val_ptr, Ptr Val) in
  let d_fun = Constr.Fun (f, args ~ptrdef ~ptrval) in
  let set_vars =
    [ mk_set_var Def res_def (Some d_fun); mk_set_var Val res_val None ]
  in
  let def_test = DE.devar @@ Named (res_def, Def) in
  let value_comp = Constr.Named (res_val, Val) in
  build_transitive_composition ~safe_def:true { set_vars; def_test; value_comp }

let eundefined () =
  { set_vars = []; def_test = DE.defalse; value_comp = Lit 0. }

let elit f = { set_vars = []; def_test = DE.detrue; value_comp = Lit f }

type local_decls = {
  def_stk_size : int;
  val_stk_size : int;
  var_stk_size : int;
  var_spa_size : int;
}
(* in practice, local_decls are encoded as stacks sizes *)

type build_atomic_expr_ret = {
  local_decls : local_decls;
  set_vars : t option set_var list;
  def_test : t;
  value_comp : t;
}

(* evaluate a complete (AKA, context free) expression. Not to be used for
   further construction. *)
let build_atomic_expression (expr_comp : atomic_expression_composition) :
    build_atomic_expr_ret =
  let empty_stacks =
    { def_top = 0; val_top = 0; var_top = 0; spa_top = 0; var_substs = [] }
  in
  let empty_local_decls =
    {
      def_stk_size = -1;
      val_stk_size = -1;
      var_stk_size = -1;
      var_spa_size = -1;
    }
  in
  let empty_locals = [] in
  let set_tests =
    List.map
      (fun { kind; id; expr } ->
        mk_set_var kind id
        @@ Option.map
             (fun expr ->
               collapse_builder empty_stacks empty_locals (make_constr expr))
             expr)
      expr_comp.set_vars
  in
  let set_locals =
    List.concat
      (List.filter_map
         (fun t -> Option.map (fun expr -> expr.spills) t.expr)
         set_tests)
  in
  let def_test =
    collapse_builder empty_stacks empty_locals
      (make_constr @@ def_expr_to_constr expr_comp.def_test)
  in
  let value_comp =
    collapse_builder empty_stacks empty_locals
      (make_constr expr_comp.value_comp)
  in
  let stacks_size =
    List.fold_left
      (fun ld (_, { slot; _ }) ->
        match slot.kind with
        | Def -> { ld with def_stk_size = max slot.depth ld.def_stk_size }
        | Val -> { ld with val_stk_size = max slot.depth ld.val_stk_size }
        | VarInfo -> { ld with var_stk_size = max slot.depth ld.var_stk_size }
        | VarSpace -> { ld with var_spa_size = max slot.depth ld.var_spa_size }
        | _ -> ld)
      empty_local_decls
      (set_locals @ def_test.spills @ value_comp.spills)
  in
  { local_decls = stacks_size; set_vars = set_tests; def_test; value_comp }

let rec build_expression = function
  | AtomicExpr e -> `Atom (build_atomic_expression e)
  | Cond (c, t, e) ->
      `Cond (build_expression c, build_expression t, build_expression e)
  | Let { kind; vardef; varval; body; followup } ->
      `Let
        (kind, vardef, varval, build_expression body, build_expression followup)

let format_slot fmt ({ kind; depth } : stack_slot) =
  let kind =
    match kind with
    | Def -> "int"
    | Val -> "real"
    | VarInfo -> "varinfo"
    | VarSpace -> "space"
    | _ -> Errors.raise_error "Can't format a slot for Irdata or pointer"
  in
  Format.fprintf fmt "%s%d" kind depth

let format_expr_var ~(env : Types.env) fmt (ev : expr_var) =
  match ev with
  | Local slot -> format_slot fmt slot
  | Named (s, _) -> Format.fprintf fmt "%s" s
  | M (m_sp_opt, var, df) ->
      let def_flag = df = Def in
      Format.fprintf fmt "%s"
        (generate_variable ~env ~trace_flag:env.dgfip_flags.flg_trace ~def_flag
           m_sp_opt var)

let has_name ~name = function
  | Local slot -> name = Format.asprintf "%a" format_slot slot
  | Named (s, _) -> name = s
  | M (m_sp_opt, var, df) ->
      let v', _ =
        match df with
        | Def -> VID.gen_def m_sp_opt var
        | _ -> VID.gen_val m_sp_opt var
      in
      name = v'

let is_var_with_name ~name = function Dvar v -> has_name ~name v | _ -> false

let find_first_and_split f l =
  let rec loop acc = function
    | [] -> None
    | hd :: tl ->
        if f hd then Some (hd, List.rev_append acc tl) else loop (hd :: acc) tl
  in
  loop [] l

let rec format_dexpr_assign ~(env : Env.t) v fmt (de : expr) =
  let format_dexpr = format_dexpr ~env in
  let default_print () = Format.fprintf fmt "%s@ =@ %a" v format_dexpr de in
  if env.dgfip_flags.flg_trace then default_print ()
  else
    match de with
    | Dvar evar ->
        let should_print =
          env.dgfip_flags.flg_trace || not (has_name ~name:v evar)
        in
        if should_print then default_print () else ()
    | Dand l -> begin
        match find_first_and_split (is_var_with_name ~name:v) l with
        | None -> default_print ()
        | Some (_, l') ->
            Format.fprintf fmt "%s@ &=@ %a" v format_dexpr (dand l')
      end
    | Dor l -> begin
        match find_first_and_split (is_var_with_name ~name:v) l with
        | None -> default_print ()
        | Some (_, l') ->
            Format.fprintf fmt "%s@ |=@ %a" v format_dexpr (dor l')
      end
    | Dbinop ((("+" | "*") as op), e1, e2) ->
        if is_var_with_name ~name:v e1 then
          Format.fprintf fmt "%s@ %s=@ %a" v op format_dexpr e2
        else if is_var_with_name ~name:v e2 then
          Format.fprintf fmt "%s@ %s=@ %a" v op format_dexpr e1
        else default_print ()
    | Dbinop ((("-" | "/") as op), e1, e2) ->
        if is_var_with_name ~name:v e1 then
          Format.fprintf fmt "%s@ %s=@ %a" v op format_dexpr e2
        else default_print ()
    | Dtrue | Dfalse | Dlit _ | Dvarinfo _ | Dvarspace _ | Dunop _ | Dbinop _
    | Dfun _ | Dite _ | Dtyp _ | Ddirect _ ->
        default_print ()

and format_dexpr ~env fmt (de : expr) =
  let format_dexpr = format_dexpr ~env in
  match de with
  | Dtrue -> Format.fprintf fmt "1"
  | Dfalse -> Format.fprintf fmt "0"
  | Dlit f -> (
      match Float.modf f with
      | 0., _ ->
          (* Print at least one decimal, distinction from integers *)
          Format.fprintf fmt "%.1f" f
      | _ ->
          (* Print literal floats as precisely as possible *)
          Format.fprintf fmt "%#.19g" f)
  | Dvar evar -> format_expr_var ~env fmt evar
  | Dand l ->
      let sep = if Config.optim_simple_binary_op () then " & " else " && " in
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt sep)
           format_dexpr)
        l
  | Dor l ->
      let sep = if Config.optim_simple_binary_op () then " | " else " || " in
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt sep)
           format_dexpr)
        l
  | Dunop (op, de) -> Format.fprintf fmt "@[<hov 2>(%s%a@])" op format_dexpr de
  | Dbinop (op, de1, de2) -> begin
      match op with
      | ">" ->
          Format.fprintf fmt "@[<hov 2>(GT_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | "<" ->
          Format.fprintf fmt "@[<hov 2>(LT_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | ">=" ->
          Format.fprintf fmt "@[<hov 2>(GE_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | "<=" ->
          Format.fprintf fmt "@[<hov 2>(LE_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | "==" ->
          Format.fprintf fmt "@[<hov 2>(EQ_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | "!=" ->
          Format.fprintf fmt "@[<hov 2>(NEQ_E(%a,%a)@])" format_dexpr de1
            format_dexpr de2
      | _ ->
          Format.fprintf fmt "@[<hov 2>(%a@ %s %a@])" format_dexpr de1 op
            format_dexpr de2
    end
  | Dfun (funname, des) ->
      Format.fprintf fmt "@[<hov 2>%s(%a@])" funname
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           format_dexpr)
        des
  | Dvarinfo v -> format_varinfo ~env fmt v
  | Dvarspace (m_sp_opt, v_opt) ->
      Format.fprintf fmt "@[%s@]"
        (match v_opt with
        | Some v -> VID.gen_var_space_id m_sp_opt v
        | None -> VID.gen_var_space_id_opt m_sp_opt)
  | Dtyp t -> Format.fprintf fmt "@[%s@]" @@ VID.gen_typ t
  | Ddirect expr -> format_dexpr fmt expr
  | Dite (dec, det, dee) ->
      Format.fprintf fmt "@[<hov 2>(%a ?@ %a@ : %a@])" format_dexpr dec
        format_dexpr det format_dexpr dee

and format_varinfo ~env fmt : varinfo_access -> unit = function
  | VIvar v -> Format.fprintf fmt "%s" (VID.gen_info_ptr v)
  | VItab (v, def, value) ->
      Format.fprintf fmt "@[lis_tabaccess_varinfo(irdata, %d, %a, %a)@]"
        (Com.Var.loc_tab_idx v) (format_dexpr ~env) def (format_dexpr ~env)
        value
  | VIfield (def, value, field) ->
      Format.fprintf fmt "@[event_field_%s_var(irdata, %a, %a)@]" field
        (format_dexpr ~env) def (format_dexpr ~env) value

let format_local_declarations fmt (ld : local_decls) =
  for i = 0 to ld.def_stk_size do
    Format.fprintf fmt "@;@[<hov 2>int int%d;@]" i
  done;
  for i = 0 to ld.val_stk_size do
    Format.fprintf fmt "@;@[<hov 2>double real%d;@]" i
  done;
  for i = 0 to ld.var_stk_size do
    Format.fprintf fmt "@;@[<hov 2>T_varinfo* varinfo%d;@]" i
  done;
  for i = 0 to ld.var_spa_size do
    Format.fprintf fmt "@;@[<hov 2>int space%d;@]" i
  done

let format_local_vars_defs ~env fmt (lv : local_vars) =
  let lv = List.rev lv in
  let format_one_assign fmt (_, { slot; subexpr }) =
    let v = Format.asprintf "%a" format_slot slot in
    Format.fprintf fmt "@;@[<hov 2>%a;@]" (format_dexpr_assign ~env v) subexpr
  in
  List.iter (format_one_assign fmt) lv

let format_assign ~env (var : string) fmt t =
  Format.fprintf fmt "%a@;@[<hov 2>%a;@]"
    (format_local_vars_defs ~env)
    t.spills
    (format_dexpr_assign ~env var)
    t.expr

let rec format_kind kind =
  match kind with
  | Def -> "char"
  | Val -> "double"
  | VarInfo -> "T_varinfo*"
  | Irdata -> "T_irdata*"
  | Ptr k -> Format.asprintf "%s*" (format_kind k)
  | VarSpace -> "int"

let format_set_vars ~env fmt (set_vars : t option set_var list) =
  List.iter (fun t -> Pp.fpr fmt "@;%s %s;" (format_kind t.kind) t.id) set_vars;
  (* We do not assign values without expr *)
  List.iter (fun t -> Option.iter (format_assign ~env t.id fmt) t.expr) set_vars

(* Building basic expressions *)

let comparison op (se1 : expression_composition) (se2 : expression_composition)
    =
  make_let Constr.Val se1 (fun d1 v1 ->
      make_let Constr.Val se2 (fun d2 v2 ->
          let safe_def = false in
          let def_test = DE.deand [ d1; d2 ] in
          let value_comp =
            let op =
              let open Com in
              match Pos.unmark op with
              | Gt -> ">"
              | Gte -> ">="
              | Lt -> "<"
              | Lte -> "<="
              | Eq -> "=="
              | Neq -> "!="
            in
            Constr.Comp (op, v1, v2)
          in
          AtomicExpr
            (build_transitive_composition ~safe_def
               { set_vars = []; def_test; value_comp })))

let binop op (se1 : expression_composition) (se2 : expression_composition) =
  make_let Constr.Val se1 (fun d1 v1 ->
      make_let Constr.Val se2 (fun d2 v2 ->
          let safe_def = true in
          let def_test =
            match Pos.unmark op with
            | Com.And | Com.Mul | Com.Div | Com.Mod -> DE.deand [ d1; d2 ]
            | Com.Or | Com.Add | Com.Sub -> DE.deor [ d1; d2 ]
          in
          let op e1 e2 =
            match Pos.unmark op with
            | Com.And -> Constr.And (e1, e2)
            | Com.Or -> Or (e1, e2)
            | Com.Add -> Plus (e1, e2)
            | Com.Sub -> Sub (e1, e2)
            | Com.Mul -> Mult (e1, e2)
            | Com.Div -> Ite (e2, Div (e1, e2), Lit 0.)
            | Com.Mod -> Ite (e2, Modulo (e1, e2), Lit 0.)
          in
          let value_comp = op v1 v2 in
          AtomicExpr
            (build_transitive_composition ~safe_def
               { set_vars = []; def_test; value_comp })))

let unop op se =
  let op, safe_def =
    match op with
    | Com.Not -> ((fun e -> Constr.Not e), false)
    | Com.Minus -> ((fun e -> Minus e), true)
  in
  make_let Constr.Val se (fun vardef varval ->
      let value_comp = op varval in
      let def_test = vardef in
      AtomicExpr
        (build_transitive_composition ~safe_def
           { set_vars = []; def_test; value_comp }))

let conditional cond thenval elseval = Cond (cond, thenval, elseval)

module Func = struct
  let supzero se =
    make_let Constr.Val se (fun vardef varval ->
        let def_test : DE.t =
          DE.(deand [ vardef; devar (Comp (">", varval, Lit 0.0)) ])
        in
        atomic
        @@ build_transitive_composition ~safe_def:false
             { set_vars = []; def_test; value_comp = varval })

  (* TODO: this code calculates the value of the expression before checking its
     definition. There may be a way to only calculate the definition. *)
  let present se =
    make_let Constr.Val se (fun vardef _varval ->
        let def_test = DE.detrue in
        let value_comp = def_expr_to_constr vardef in
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test; value_comp })

  let null se =
    make_let Constr.Val se (fun vardef varval ->
        (* S: Checking if expr is defined in expression is probably useless *)
        let value_comp =
          Constr.And (def_expr_to_constr vardef, Comp ("==", varval, Lit 0.0))
        in
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test = vardef; value_comp })

  let arr se =
    make_let Constr.Val se (fun vardef varval ->
        let value_comp = Constr.Fun ("my_arr", [ varval ]) in
        (* Here we boldly assume that rounding value of `undef` will give zero,
           given the invariant. Pretty sure that not true, in case of doubt, turn
           `safe_def` to false *)
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test = vardef; value_comp })

  let inf se =
    make_let Constr.Val se (fun vardef varval ->
        let value_comp = Constr.Fun ("my_floor", [ varval ]) in
        (* same as above *)
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test = vardef; value_comp })

  let abs se =
    make_let Constr.Val se (fun vardef varval ->
        let value_comp = Constr.Fun ("fabs", [ varval ]) in
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test = vardef; value_comp })

  let max se1 se2 =
    make_let Constr.Val se1 (fun d1 v1 ->
        make_let Constr.Val se2 (fun d2 v2 ->
            let def_test = DE.deor [ d1; d2 ] in
            let value_comp = Constr.Fun ("max", [ v1; v2 ]) in
            atomic
            @@ build_transitive_composition ~safe_def:true
                 { set_vars = []; def_test; value_comp }))

  let min se1 se2 =
    make_let Constr.Val se1 (fun d1 v1 ->
        make_let Constr.Val se2 (fun d2 v2 ->
            let def_test = DE.deor [ d1; d2 ] in
            let value_comp = Constr.Fun ("min", [ v1; v2 ]) in
            atomic
            @@ build_transitive_composition ~safe_def:true
                 { set_vars = []; def_test; value_comp }))

  let gtez se =
    make_let Constr.Val se (fun d1 v1 ->
        let value_comp = Constr.Comp (">=", v1, Lit 0.0) in
        atomic
        @@ build_transitive_composition ~safe_def:false
             { set_vars = []; def_test = d1; value_comp })

  let gtz se =
    make_let Constr.Val se (fun d1 v1 ->
        let value_comp = Constr.Comp (">", v1, Lit 0.0) in
        atomic
        @@ build_transitive_composition ~safe_def:true
             { set_vars = []; def_test = d1; value_comp })

  let sum (args : expression_composition list) =
    let rec loop def_test value_comp = function
      | [] ->
          atomic
          @@ build_transitive_composition ~safe_def:true
               { set_vars = []; def_test; value_comp }
      | hd :: tl ->
          make_let Constr.Val hd (fun d v ->
              let def_test = DE.deor [ def_test; d ]
              and value_comp = Constr.Plus (value_comp, v) in
              loop def_test value_comp tl)
    in
    match args with
    | [] ->
        atomic
        @@ build_transitive_composition ~safe_def:true
             {
               set_vars = [];
               def_test = DE.detrue;
               value_comp = Constr.Lit 0.0;
             }
    | _ -> loop DE.defalse (Constr.Lit 0.0) args

  let multimax (e : expression_composition) (m_sp_opt, v) =
    let ptr = VID.gen_info_ptr v in
    make_let Constr.Val e (fun vardef varval ->
        let d_fun =
          dfun_with_ptr "multimax_varinfo" (fun ~ptrdef ~ptrval ->
              [
                Constr.irdata;
                Named (VID.gen_var_space_id m_sp_opt v, VarSpace);
                Named (ptr, Ptr Val);
                def_expr_to_constr vardef;
                varval;
                ptrdef;
                ptrval;
              ])
        in
        atomic @@ build_transitive_composition ~safe_def:true d_fun)

  let nb_events () =
    let def_test = DE.detrue in
    let value_comp = Constr.Fun ("nb_evenements", [ Constr.irdata ]) in
    atomic
    @@ build_transitive_composition ~safe_def:true
         { set_vars = []; def_test; value_comp }

  let nb_anomalies () =
    let def_test = DE.detrue in
    let value_comp = Constr.Fun ("nb_anomalies", [ Constr.irdata ]) in
    atomic
    @@ build_transitive_composition ~safe_def:true
         { set_vars = []; def_test; value_comp }

  let nb_discordances () =
    let def_test = DE.detrue in
    let value_comp = Constr.Fun ("nb_discordances", [ Constr.irdata ]) in
    atomic
    @@ build_transitive_composition ~safe_def:true
         { set_vars = []; def_test; value_comp }

  let nb_informatives () =
    let def_test = DE.detrue in
    let value_comp = Constr.Fun ("nb_informatives", [ Constr.irdata ]) in
    atomic
    @@ build_transitive_composition ~safe_def:true
         { set_vars = []; def_test; value_comp }

  let nb_bloquantes () =
    let def_test = DE.detrue in
    let value_comp = Constr.Fun ("nb_bloquantes", [ Constr.irdata ]) in
    atomic
    @@ build_transitive_composition ~safe_def:true
         { set_vars = []; def_test; value_comp }

  let call fn args =
    let d_fun args =
      atomic
      @@ dfun_with_ptr fn (fun ~ptrdef ~ptrval ->
          Constr.irdata :: ptrdef :: ptrval :: args)
    in
    make_lets Constr.Val args (fun l ->
        let args =
          List.flatten @@ List.map (fun (d, v) -> [ def_expr_to_constr d; v ]) l
        in
        d_fun args)
end

let write_atomic_decoupled_expr ~(env : Env.t) oc res_def res_val build_ret =
  let pr form = Format.fprintf oc form in
  let assign var value = format_assign ~env var oc value in
  let undefined_value =
    match build_ret.value_comp.kind with
    | Val -> "0.0"
    | VarInfo -> "NULL"
    | Def -> "0"
    | VarSpace -> "0"
    | Ptr _ -> "NULL"
    | Irdata ->
        Errors.raise_error "Trying to set irdata to null as default value."
  in
  pr "@;@[<v 2>{";
  format_local_declarations oc build_ret.local_decls;
  format_set_vars ~env oc build_ret.set_vars;
  assign res_def build_ret.def_test;
  if is_always_true build_ret.def_test then assign res_val build_ret.value_comp
  else begin
    (* the value is only meaningful when the definition flag holds *)
    pr "@;@[<v 2>if (%s) {" res_def;
    assign res_val build_ret.value_comp;
    pr "@]@;} else %s = %s;" res_val undefined_value
  end;
  pr "@]@;}"

let fresh_cond_vars =
  let cpt = ref 0 in
  fun () ->
    let res_def = Format.sprintf "cond_def%i" !cpt
    and res_val = Format.sprintf "cond_val%i" !cpt in
    incr cpt;
    (res_def, res_val)

let rec write_decoupled_expr ~(env : Env.t) oc =
  let pr form = Format.fprintf oc form in
  fun res_def res_val -> function
    | `Atom a -> write_atomic_decoupled_expr ~env oc res_def res_val a
    | `Cond (c, t, e) ->
        let d, v = fresh_cond_vars () in
        pr "@;{@[<v 2>";
        pr "@;int %s;" d;
        pr "@;double %s;" v;
        write_decoupled_expr ~env oc d v c;
        pr "@;/* definedness check */";
        pr "@;if(%s == 0) {%s = 0; %s = 0.0;}" d res_def res_val;
        pr "@;/* if true */";
        pr "@;else if (NEQ_E(%s,0.0)) {@;@[<v 2>" v;
        write_decoupled_expr ~env oc res_def res_val t;
        pr "@;/* if false */";
        pr "@;}@] else {@;@[<v 2>";
        write_decoupled_expr ~env oc res_def res_val e;
        pr "@;}@]";
        pr "@;}@]"
    | `Let (varkind, vardef, varval, body, followup) ->
        pr "@;{@[<v 2>";
        pr "@;int %s;" vardef;
        pr "@;%s %s;" (format_kind varkind) varval;
        write_decoupled_expr ~env oc vardef varval body;
        write_decoupled_expr ~env oc res_def res_val followup;
        pr "@;}@]"

let write_c_expr ~env oc res_def res_val expr =
  expr |> build_expression |> write_decoupled_expr ~env oc res_def res_val
