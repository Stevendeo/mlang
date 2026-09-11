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
open Core

module Map = struct
  type ('v, 'w, 'e, 'f) mapper = {
    instr :
      ('v, 'w, 'e, 'f) mapper ->
      ('v, 'e) m_instruction ->
      ('w, 'f) m_instruction;
    switch :
      ('v, 'w, 'e, 'f) mapper -> 'v switch_expression -> 'w switch_expression;
    formula :
      ('v, 'w, 'e, 'f) mapper -> 'v formula Pos.marked -> 'w formula Pos.marked;
    formula_decl :
      ('v, 'w, 'e, 'f) mapper -> 'v formula_decl -> 'w formula_decl;
    print_arg :
      ('v, 'w, 'e, 'f) mapper ->
      'v print_arg Pos.marked ->
      'w print_arg Pos.marked;
    set_value : ('v, 'w, 'e, 'f) mapper -> 'v set_value -> 'w set_value;
    loop_variables :
      ('v, 'w, 'e, 'f) mapper ->
      'v loop_variables Pos.marked ->
      'w loop_variables Pos.marked;
    loop_variable :
      ('v, 'w, 'e, 'f) mapper -> 'v loop_variable -> 'w loop_variable;
    loop_range : ('v, 'w, 'e, 'f) mapper -> 'v loop_range -> 'w loop_range;
    expr : ('v, 'w, 'e, 'f) mapper -> 'v m_expression -> 'w m_expression;
    access : ('v, 'w, 'e, 'f) mapper -> 'v m_access -> 'w m_access;
    case : ('v, 'w, 'e, 'f) mapper -> 'v case -> 'w case;
    atom : ('v, 'w, 'e, 'f) mapper -> 'v atom Pos.marked -> 'w atom Pos.marked;
    varspace : ('v, 'w, 'e, 'f) mapper -> var_space -> var_space;
    var : 'v -> 'w;
    err : 'e -> 'f;
  }

  let map_l f self = List.map (fun x -> f self x)

  let instr self instr =
    let ret =
      match Pos.unmark instr with
      | Affectation form -> Affectation (self.formula self form)
      | IfThenElse (cond, tbody, ebody) ->
          let cond = self.expr self cond in
          let tbody = map_l self.instr self tbody in
          let ebody = map_l self.instr self ebody in
          IfThenElse (cond, tbody, ebody)
      | WhenDoElse (branches, ebody) ->
          let map_branch (guard, body, pos) =
            let guard = self.expr self guard in
            let body = map_l self.instr self body in
            (guard, body, pos)
          in
          let branches = List.map map_branch branches in
          let ebody' = Pos.map (map_l self.instr self) ebody in
          WhenDoElse (branches, ebody')
      | ComputeDomain (a, vs) ->
          let vs = self.varspace self vs in
          ComputeDomain (a, vs)
      | ComputeChaining (a, vs) ->
          let vs = self.varspace self vs in
          ComputeChaining (a, vs)
      | ComputeVerifs (a, expr, sp) ->
          let sp = self.varspace self sp in
          let expr = self.expr self expr in
          ComputeVerifs (a, expr, sp)
      | ComputeTarget (a, args, sp) ->
          let sp = self.varspace self sp in
          let args = map_l self.access self args in
          ComputeTarget (a, args, sp)
      | VerifBlock body ->
          let body = map_l self.instr self body in
          VerifBlock body
      | Print (std, args) ->
          let args = map_l self.print_arg self args in
          Print (std, args)
      | Iterate (v, vars, cat_filters, body) ->
          let v = self.var v in
          let vars = map_l self.access self vars in
          let cat_filters =
            List.map
              (fun (a, e, sp) -> (a, self.expr self e, self.varspace self sp))
              cat_filters
          in
          let body = map_l self.instr self body in
          Iterate (v, vars, cat_filters, body)
      | Iterate_values (v, intervals, body) ->
          let v = self.var v in
          let intervals =
            List.map
              (fun (a, b, c) ->
                (self.expr self a, self.expr self b, self.expr self c))
              intervals
          in
          let body = map_l self.instr self body in
          Iterate_values (v, intervals, body)
      | Restore (als, cat_filters, events, event_filters, body) ->
          let als = map_l self.access self als in
          let cat_filters =
            List.map
              (fun (v, a, e, sp) ->
                (self.var v, a, self.expr self e, self.varspace self sp))
              cat_filters
          in
          let events = map_l self.expr self events in
          let event_filters =
            List.map
              (fun (v, e) -> (self.var v, self.expr self e))
              event_filters
          in
          let body = map_l self.instr self body in
          Restore (als, cat_filters, events, event_filters, body)
      | ArrangeEvents (a, b, c, body) ->
          let a =
            Option.map
              (fun (v, v', e) -> (self.var v, self.var v', self.expr self e))
              a
          in
          let b = Option.map (fun (v, e) -> (self.var v, self.expr self e)) b in
          let c = Option.map (fun e -> self.expr self e) c in
          let body = map_l self.instr self body in
          ArrangeEvents (a, b, c, body)
      | Switch (sexpr, cases) ->
          let sexpr = self.switch self sexpr in
          let cases =
            List.map
              (fun (cases, body) ->
                let cases = map_l self.case self cases in
                let body = map_l self.instr self body in
                (cases, body))
              cases
          in
          Switch (sexpr, cases)
      | RaiseError (err, b) ->
          let err = Pos.map self.err err in
          RaiseError (err, b)
      | CleanErrors -> CleanErrors
      | CleanFinalizedErrors -> CleanFinalizedErrors
      | ExportErrors -> ExportErrors
      | FinalizeErrors -> FinalizeErrors
      | Stop k -> Stop k
    in
    Pos.same ret instr

  let switch self e =
    match e with
    | SEValue e -> SEValue (self.expr self e)
    | SESameVariable a -> SESameVariable (self.access self a)

  let formula self formula =
    let ret =
      match Pos.unmark formula with
      | SingleFormula decl -> SingleFormula (self.formula_decl self decl)
      | MultipleFormulaes (loop, decl) ->
          let loop = self.loop_variables self loop in
          let decl = self.formula_decl self decl in
          MultipleFormulaes (loop, decl)
    in
    Pos.same ret formula

  let formula_decl self decl =
    match decl with
    | VarDecl (id, expr) ->
        let id = self.access self id in
        let expr = self.expr self expr in
        VarDecl (id, expr)
    | EventFieldRef (expr, b, c, v) ->
        let expr = self.expr self expr in
        let v = self.var v in
        EventFieldRef (expr, b, c, v)

  let print_arg self arg =
    let ret =
      match Pos.unmark arg with
      | PrintString s -> PrintString s
      | PrintAccess (a, acc) ->
          let acc = self.access self acc in
          PrintAccess (a, acc)
      | PrintIndent e -> PrintIndent (self.expr self e)
      | PrintExpr (e, b, c) ->
          let e = self.expr self e in
          PrintExpr (e, b, c)
    in
    Pos.same ret arg

  let expr self expr =
    let ret =
      match Pos.unmark expr with
      | TestInSet (a, e, svs) ->
          let e = self.expr self e in
          let svs = map_l self.set_value self svs in
          TestInSet (a, e, svs)
      | Unop (op, e) -> Unop (op, self.expr self e)
      | Comparison (op, l, r) ->
          let l = self.expr self l in
          let r = self.expr self r in
          Comparison (op, l, r)
      | Binop (op, l, r) ->
          let l = self.expr self l in
          let r = self.expr self r in
          Binop (op, l, r)
      | Conditional (cond, texpr, eexpr) ->
          let cond = self.expr self cond in
          let texpr = self.expr self texpr in
          let eexpr = Option.map (self.expr self) eexpr in
          Conditional (cond, texpr, eexpr)
      | FuncCall (a, args) ->
          let args = map_l self.expr self args in
          FuncCall (a, args)
      | FuncCallLoop (a, lvs, arg) ->
          let lvs = self.loop_variables self lvs in
          let arg = self.expr self arg in
          FuncCallLoop (a, lvs, arg)
      | Literal l -> Literal l
      | Var v -> Var (Pos.unmark (self.access self (Pos.same v expr)))
      | Loop (lvs, e) ->
          let lvs = self.loop_variables self lvs in
          let e = self.expr self e in
          Loop (lvs, e)
      | NbCategory a -> NbCategory a
      | Attribut (acc, b) ->
          let acc = self.access self acc in
          Attribut (acc, b)
      | Size acc -> Size (self.access self acc)
      | Type (a, b) -> Type (self.access self a, b)
      | SameVariable (a, b) ->
          let a = self.access self a in
          let b = self.access self b in
          SameVariable (a, b)
      | InDomain (a, b) -> InDomain (self.access self a, b)
      | NbAnomalies -> NbAnomalies
      | NbDiscordances -> NbDiscordances
      | NbInformatives -> NbInformatives
      | NbBloquantes -> NbBloquantes
    in
    Pos.same ret expr

  let set_value self sv =
    match sv with
    | FloatValue f -> FloatValue f
    | VarValue a -> VarValue (self.access self a)
    | IntervalValue (l, r) -> IntervalValue (l, r)

  let loop_variables self lvs =
    let ret =
      match Pos.unmark lvs with
      | ValueSets lvs ->
          let lvs = map_l self.loop_variable self lvs in
          ValueSets lvs
      | Ranges lvs ->
          let lvs = map_l self.loop_variable self lvs in
          Ranges lvs
    in
    Pos.same ret lvs

  let loop_variable self (a, ranges) = (a, map_l self.loop_range self ranges)

  let loop_range self range =
    match range with
    | Single atom -> Single (self.atom self atom)
    | Range (l, r) ->
        let l = self.atom self l in
        let r = self.atom self r in
        Range (l, r)
    | Interval (l, r) ->
        let l = self.atom self l in
        let r = self.atom self r in
        Interval (l, r)

  let case self case =
    match case with
    | CDefault -> CDefault
    | CValue v -> CValue v
    | CVar a -> CVar (self.access self a)

  let access self a =
    let ret =
      match Pos.unmark a with
      | VarAccess (sp, v) ->
          let sp = self.varspace self sp in
          VarAccess (sp, self.var v)
      | TabAccess ((sp, v), e) ->
          let sp = self.varspace self sp in
          let v = self.var v in
          let e = self.expr self e in
          TabAccess ((sp, v), e)
      | FieldAccess (sp, e, c, d) ->
          let sp = self.varspace self sp in
          let e = self.expr self e in
          FieldAccess (sp, e, c, d)
    in
    Pos.same ret a

  let atom self atom =
    let ret =
      match Pos.unmark atom with
      | AtomVar v -> AtomVar (self.var v)
      | AtomLiteral l -> AtomLiteral l
    in
    Pos.same ret atom

  let varspace _self space = space

  let default var err =
    {
      instr;
      switch;
      formula;
      formula_decl;
      print_arg;
      set_value;
      loop_variables;
      loop_variable;
      loop_range;
      expr;
      access;
      atom;
      case;
      var;
      err;
      varspace;
    }
end

module Fold = struct
  type ('a, 'v, 'e) folder = {
    instr : ('a, 'v, 'e) folder -> ('v, 'e) m_instruction -> 'a -> 'a;
    switch : ('a, 'v, 'e) folder -> 'v switch_expression -> 'a -> 'a;
    formula : ('a, 'v, 'e) folder -> 'v formula Pos.marked -> 'a -> 'a;
    formula_decl : ('a, 'v, 'e) folder -> 'v formula_decl -> 'a -> 'a;
    print_arg : ('a, 'v, 'e) folder -> 'v print_arg Pos.marked -> 'a -> 'a;
    set_value : ('a, 'v, 'e) folder -> 'v set_value -> 'a -> 'a;
    loop_variables :
      ('a, 'v, 'e) folder -> 'v loop_variables Pos.marked -> 'a -> 'a;
    loop_variable : ('a, 'v, 'e) folder -> 'v loop_variable -> 'a -> 'a;
    loop_range : ('a, 'v, 'e) folder -> 'v loop_range -> 'a -> 'a;
    expr : ('a, 'v, 'e) folder -> 'v m_expression -> 'a -> 'a;
    access : ('a, 'v, 'e) folder -> 'v m_access -> 'a -> 'a;
    case : ('a, 'v, 'e) folder -> 'v case -> 'a -> 'a;
    atom : ('a, 'v, 'e) folder -> 'v atom Pos.marked -> 'a -> 'a;
    varspace : ('a, 'v, 'e) folder -> var_space -> 'a -> 'a;
  }

  let fold_l f self = List.fold_left (fun acc x -> f self x acc)

  let fold_proj f self p = List.fold_left (fun acc x -> f self (p x) acc)

  let fold_filters self =
    List.fold_left (fun acc (_, e, sp) ->
        let acc = self.varspace self sp acc in
        self.expr self e acc)

  let instr self instr acc =
    match Pos.unmark instr with
    | Affectation form -> self.formula self form acc
    | IfThenElse (e, tbody, fbody) ->
        let acc = self.expr self e acc in
        let acc = fold_l self.instr self acc tbody in
        fold_l self.instr self acc fbody
    | WhenDoElse (branches, else_body) ->
        let fold_branch (guard, body, _) acc =
          let acc = self.expr self guard acc in
          fold_l self.instr self acc body
        in
        let acc =
          List.fold_left (fun acc b -> fold_branch b acc) acc branches
        in
        fold_l self.instr self acc (Pos.unmark else_body)
    | ComputeDomain (_, sp) -> self.varspace self sp acc
    | ComputeChaining (_, sp) -> self.varspace self sp acc
    | ComputeVerifs (_, expr, sp) ->
        let acc = self.varspace self sp acc in
        self.expr self expr acc
    | ComputeTarget (_, params, sp) ->
        let acc = self.varspace self sp acc in
        fold_l self.access self acc params
    | VerifBlock body -> fold_l self.instr self acc body
    | Print (_, args) -> fold_l self.print_arg self acc args
    | Iterate (_, vars, cat_filters, body) ->
        let acc = fold_l self.access self acc vars in
        let acc = fold_filters self acc cat_filters in
        fold_l self.instr self acc body
    | Iterate_values (_, intervals, body) ->
        let acc =
          List.fold_left
            (fun acc (start, _end, step) ->
              let acc = self.expr self start acc in
              let acc = self.expr self _end acc in
              self.expr self step acc)
            acc intervals
        in
        fold_l self.instr self acc body
    | Restore (als, cat_filters, events, event_filters, body) ->
        let acc = fold_l self.access self acc als in
        let acc =
          fold_proj self.expr self (fun (_, _, e, _) -> e) acc cat_filters
        in
        let acc =
          fold_proj self.varspace self (fun (_, _, _, sp) -> sp) acc cat_filters
        in
        let acc = fold_l self.expr self acc events in
        let acc =
          fold_proj self.expr self (fun (_, e) -> e) acc event_filters
        in
        fold_l self.instr self acc body
    | ArrangeEvents (a, b, c, body) ->
        let acc =
          Option.fold ~none:acc ~some:(fun (_, _, e) -> self.expr self e acc) a
        in
        let acc =
          Option.fold ~none:acc ~some:(fun (_, e) -> self.expr self e acc) b
        in
        let acc =
          Option.fold ~none:acc ~some:(fun e -> self.expr self e acc) c
        in
        fold_l self.instr self acc body
    | Switch (sexpr, cases) ->
        let acc = self.switch self sexpr acc in
        let acc =
          List.fold_left
            (fun acc (cases, body) ->
              let acc = fold_l self.case self acc cases in
              fold_l self.instr self acc body)
            acc cases
        in
        acc
    | RaiseError (_, _)
    | CleanErrors | CleanFinalizedErrors | ExportErrors | FinalizeErrors
    | Stop _ ->
        acc

  let switch self e acc =
    match e with
    | SEValue e -> self.expr self e acc
    | SESameVariable a -> self.access self a acc

  let formula self formula acc =
    match Pos.unmark formula with
    | SingleFormula decl -> self.formula_decl self decl acc
    | MultipleFormulaes (loop, decl) ->
        let acc = self.loop_variables self loop acc in
        self.formula_decl self decl acc

  let formula_decl self decl acc =
    match decl with
    | VarDecl (id, expr) ->
        let acc = self.access self id acc in
        self.expr self expr acc
    | EventFieldRef (e, _, _, _) -> self.expr self e acc

  let print_arg self arg acc =
    match Pos.unmark arg with
    | PrintAccess (_, a) -> self.access self a acc
    | PrintIndent e | PrintExpr (e, _, _) -> self.expr self e acc
    | PrintString _ -> acc

  let expr self expr acc =
    match Pos.unmark expr with
    | TestInSet (_, e, set) ->
        let acc = self.expr self e acc in
        fold_l self.set_value self acc set
    | Unop (_, e) -> self.expr self e acc
    | Comparison (_, l, r) | Binop (_, l, r) ->
        let acc = self.expr self l acc in
        self.expr self r acc
    | Conditional (cond, texp, fexp) ->
        let acc = self.expr self cond acc in
        let acc = self.expr self texp acc in
        Option.fold ~none:acc ~some:(fun e -> self.expr self e acc) fexp
    | FuncCall (_, args) -> fold_l self.expr self acc args
    | FuncCallLoop (_, lvs, e) ->
        let acc = self.loop_variables self lvs acc in
        self.expr self e acc
    | Loop (lvs, e) ->
        let acc = self.loop_variables self lvs acc in
        self.expr self e acc
    | Var a -> self.access self (Pos.same a expr) acc
    | Attribut (a, _) -> self.access self a acc
    | Size a -> self.access self a acc
    | Type (a, _) -> self.access self a acc
    | SameVariable (a, b) ->
        let acc = self.access self a acc in
        self.access self b acc
    | InDomain (a, _) -> self.access self a acc
    | NbAnomalies | NbDiscordances | NbInformatives | NbBloquantes
    | NbCategory _ | Literal _ ->
        acc

  let set_value self sv acc =
    match sv with
    | FloatValue _ -> acc
    | VarValue a -> self.access self a acc
    | IntervalValue (_, _) -> acc

  let loop_variables self lvs acc =
    match Pos.unmark lvs with
    | ValueSets lvs | Ranges lvs -> fold_l self.loop_variable self acc lvs

  let loop_variable self (_, lrs) acc = fold_l self.loop_range self acc lrs

  let loop_range self range acc =
    match range with
    | Single atom -> self.atom self atom acc
    | Range (l, r) | Interval (l, r) ->
        let acc = self.atom self l acc in
        self.atom self r acc

  let case self case acc =
    match case with
    | CDefault | CValue _ -> acc
    | CVar a -> self.access self a acc

  let access self a acc =
    match Pos.unmark a with
    | VarAccess (sp, _) -> self.varspace self sp acc
    | TabAccess ((sp, _), e) ->
        let acc = self.varspace self sp acc in
        self.expr self e acc
    | FieldAccess (sp, e, _, _) ->
        let acc = self.varspace self sp acc in
        self.expr self e acc

  let atom _self _atom acc = acc

  let varspace _self _vs acc = acc

  let default =
    {
      instr;
      switch;
      formula;
      formula_decl;
      print_arg;
      set_value;
      loop_variables;
      loop_variable;
      loop_range;
      expr;
      access;
      atom;
      case;
      varspace;
    }
end

let rec access_map_var f = function
  | VarAccess (m_sp_opt, v) -> VarAccess (m_sp_opt, f v)
  | TabAccess ((m_sp_opt, v), m_i) ->
      let v' = f v in
      let m_i' = m_expr_map_var f m_i in
      TabAccess ((m_sp_opt, v'), m_i')
  | FieldAccess (m_sp_opt, m_i, field, id) ->
      let m_i' = m_expr_map_var f m_i in
      FieldAccess (m_sp_opt, m_i', field, id)

and m_access_map_var f m_access = Pos.map (access_map_var f) m_access

and set_value_map_var f = function
  | FloatValue value -> FloatValue value
  | VarValue m_access ->
      let m_access' = m_access_map_var f m_access in
      VarValue m_access'
  | IntervalValue (i0, i1) -> IntervalValue (i0, i1)

and atom_map_var f = function
  | AtomVar v -> AtomVar (f v)
  | AtomLiteral l -> AtomLiteral l

and m_atom_map_var f m_a = Pos.map (atom_map_var f) m_a

and loop_range_map_var f = function
  | Single m_a0 -> Single (m_atom_map_var f m_a0)
  | Range (m_a0, m_a1) ->
      let m_a0' = m_atom_map_var f m_a0 in
      let m_a1' = m_atom_map_var f m_a1 in
      Range (m_a0', m_a1')
  | Interval (m_a0, m_a1) ->
      let m_a0' = m_atom_map_var f m_a0 in
      let m_a1' = m_atom_map_var f m_a1 in
      Interval (m_a0', m_a1')

and loop_variable_map_var f (m_ch, svl) =
  let svl' = List.map (loop_range_map_var f) svl in
  (m_ch, svl')

and loop_variables_map_var f = function
  | ValueSets lvl -> ValueSets (List.map (loop_variable_map_var f) lvl)
  | Ranges lvl -> Ranges (List.map (loop_variable_map_var f) lvl)

and expr_map_var f = function
  | TestInSet (positive, m_e0, values) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let values' = List.map (set_value_map_var f) values in
      TestInSet (positive, m_e0', values')
  | Unop (op, m_e0) -> Unop (op, m_expr_map_var f m_e0)
  | Comparison (op, m_e0, m_e1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      Comparison (op, m_e0', m_e1')
  | Binop (op, m_e0, m_e1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      Binop (op, m_e0', m_e1')
  | Conditional (m_e0, m_e1, m_e2_opt) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_e1' = m_expr_map_var f m_e1 in
      let m_e2_opt' = Option.map (m_expr_map_var f) m_e2_opt in
      Conditional (m_e0', m_e1', m_e2_opt')
  | FuncCall (fn, m_el) ->
      let m_el' = List.map (m_expr_map_var f) m_el in
      FuncCall (fn, m_el')
  | FuncCallLoop (fn, m_loop, m_e0) ->
      let m_loop' = Pos.map (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      FuncCallLoop (fn, m_loop', m_e0')
  | Literal l -> Literal l
  | Var access -> Var (access_map_var f access)
  | Loop (m_loop, m_e0) ->
      let m_loop' = Pos.map (loop_variables_map_var f) m_loop in
      let m_e0' = m_expr_map_var f m_e0 in
      Loop (m_loop', m_e0')
  | NbCategory cvm -> NbCategory cvm
  | Attribut (m_access, attr) ->
      let m_access' = m_access_map_var f m_access in
      Attribut (m_access', attr)
  | Size m_access -> Size (m_access_map_var f m_access)
  | Type (m_access, m_typ) -> Type (m_access_map_var f m_access, m_typ)
  | SameVariable (m_access0, m_access1) ->
      let m_access0' = m_access_map_var f m_access0 in
      let m_access1' = m_access_map_var f m_access1 in
      SameVariable (m_access0', m_access1')
  | InDomain (m_access, cvm) ->
      let m_access' = m_access_map_var f m_access in
      InDomain (m_access', cvm)
  | NbAnomalies -> NbAnomalies
  | NbDiscordances -> NbDiscordances
  | NbInformatives -> NbInformatives
  | NbBloquantes -> NbBloquantes

and m_expr_map_var f e = Pos.map (expr_map_var f) e

let rec print_arg_map_var f = function
  | PrintString s -> PrintString s
  | PrintAccess (info, m_a) -> PrintAccess (info, m_access_map_var f m_a)
  | PrintIndent m_e0 -> PrintIndent (m_expr_map_var f m_e0)
  | PrintExpr (m_e0, i0, i1) -> PrintExpr (m_expr_map_var f m_e0, i0, i1)

and formula_loop_map_var f m_lvs = Pos.map (loop_variables_map_var f) m_lvs

and formula_decl_map_var f = function
  | VarDecl (m_access, m_e1) ->
      let m_access' = m_access_map_var f m_access in
      let m_e1' = m_expr_map_var f m_e1 in
      VarDecl (m_access', m_e1')
  | EventFieldRef (m_e0, m_if, id, v) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let v' = f v in
      EventFieldRef (m_e0', m_if, id, v')

and formula_map_var f = function
  | SingleFormula fd -> SingleFormula (formula_decl_map_var f fd)
  | MultipleFormulaes (fl, fd) ->
      let fl' = formula_loop_map_var f fl in
      let fd' = formula_decl_map_var f fd in
      MultipleFormulaes (fl', fd')

and case_map_var f = function
  | CDefault -> CDefault
  | CValue v -> CValue v
  | CVar acc -> CVar (m_access_map_var f acc)

and switch_expr_map_var f = function
  | SEValue e -> SEValue (m_expr_map_var f e)
  | SESameVariable m_a -> SESameVariable (m_access_map_var f m_a)

and instr_map_var f g = function
  | Affectation m_f -> Affectation (Pos.map (formula_map_var f) m_f)
  | IfThenElse (m_e0, m_il0, m_il1) ->
      let m_e0' = m_expr_map_var f m_e0 in
      let m_il0' = List.map (m_instr_map_var f g) m_il0 in
      let m_il1' = List.map (m_instr_map_var f g) m_il1 in
      IfThenElse (m_e0', m_il0', m_il1')
  | Switch (e, l) ->
      let e' = switch_expr_map_var f e in
      let l' =
        List.map
          (fun (c, l) ->
            (List.map (case_map_var f) c, List.map (m_instr_map_var f g) l))
          l
      in
      Switch (e', l')
  | WhenDoElse (m_eil, m_il) ->
      let map (m_e0, m_il0, pos) =
        let m_e0' = m_expr_map_var f m_e0 in
        let m_il0' = List.map (m_instr_map_var f g) m_il0 in
        (m_e0', m_il0', pos)
      in
      let m_eil' = List.map map m_eil in
      let m_il' = Pos.map (List.map (m_instr_map_var f g)) m_il in
      WhenDoElse (m_eil', m_il')
  | ComputeDomain (dom, m_sp_opt) -> ComputeDomain (dom, m_sp_opt)
  | ComputeChaining (ch, m_sp_opt) -> ComputeChaining (ch, m_sp_opt)
  | ComputeVerifs (m_sl, m_e0, m_sp_opt) ->
      let m_e0' = m_expr_map_var f m_e0 in
      ComputeVerifs (m_sl, m_e0', m_sp_opt)
  | ComputeTarget (tn, args, m_sp_opt) ->
      let args' = List.map (m_access_map_var f) args in
      ComputeTarget (tn, args', m_sp_opt)
  | VerifBlock m_il0 -> VerifBlock (List.map (m_instr_map_var f g) m_il0)
  | Print (pr_std, pr_args) ->
      let pr_args' = List.map (Pos.map (print_arg_map_var f)) pr_args in
      Print (pr_std, pr_args')
  | Iterate (v, al, cvml, m_il) ->
      let v' = f v in
      let al' = List.map (m_access_map_var f) al in
      let cvml' =
        let map (cvm, m_e, m_sp_opt) = (cvm, m_expr_map_var f m_e, m_sp_opt) in
        List.map map cvml
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Iterate (v', al', cvml', m_il')
  | Iterate_values (v, e3l, m_il) ->
      let v' = f v in
      let e3l' =
        let map (m_e0, m_e1, m_e2) =
          let m_e0' = m_expr_map_var f m_e0 in
          let m_e1' = m_expr_map_var f m_e1 in
          let m_e2' = m_expr_map_var f m_e2 in
          (m_e0', m_e1', m_e2')
        in
        List.map map e3l
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Iterate_values (v', e3l', m_il')
  | Restore (al, cvml, el, vel, m_il) ->
      let al' = List.map (m_access_map_var f) al in
      let cvml' =
        let map (v, cvm, m_e0, m_sp_opt) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', cvm, m_e0', m_sp_opt)
        in
        List.map map cvml
      in
      let el' = List.map (m_expr_map_var f) el in
      let vel' =
        let map (v, m_e0) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', m_e0')
        in
        List.map map vel
      in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      Restore (al', cvml', el', vel', m_il')
  | ArrangeEvents (vve_opt, ve_opt, e_opt, m_il) ->
      let vve_opt' =
        let map (v0, v1, m_e0) =
          let v0' = f v0 in
          let v1' = f v1 in
          let m_e0' = m_expr_map_var f m_e0 in
          (v0', v1', m_e0')
        in
        Option.map map vve_opt
      in
      let ve_opt' =
        let map (v, m_e0) =
          let v' = f v in
          let m_e0' = m_expr_map_var f m_e0 in
          (v', m_e0')
        in
        Option.map map ve_opt
      in
      let e_opt' = Option.map (m_expr_map_var f) e_opt in
      let m_il' = List.map (m_instr_map_var f g) m_il in
      ArrangeEvents (vve_opt', ve_opt', e_opt', m_il')
  | RaiseError (m_err, m_s_opt) ->
      let m_err' = Pos.map g m_err in
      RaiseError (m_err', m_s_opt)
  | CleanErrors -> CleanErrors
  | CleanFinalizedErrors -> CleanFinalizedErrors
  | ExportErrors -> ExportErrors
  | FinalizeErrors -> FinalizeErrors
  | Stop s -> Stop s

and m_instr_map_var f g m_i = Pos.map (instr_map_var f g) m_i

(** {2 Folder} *)

type var_usage = Read | Write | Info | DeclRef | ArgRef | DeclLocal | Macro

let fold_list fold l acc = List.fold_left (fun a e -> fold e a) acc l

let fold_opt fold opt acc = match opt with Some e -> fold e acc | None -> acc

let rec access_fold_var usage f a acc =
  match a with
  | VarAccess (m_sp_opt, v) -> acc |> f usage m_sp_opt (Some v)
  | TabAccess ((m_sp_opt, v), m_i) ->
      acc |> f usage m_sp_opt (Some v) |> m_expr_fold_var f m_i
  | FieldAccess (m_sp_opt, m_i, _, _) ->
      acc |> f usage m_sp_opt None |> m_expr_fold_var f m_i

and m_access_fold_var usage f m_access acc =
  acc |> access_fold_var usage f (Pos.unmark m_access)

and set_value_fold_var f sv acc =
  match sv with
  | FloatValue _ -> acc
  | VarValue m_access -> acc |> m_access_fold_var Read f m_access
  | IntervalValue _ -> acc

and atom_fold_var f a acc =
  match a with
  | AtomVar v -> acc |> f Macro None (Some v)
  | AtomLiteral _ -> acc

and m_atom_fold_var f m_a acc = acc |> atom_fold_var f (Pos.unmark m_a)

and loop_range_fold_var f svl acc =
  match svl with
  | Single m_a0 -> acc |> m_atom_fold_var f m_a0
  | Range (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1
  | Interval (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1

and loop_variable_fold_var f (_, svl) acc =
  fold_list (loop_range_fold_var f) svl acc

and loop_variables_fold_var f lv acc =
  match lv with
  | ValueSets lvl -> fold_list (loop_variable_fold_var f) lvl acc
  | Ranges lvl -> fold_list (loop_variable_fold_var f) lvl acc

and expr_fold_var f e acc =
  match e with
  | TestInSet (_, m_e0, values) ->
      acc |> m_expr_fold_var f m_e0 |> fold_list (set_value_fold_var f) values
  | Unop (_, m_e0) -> m_expr_fold_var f m_e0 acc
  | Comparison (_, m_e0, m_e1) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
  | Binop (_, m_e0, m_e1) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
  | Conditional (m_e0, m_e1, m_e2_opt) ->
      acc |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
      |> fold_opt (m_expr_fold_var f) m_e2_opt
  | FuncCall (_, m_el) -> fold_list (m_expr_fold_var f) m_el acc
  | FuncCallLoop (_, m_loop, m_e0) ->
      acc
      |> loop_variables_fold_var f (Pos.unmark m_loop)
      |> m_expr_fold_var f m_e0
  | Literal _ -> acc
  | Var access -> access_fold_var Read f access acc
  | Loop (m_loop, m_e0) ->
      acc
      |> loop_variables_fold_var f (Pos.unmark m_loop)
      |> m_expr_fold_var f m_e0
  | NbCategory _ -> acc
  | Attribut (m_access, _) -> m_access_fold_var Info f m_access acc
  | Size m_access -> m_access_fold_var Info f m_access acc
  | Type (m_access, _) -> m_access_fold_var Info f m_access acc
  | SameVariable (m_access0, m_access1) ->
      acc
      |> m_access_fold_var Info f m_access0
      |> m_access_fold_var Info f m_access1
  | InDomain (m_access, _) -> m_access_fold_var Info f m_access acc
  | NbAnomalies -> acc
  | NbDiscordances -> acc
  | NbInformatives -> acc
  | NbBloquantes -> acc

and m_expr_fold_var f e acc = expr_fold_var f (Pos.unmark e) acc

let rec print_arg_fold_var f pa acc =
  match pa with
  | PrintString _ -> acc
  | PrintAccess (_, m_a) -> m_access_fold_var Info f m_a acc
  | PrintIndent m_e0 -> m_expr_fold_var f m_e0 acc
  | PrintExpr (m_e0, _, _) -> m_expr_fold_var f m_e0 acc

and m_print_arg_fold_var f m_pa acc = print_arg_fold_var f (Pos.unmark m_pa) acc

and formula_loop_fold_var f m_lvs acc =
  loop_variables_fold_var f (Pos.unmark m_lvs) acc

and formula_decl_fold_var f fd acc =
  match fd with
  | VarDecl (m_access, m_e1) ->
      acc |> m_access_fold_var Write f m_access |> m_expr_fold_var f m_e1
  | EventFieldRef (m_e0, _, _, v) ->
      acc |> m_expr_fold_var f m_e0 |> f ArgRef None (Some v)

and formula_fold_var f fm acc =
  match fm with
  | SingleFormula fd -> formula_decl_fold_var f fd acc
  | MultipleFormulaes (fl, fd) ->
      acc |> formula_loop_fold_var f fl |> formula_decl_fold_var f fd

and switch_expr_fold_var f se acc =
  match se with
  | SEValue e -> m_expr_fold_var f e acc
  | SESameVariable v -> m_access_fold_var Info f v acc

and instr_fold_var f instr acc =
  match instr with
  | Affectation m_f -> formula_fold_var f (Pos.unmark m_f) acc
  | IfThenElse (m_e0, m_il0, m_il1) ->
      acc |> m_expr_fold_var f m_e0
      |> fold_list (m_instr_fold_var f) m_il0
      |> fold_list (m_instr_fold_var f) m_il1
  | Switch (e, l) ->
      acc |> switch_expr_fold_var f e
      |> fold_list (fun (_, l) -> fold_list (m_instr_fold_var f) l) l
  | WhenDoElse (m_eil, m_il) ->
      let fold (m_e0, m_il0, _) accu =
        accu |> m_expr_fold_var f m_e0 |> fold_list (m_instr_fold_var f) m_il0
      in
      acc |> fold_list fold m_eil
      |> fold_list (m_instr_fold_var f) (Pos.unmark m_il)
  | ComputeDomain _ -> acc
  | ComputeChaining _ -> acc
  | ComputeVerifs (_, m_e0, _) -> m_expr_fold_var f m_e0 acc
  | ComputeTarget (_, args, _) ->
      fold_list (m_access_fold_var ArgRef f) args acc
  | VerifBlock m_il0 -> fold_list (m_instr_fold_var f) m_il0 acc
  | Print (_, pr_args) -> fold_list (m_print_arg_fold_var f) pr_args acc
  | Iterate (v, al, cvml, m_il) ->
      acc |> f DeclRef None (Some v)
      |> fold_list (m_access_fold_var ArgRef f) al
      |> (let fold (_, m_e, m_sp_opt) accu =
            accu |> f DeclRef m_sp_opt (Some v) |> m_expr_fold_var f m_e
          in
          fold_list fold cvml)
      |> fold_list (m_instr_fold_var f) m_il
  | Iterate_values (v, e3l, m_il) ->
      acc |> f DeclLocal None (Some v)
      |> (let fold (m_e0, m_e1, m_e2) accu =
            accu |> m_expr_fold_var f m_e0 |> m_expr_fold_var f m_e1
            |> m_expr_fold_var f m_e2
          in
          fold_list fold e3l)
      |> fold_list (m_instr_fold_var f) m_il
  | Restore (al, cvml, el, vel, m_il) ->
      acc
      |> fold_list (m_access_fold_var ArgRef f) al
      |> (let fold (v, _, m_e0, m_sp_opt) accu =
            accu |> f DeclRef m_sp_opt (Some v) |> m_expr_fold_var f m_e0
          in
          fold_list fold cvml)
      |> fold_list (m_expr_fold_var f) el
      |> (let fold (v, m_e0) accu =
            accu |> f DeclLocal None (Some v) |> m_expr_fold_var f m_e0
          in
          fold_list fold vel)
      |> fold_list (m_instr_fold_var f) m_il
  | ArrangeEvents (vve_opt, ve_opt, e_opt, m_il) ->
      acc
      |> (let fold (v0, v1, m_e0) accu =
            accu |> f DeclLocal None (Some v0) |> f DeclLocal None (Some v1)
            |> m_expr_fold_var f m_e0
          in
          fold_opt fold vve_opt)
      |> (let fold (v, m_e0) accu =
            accu |> f DeclLocal None (Some v) |> m_expr_fold_var f m_e0
          in
          fold_opt fold ve_opt)
      |> fold_opt (m_expr_fold_var f) e_opt
      |> fold_list (m_instr_fold_var f) m_il
  | RaiseError _ -> acc
  | CleanErrors -> acc
  | CleanFinalizedErrors -> acc
  | ExportErrors -> acc
  | FinalizeErrors -> acc
  | Stop _ -> acc

and m_instr_fold_var f m_i acc = instr_fold_var f (Pos.unmark m_i) acc
