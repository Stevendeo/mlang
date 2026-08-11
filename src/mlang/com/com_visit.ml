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

and set_value_loop_map_var f = function
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
  let svl' = List.map (set_value_loop_map_var f) svl in
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

and set_value_loop_fold_var f svl acc =
  match svl with
  | Single m_a0 -> acc |> m_atom_fold_var f m_a0
  | Range (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1
  | Interval (m_a0, m_a1) ->
      acc |> m_atom_fold_var f m_a0 |> m_atom_fold_var f m_a1

and loop_variable_fold_var f (_, svl) acc =
  fold_list (set_value_loop_fold_var f) svl acc

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
