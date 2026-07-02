open Comcom

let format_value_typ fmt t =
  Pp.string fmt
    (match t with
    | Boolean -> "BOOLEEN"
    | DateYear -> "DATE_AAAA"
    | DateDayMonthYear -> "DATE_JJMMAAAA"
    | DateMonth -> "DATE_MM"
    | Integer -> "ENTIER"
    | Real -> "REEL")

let format_literal fmt l =
  match l with
  | Float f -> Format.fprintf fmt "%g" f
  | Undefined -> Format.pp_print_string fmt "indefini"

let format_atom form_var fmt vl =
  match vl with
  | AtomVar v -> form_var fmt v
  | AtomLiteral l -> format_literal fmt l.lit

let format_set_value_loop form_var fmt sv =
  let form_atom = format_atom form_var in
  match sv with
  | Single l -> Format.fprintf fmt "%a" form_atom (Pos.unmark l)
  | Range (i1, i2) ->
      Format.fprintf fmt "%a..%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)
  | Interval (i1, i2) ->
      Format.fprintf fmt "%a-%a" form_atom (Pos.unmark i1) form_atom
        (Pos.unmark i2)

let format_loop_variable_ranges form_var fmt (v, vs) =
  Format.fprintf fmt "un %c dans %a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variable_value_set form_var fmt (v, vs) =
  Format.fprintf fmt "%c=%a" (Pos.unmark v)
    (Pp.list_comma (format_set_value_loop form_var))
    vs

let format_loop_variables form_var fmt lvs =
  match lvs with
  | ValueSets vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        (format_loop_variable_value_set form_var)
        fmt vvs
  | Ranges vvs ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " et ")
        (format_loop_variable_ranges form_var)
        fmt vvs

let format_unop fmt op =
  Format.pp_print_string fmt (match op with Not -> "non" | Minus -> "-")

let format_binop fmt op =
  Format.pp_print_string fmt
    (match op with
    | And -> "et"
    | Or -> "ou"
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%")

let format_comp_op fmt op =
  Format.pp_print_string fmt
    (match op with
    | Gt -> ">"
    | Gte -> ">="
    | Lt -> "<"
    | Lte -> "<="
    | Eq -> "="
    | Neq -> "!=")

let format_varid form_var fmt (m_sp_opt, v) =
  let sp_str =
    match m_sp_opt with
    | None -> ""
    | Some (m_sp, _) -> get_var_name (Pos.unmark m_sp) ^ "."
  in
  Pp.fpr fmt "%s%a" sp_str form_var v

let format_func fmt f =
  Format.pp_print_string fmt
    (match f with
    | SumFunc -> "somme"
    | AbsFunc -> "abs"
    | MinFunc -> "min"
    | MaxFunc -> "max"
    | GtzFunc -> "positif"
    | GtezFunc -> "positif_ou_nul"
    | NullFunc -> "null"
    | ArrFunc -> "arr"
    | InfFunc -> "inf"
    | PresentFunc -> "present"
    | Multimax -> "multimax"
    | Supzero -> "supzero"
    | VerifNumber -> "numero_verif"
    | ComplNumber -> "numero_compl"
    | NbEvents -> "nb_evenements"
    | Func fn -> fn)

let rec format_expression form_var fmt =
  let form_expr = format_expression form_var in
  function
  | TestInSet (belong, e, values) ->
      Format.fprintf fmt "(%a %sdans %a)" form_expr (Pos.unmark e)
        (if belong then "" else "non ")
        (Pp.list_comma (format_set_value form_var))
        values
  | Comparison (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_comp_op
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" form_expr (Pos.unmark e1) format_binop
        (Pos.unmark op) form_expr (Pos.unmark e2)
  | Unop (op, e) ->
      Format.fprintf fmt "%a %a" format_unop op form_expr (Pos.unmark e)
  | Conditional (e1, e2, e3) ->
      let pp_sinon fmt e = Format.fprintf fmt " sinon %a" form_expr e in
      Format.fprintf fmt "(si %a alors %a%a finsi)" form_expr (Pos.unmark e1)
        form_expr (Pos.unmark e2)
        (Pp.option (Pp.unmark pp_sinon))
        e3
  | FuncCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func (Pos.unmark f)
        (Pp.list_space (Pp.unmark form_expr))
        args
  | FuncCallLoop (f, lvs, e) ->
      Format.fprintf fmt "%a(%a%a)" format_func (Pos.unmark f)
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | Literal { lit; _ } -> format_literal fmt lit
  | Var acc -> format_access form_var fmt acc
  | Loop (lvs, e) ->
      Format.fprintf fmt "pour %a%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs) form_expr (Pos.unmark e)
  | NbCategory cs ->
      Format.fprintf fmt "nb_categorie(%a)" (CatVar.Map.pp_keys ()) cs
  | Attribut (m_acc, a) ->
      Format.fprintf fmt "attribut(%a, %s)" (format_access form_var)
        (Pos.unmark m_acc) (Pos.unmark a)
  | Size m_acc ->
      Format.fprintf fmt "taille(%a)" (format_access form_var)
        (Pos.unmark m_acc)
  | Type (m_acc, m_typ) ->
      Format.fprintf fmt "type(%a, %a)" (format_access form_var)
        (Pos.unmark m_acc) format_value_typ (Pos.unmark m_typ)
  | SameVariable (m_acc0, m_acc1) ->
      Format.fprintf fmt "est_variable(%a, %a)" (format_access form_var)
        (Pos.unmark m_acc0) (format_access form_var) (Pos.unmark m_acc1)
  | InDomain (m_acc, cvm) ->
      Format.fprintf fmt "dans_domaine(%a, %a)" (format_access form_var)
        (Pos.unmark m_acc) (CatVar.Map.pp_keys ()) cvm
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

and format_access form_var fmt = function
  | VarAccess v_id -> format_varid form_var fmt v_id
  | TabAccess (v_id, m_i) ->
      Pp.fpr fmt "%a[%a]" (format_varid form_var) v_id
        (format_expression form_var)
        (Pos.unmark m_i)
  | FieldAccess (m_sp_opt, e, f, _) ->
      let sp_str =
        match m_sp_opt with
        | None -> ""
        | Some (m_sp, _) -> get_var_name (Pos.unmark m_sp) ^ "."
      in
      Pp.fpr fmt "%schamp_evenement(%a, %s)" sp_str
        (format_expression form_var)
        (Pos.unmark e) (Pos.unmark f)

and format_case form_var fmt = function
  | CDefault -> Format.pp_print_string fmt "default"
  | CValue v -> format_literal fmt v
  | CVar acc -> format_access form_var fmt (Pos.unmark acc)

and format_set_value form_var fmt sv =
  match sv with
  | FloatValue i -> Pp.fpr fmt "%f" (Pos.unmark i)
  | VarValue m_acc -> format_access form_var fmt (Pos.unmark m_acc)
  | IntervalValue (i1, i2) ->
      Pp.fpr fmt "%d..%d" (Pos.unmark i1) (Pos.unmark i2)

let format_print_arg form_var fmt = function
  | PrintString s -> Format.fprintf fmt "\"%s\"" s
  | PrintAccess (info, m_a) ->
      let infoStr = match info with Name -> "nom" | Alias -> "alias" in
      Format.fprintf fmt "%s(%a)" infoStr (format_access form_var)
        (Pos.unmark m_a)
  | PrintIndent e ->
      Format.fprintf fmt "indenter(%a)"
        (Pp.unmark (format_expression form_var))
        e
  | PrintExpr (e, min, max) ->
      if min = max_int then
        Format.fprintf fmt "(%a)" (Pp.unmark (format_expression form_var)) e
      else if max = max_int then
        Format.fprintf fmt "(%a):%d"
          (Pp.unmark (format_expression form_var))
          e min
      else
        Format.fprintf fmt "(%a):%d..%d"
          (Pp.unmark (format_expression form_var))
          e min max

let format_formula_decl form_var fmt = function
  | VarDecl (m_access, e) ->
      format_access form_var fmt (Pos.unmark m_access);
      Format.fprintf fmt " = %a" (format_expression form_var) (Pos.unmark e)
  | EventFieldRef (idx, f, _, v) ->
      Format.fprintf fmt "champ_evenement(%a,%s) reference %a"
        (format_expression form_var)
        (Pos.unmark idx) (Pos.unmark f) form_var v

let format_formula form_var fmt f =
  match f with
  | SingleFormula f -> format_formula_decl form_var fmt f
  | MultipleFormulaes (lvs, f) ->
      Format.fprintf fmt "pour %a\n%a"
        (format_loop_variables form_var)
        (Pos.unmark lvs)
        (format_formula_decl form_var)
        f

let rec format_instruction form_var form_err =
  let form_expr = format_expression form_var in
  let form_access = format_access form_var in
  let form_instrs = format_instructions form_var form_err in
  fun fmt instr ->
    match instr with
    | Affectation f -> Pp.unmark (format_formula form_var) fmt f
    | IfThenElse (cond, t, []) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]@\n" form_expr
          (Pos.unmark cond) form_instrs t
    | IfThenElse (cond, t, f) ->
        Format.fprintf fmt "if(%a):@\n@[<h 2>  %a@]else:@\n@[<h 2>  %a@]@\n"
          form_expr (Pos.unmark cond) form_instrs t form_instrs f
    | Switch (e, l) ->
        Format.fprintf fmt "aiguillage ";
        let () =
          match e with
          | SEValue e -> Format.fprintf fmt "(%a)" form_expr (Pos.unmark e)
          | SESameVariable v ->
              Format.fprintf fmt "nom (%a)" form_access (Pos.unmark v)
        in
        Format.fprintf fmt " : (@,";
        List.iter
          (fun (cl, l) ->
            List.iter (Format.fprintf fmt "%a :@," (format_case form_var)) cl;
            Format.fprintf fmt "@[<h 2>  %a@]" form_instrs l)
          l;
        Format.fprintf fmt "@]@,"
    | WhenDoElse (wdl, ed) ->
        let pp_wd th fmt (expr, dl, _) =
          Format.fprintf fmt "@[<v 2>%swhen (%a) do@\n%a@;@]" th form_expr
            (Pos.unmark expr) form_instrs dl
        in
        let pp_wdl fmt wdl =
          let rec aux th = function
            | wd :: l ->
                pp_wd th fmt wd;
                aux "then_" l
            | [] -> ()
          in
          aux "" wdl
        in
        let pp_ed fmt (Pos.Mark (dl, _)) =
          Format.fprintf fmt "@[<v 2>else_do@\n%a@;@]endwhen@;" form_instrs dl
        in
        Format.fprintf fmt "%a%a@\n" pp_wdl wdl pp_ed ed
    | VerifBlock vb ->
        Format.fprintf fmt
          "@[<v 2># debut verif block@\n%a@]@\n# fin verif block@\n" form_instrs
          vb
    | ComputeDomain (l, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "calculer domaine %a%a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l) pp_sp m_sp_opt
    | ComputeChaining (ch, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "calculer enchaineur %s%a;" (Pos.unmark ch) pp_sp
          m_sp_opt
    | ComputeVerifs (l, expr, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        Format.fprintf fmt "verifier %a%a : avec %a;"
          (Pp.list_space (Pp.unmark Pp.string))
          (Pos.unmark l) (Pp.unmark form_expr) expr pp_sp m_sp_opt
    | ComputeTarget (tname, targs, m_sp_opt) ->
        let pp_sp fmt m_sp_opt =
          match m_sp_opt with
          | None -> ()
          | Some (m_sp, _) ->
              Pp.fpr fmt " : espace %s" (get_var_name (Pos.unmark m_sp))
        in
        let pp_args fmt = function
          | [] -> ()
          | args ->
              let pp_m_access fmt m_a =
                format_access form_var fmt (Pos.unmark m_a)
              in
              Pp.list_comma pp_m_access fmt args
        in
        Format.fprintf fmt "calculer cible %s%a%a@," (Pos.unmark tname) pp_args
          targs pp_sp m_sp_opt
    | Print (std, args) ->
        let print_cmd =
          match std with StdOut -> "afficher" | StdErr -> "afficher_erreur"
        in
        Format.fprintf fmt "%s %a;" print_cmd
          (Pp.list_space (Pp.unmark (format_print_arg form_var)))
          args
    | Iterate (var, al, var_params, itb) ->
        let form_alist fmt = function
          | [] -> ()
          | al ->
              let form = Pp.list_comma @@ Pp.unmark form_access in
              Format.fprintf fmt "@;: %a" form al
        in
        let format_var_param fmt (vcs, expr, m_sp_opt) =
          let sp_str =
            match m_sp_opt with
            | None -> ""
            | Some (m_sp, _) -> " : espace " ^ get_var_name (Pos.unmark m_sp)
          in
          Format.fprintf fmt ": categorie %a : avec %a%s@\n"
            (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr) sp_str
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: %a@;: dans (" form_var
          var form_alist al
          (Pp.list_space format_var_param)
          var_params;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Iterate_values (var, var_intervals, itb) ->
        let format_var_intervals fmt (e0, e1, step) =
          Format.fprintf fmt ": entre %a .. %a increment %a@\n" form_expr
            (Pos.unmark e0) form_expr (Pos.unmark e1) form_expr
            (Pos.unmark step)
        in
        Format.fprintf fmt "iterate variable %a@;: %a@;: dans (" form_var var
          (Pp.list_space format_var_intervals)
          var_intervals;
        Format.fprintf fmt "@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | Restore (al, var_params, evts, evtfs, rb) ->
        let form_alist fmt = function
          | [] -> ()
          | al ->
              let form = Pp.list_comma @@ Pp.unmark form_access in
              Format.fprintf fmt "@;: variables %a" form al
        in
        let format_var_param fmt (var, vcs, expr, m_sp_opt) =
          let sp_str =
            match m_sp_opt with
            | None -> ""
            | Some (m_sp, _) -> " : espace " ^ get_var_name (Pos.unmark m_sp)
          in
          Format.fprintf fmt "@;: variable %a : categorie %a : avec %a%s"
            form_var var (CatVar.Map.pp_keys ()) vcs form_expr (Pos.unmark expr)
            sp_str
        in
        let format_var_params fmt = function
          | [] -> ()
          | var_params -> Pp.list "" format_var_param fmt var_params
        in
        let format_evts fmt = function
          | [] -> ()
          | evts ->
              Format.fprintf fmt "@;: evenements %a"
                (Pp.list_comma (Pp.unmark form_expr))
                evts
        in
        let format_evtfs fmt = function
          | [] -> ()
          | evtfs ->
              List.iter
                (fun (v, e) ->
                  Format.fprintf fmt "@;: evenement %a : avec %a" form_var v
                    (Pp.unmark form_expr) e)
                evtfs
        in
        Format.fprintf fmt "restaure%a%a%a%a@;: apres (" form_alist al
          format_var_params var_params format_evts evts format_evtfs evtfs;
        Format.fprintf fmt "@[<h 2>  %a@]@;)@;" form_instrs rb
    | ArrangeEvents (s, f, a, itb) ->
        Format.fprintf fmt "arrange_evenements@;:";
        (match s with
        | Some (v0, v1, e) ->
            Format.fprintf fmt "trier %a,%a : avec %a@;" form_var v0 form_var v1
              form_expr (Pos.unmark e)
        | None -> ());
        (match f with
        | Some (v, e) ->
            Format.fprintf fmt "filter %a : avec %a@;" form_var v form_expr
              (Pos.unmark e)
        | None -> ());
        (match a with
        | Some e -> Format.fprintf fmt "ajouter %a@;" form_expr (Pos.unmark e)
        | None -> ());
        Format.fprintf fmt ": dans (@[<h 2>  %a@]@\n)@\n" form_instrs itb
    | RaiseError (err, var_opt) ->
        Format.fprintf fmt "leve_erreur %a %s\n" form_err (Pos.unmark err)
          (match var_opt with Some var -> " " ^ Pos.unmark var | None -> "")
    | CleanErrors -> Format.fprintf fmt "nettoie_erreurs\n"
    | CleanFinalizedErrors -> Format.fprintf fmt "nettoie_erreurs_finalisees\n"
    | ExportErrors -> Format.fprintf fmt "exporte_erreurs\n"
    | FinalizeErrors -> Format.fprintf fmt "finalise_erreurs\n"
    | Stop (SKId None) -> Format.fprintf fmt "stop\n"
    | Stop (SKId (Some s)) -> Format.fprintf fmt "stop %s\n" s
    | Stop SKApplication -> Format.fprintf fmt "stop application\n"
    | Stop SKFun -> Format.fprintf fmt "stop fonction\n"
    | Stop SKTarget -> Format.fprintf fmt "stop cible\n"

and format_instructions form_var form_err fmt instrs =
  Pp.list "" (Pp.unmark (format_instruction form_var form_err)) fmt instrs
