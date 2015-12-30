open Tast
open Misc
open Context

(* La substitution triviale *)
let subst0 () = Smap.empty

(* tparam_type list -> typerType list -> substitution *)
let subst_from_lists tpts ts = 
  let rec aux m = function 
    | [], []          ->  m
    | t1::q1, t2::q2  ->  Smap.add (get_tpt_id t1) t2 (aux m (q1, q2)) 
    | _               ->
            failwith ("Les listes tptcs et ts n'ont pas la même longueur "^
            "dans subst_from_lists, ce n'est pas normal.")
  in aux Smap.empty (tpts, ts)


(* Le type Array[String]
 * loc -> tclasse *)
let array_tc l =
  let tptcs = [{
    tptc_cont = TPTCrien {
      tpt_cont = ("S", None);
      tpt_loc = l 
      };
    tptc_loc = l
    }] in {
  cc_name   = "Array";
  cc_tptcs  = tptcs;
  cc_params = [];
  cc_deriv  = None;
  cc_env = (add_classe_env (env0 ()) {
    cc_name   = "S";
    cc_tptcs  = [];
    cc_params = [];
    cc_deriv  = None;
    cc_env    = env0 ()
  })
}

(* Ajout d'une valeur à une substitution en vue d'une composition.
 * ident -> typerType -> substitution -> substitution *)
let rec add_one_key id t m = 
  let f id' = function 
    | Tclasse (cid, s) ->
        if id' = id then
          if cid = id then
            t
          else
            Tclasse (cid, s)
        else
          if id = cid then
            t
          else 
            Tclasse (cid, add_one_key id t s)
    | t' -> t'
  in let m' = Smap.mapi f m in
  if Smap.exists (fun i _ -> i = id) m' then
    m'
  else
    Smap.add id t m'

(* Composition des substitutions.
 * substitution -> substitution -> substitution *)
let subst_compose = Smap.fold add_one_key

(* fonction de substitution
 * substitution -> typerType -> typerType *)
let subst s = function
  | Tclasse (cid, s') ->
      begin try 
          Smap.find cid s
      with  
        | Not_found ->
            Tclasse (cid, subst_compose s s')
      end
  | t -> t 

(* Idem mais mais applicable à un id.
 * /!\ Doit servir uniquement à substituer un paramètre de type
 * subst_id -> context -> substitution -> ident -> typerType *)
let subst_id s id = subst s (Tclasse (id, subst0 ()))

(* Substitution de tous les types dans un accès.
 * substitution -> tacces -> tacces. *)
let rec subst_acc s a = match a.ta_cont with
  | TAident _ -> a
  | TAexpr_ident (e, i) ->
      {
        ta_cont = TAexpr_ident (subst_expr s e, i);
        ta_loc = a.ta_loc
      }

(* Substitution de tous les types qui apparaissent dans une expression...
 * Oui c'est violent, mais on n'a pas trouvé de meilleure solution.
 * substitution -> texpr -> texpr *)
and subst_expr s e =
  let cont = match e.te_cont with
    | TEvoid | TEthis | TEnull | TEint _ | TEstr _ | TEbool _ ->
        e.te_cont
    | TEacc a ->
        TEacc (subst_acc s a)
    | TEacc_exp (a, e') ->
        TEacc_exp (subst_acc s a, subst_expr s e')
    | TEacc_typ_exp (e', mid, targst, es) ->
        let e'' = subst_expr s e' in
        let targst' = {
          tat_cont = List.map (subst s) targst.tat_cont;
          tat_loc = targst.tat_loc;
        } in
        let es' = List.map (subst_expr s) es in
        TEacc_typ_exp (e'', mid, targst', es')
    | TEnew (i, targst, es) ->
        let targst' = {
          tat_cont = List.map (subst s) targst.tat_cont;
          tat_loc = targst.tat_loc;
        } in
        let es' = List.map (subst_expr s) es in
        TEnew (i, targst', es')
    | TEneg e' ->
        TEneg (subst_expr s e')
    | TEmoins e' ->
        TEmoins (subst_expr s e')
    | TEbinop (b, e1, e2) ->
        TEbinop (b, subst_expr s e1, subst_expr s e2)
    | TEifelse (eb, e1, e2) ->
        TEifelse (subst_expr s eb, subst_expr s e1, subst_expr s e2)
    | TEwhile (eb, e') ->
        TEwhile (subst_expr s eb, subst_expr s e')
    | TEreturn None ->
        e.te_cont
    | TEreturn (Some e') ->
        TEreturn (Some (subst_expr s e'))
    | TEprint e' ->
        TEprint (subst_expr s e')
    | TEbloc b ->
        TEbloc (subst_bloc s b)
  in
  {
    te_cont = cont;
    te_loc = e.te_loc;
    te_typ = subst s e.te_typ
  }

(* Substitue tous les types dans un bloc.
 * substitution -> tbloc -> tbloc *)
and subst_bloc s = function
  | [] -> []
  | (TIvar v) :: b ->
      let v' = subst_tvar s v in
      (TIvar v') :: (subst_bloc s b)
  | (TIexpr e) :: b ->
      (TIexpr (subst_expr s e)) :: (subst_bloc s b)

(* Substitue tous les types dans une variable.
 * substitution -> tvar -> tvar *)
and subst_tvar s v =
  let cont = begin match v.tv_cont with
    | TVal (i, t, e) ->
        TVal (i, subst s t, subst_expr s e)
    | TVar (i, t, e) ->
        TVar (i, subst s t, subst_expr s e)
  end in
  {
    tv_cont = cont;
    tv_typ = subst s v.tv_typ;
    tv_loc = v.tv_loc;
  }

(* Substitue tous les types dans un paramètre de type. 
 * substitution -> tparam_type -> tparam_type *)
let subst_tpt s tpt =
  let cont =
    let (i, h) = tpt.tpt_cont in
    let h' = match h with
      | None -> None
      | Some (HTinf t) -> Some (HTinf (subst s t))
      | Some (HTsup t) -> Some (HTsup (subst s t))
    in (i, h')
  in
  {
    tpt_cont = cont;
    tpt_loc = tpt.tpt_loc;
  }

(* Substitue tous les types dans une paramètre de type de classe.
 * substitution -> tparam_type_classe -> tparam_type_classe *)
let subst_tptc s tptc = 
  let cont = match tptc.tptc_cont with
    | TPTCplus tpt  -> TPTCplus (subst_tpt s tpt)
    | TPTCmoins tpt -> TPTCmoins (subst_tpt s tpt)
    | TPTCrien tpt  -> TPTCrien (subst_tpt s tpt)
  in {
    tptc_cont = cont;
    tptc_loc = tptc.tptc_loc;
  }

(* Substitue tous les types dans un paramètre.
 * substitution -> tparametre -> tparametre *)
let subst_param s p = {
  tp_name = p.tp_name;
  tp_typ = subst s p.tp_typ;
  tp_loc = p.tp_loc;
}

(* Idem que la fonction précédente mais pour une variable de contexte.
 * substitution -> context_var -> context_var *)
let subst_cvar s = function
  | CVar(i, t) -> CVar (i, subst s t)
  | CVal(i, t) -> CVal (i, subst s t)

(* Adapte une variable héritée à la classe fille.
 * substitution -> context_var -> context_var *)
let update_var = subst_cvar

(* Adapte une contrainte d'un environnement de méthode héritée à la classe
 * fille.
 * substitution -> constr -> constr *)
let update_constr s (i, t) = (i, subst s t)

(* Adapte une classe d'un environnement de classe héritée à la classe fille.
 * substitution -> context_classe -> context_classe *)
let rec update_classe s c = {
  cc_name = c.cc_name;
  cc_tptcs = List.map (subst_tptc s) c.cc_tptcs;
  cc_params = List.map (subst_param s) c.cc_params;
  cc_deriv =
    begin match c.cc_deriv with
      | None -> None
      | Some (t, es) -> 
          Some (subst s t, List.map (subst_expr s) es)
    end;
  cc_env = update_env s c.cc_env;
}

(* Adapte l'environnement d'une méthode héritée en vue de l'ajout à une classe
 * fille.
 * substitution -> context -> context *)
and update_env s env = {
  classes = List.map (update_classe s) env.classes;
  constrs = List.map (update_constr s) env.constrs; 
  vars    = List.map (update_var s) env.vars;
  meths   = List.map (update_meth s) env.meths;
}

(* Adapte les méthodes héritées pour les ajouter à l'environnement d'une classe.
 * substitution -> tmethode -> tmethode  *)
and update_meth s m = 
  let subst_param p = {
    tp_name = p.tp_name;
    tp_typ = subst s p.tp_typ;
    tp_loc = p.tp_loc;
  } in
  let params = List.map subst_param m.tm_params in
  let res_type = subst s m.tm_res_type in
  let res_expr = subst_expr s m.tm_res_expr in
  let env = update_env s m.tm_env in
  {
    tm_name         = m.tm_name;
    tm_override     = m.tm_override;
    tm_type_params  = m.tm_type_params;
    tm_params       = params;
    tm_res_type     = res_type;
    tm_res_expr     = res_expr;
    tm_loc          = m.tm_loc;
    tm_env          = env;
  }

