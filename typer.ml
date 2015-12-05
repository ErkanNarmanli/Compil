open Ast
open Tast

exception TypeError of loc * string

(* Fonctions utiles *)

let rec iter3 f l1 l2 l3 = match (l1, l2, l3) with
  | ([], [], []) -> ()
  | (t1::q1, t2::q2, t3::q3) ->
      f t1 t2 t3;
      iter3 f q1 q2 q3
  | _ -> raise (Invalid_argument "Tailles incompatibles dans iter3") 

(* Ajoute une variable à un environnement *)
(* env -> tvar -> env *)
let add_tvar_env env tv = 
    {
        classes = env.classes ;
        constrs = env.constrs ;
        vars    = tv::env.vars;
        meths   = env.meths 
    }

(* Ajoute une classe a un environnement
 * context -> tclasse -> context *)
let add_classe_env env c = 
  {
    classes = c::env.classes;
    constrs = env.constrs ; 
    vars    = env.vars;
    meths   = env.meths
  }

(* Ajoute une contrainte a un environnement
 * context -> (ident * typerTyper) -> context *)
let add_constr_env env constr = 
  {
    classes = env.classes;
    constrs = constr::env.constrs;
    vars    = env.vars;
    meths   = env.meths
  }

(* Affichage d'un type *)
let rec string_of_typ = function
  | Tany ->         "Any"
  | TanyVal ->      "AnyVal"
  | Tboolean ->     "Boolean"
  | Tint ->         "Int"
  | Tunit ->        "Unit"
  | TanyRef ->      "AnyRef"
  | Tstring ->      "String"
  | Tnull ->        "Null"
  | Tnothing ->     "Nothing"
  | Tclasse (c, targst) ->
                    string_of_class c.tc_name targst

(* Affichage d'une classe *)
and string_of_class i targst =
  i^"["^(
  List.fold_left (fun s t -> s^(string_of_typ t)^", ") "" targst.tat_cont
  )^"]"

(* chercher une classe dans l'environnement env *)
let classe_lookup env id = 
  let rec aux = function
    | []      -> raise Not_found
    | c::q    -> if c.tc_name = id then
                   c
                 else
                   aux q
  in aux env.classes

(* fonction de substitution
 * subst : contex -> tclasse -> targuments_type -> typerType -> typerType *)
let subst env c targst t = 
  let assoc = (List.combine (get_tptc_id_list c.ttype_class_params) 
                            targst.tat_cont)
  in
  let rec aux = function
    | Tclasse (c', targst') ->
        begin try 
            List.assoc c'.tc_name assoc
        with 
          | Not_found ->
            begin try
                let _ = classe_lookup env c'.tc_name in
                Tclasse (c', {
                              tat_cont = List.map aux targst'.tat_cont ; 
                              tat_loc  = targst'.tat_loc
                              })              
            with
              | Not_found -> raise (TypeError (c'.tc_loc, "Classe inconnue :
                  \""^c'.tc_name^"\"."))
            end
        end
    | t -> t 
  in aux t

(* typ -> typerType *)
let rec typerType_of_typ env t = match t.t_name with
    | "Any"     -> Tany
    | "AnyVal"  -> TanyVal
    | "Boolean" -> Tboolean
    | "Int"     -> Tint
    | "Unit"    -> Tunit
    | "AnyRef"  -> TanyRef
    | "String"  -> Tstring
    | "Null"    -> Tnull
    | "Nothing" -> Tnothing
    | s         -> let c = classe_lookup env s in
                   let args = begin match t.args_type.at_cont with
                     | None -> []
                     | Some l -> l end in
                   Tclasse (c, {
                       tat_cont = List.map (typerType_of_typ env) args;
                       tat_loc = t.t_loc
                   })
        
(* arguments_type -> targuments_type *)
let targst_of_argst env a =
  let l = match a.at_cont with
    | None -> []
    | Some l' -> List.map (typerType_of_typ env) l'
  in { tat_cont = l; tat_loc = a.at_loc }

(* Recherche de la déclaration d'une variable dans une var list *)
(* ident -> tvar list -> typerType *)
let rec var_lookup id = function
    (* TODO pas beau : mieux = appliquer à env et non env.vars *)
  | []    ->  raise Not_found
  | v::q  ->  if id = get_var_id v then
                v.tv_typ
              else
                var_lookup id q

(* ident -> tclasse -> tmethode *)
let meth_lookup m_id c =
  let rec aux = function 
    | []   -> raise Not_found
    | m::q ->
        if (get_meth_id m) = m_id then
          m
        else
          aux q
  in aux c.tc_env.meths


let rec decl_lookup id = function
  | []    ->  raise Not_found
  | d::q  ->  begin match d with
                | TDvar v -> if id = get_var_id v then
                               d
                             else
                               decl_lookup id q
                | TDmeth m -> decl_lookup id q
              end

(* Parcourt une liste de déclarations à la recherche du champ x 
 * Renvoie le type de x *)
and decl_type_lookup x = function
  | []    ->  raise Not_found
  | d::q  ->  begin match d with
                  | TDvar v ->  if x = get_var_id v then
                                  v.tv_typ
                                else
                                  decl_type_lookup x q
                  | TDmeth m -> raise (TypeError (m.tm_loc, "Il est interdit
                                d'appeler une méthode de cette façon. Il manque
                                les arguments"))
              end
(* Fonction auxiliaire qui vérifie qu'une
 * classe hérite d'une autre (indirectement) *)
let herits_from c1 c2 = 
  let rec herits_from_c2 c = 
    if c.tc_name = c2.tc_name then
      true
    else begin match c.tderiv with
      | Some (t, _) -> begin match t with
            | Tclasse (c', targst') -> herits_from_c2 c'
            | _ -> false
          end 
      | None -> false
    end in herits_from_c2 c1
    
(* Sous-typage *)
let rec is_sstype env t1 t2 = match (t1, t2) with
  | Tnothing, _             -> true
  | Tnull,  Tclasse (_, _)  -> true
  | Tnull, Tstring          -> true 
  | _, TanyVal              -> t1 = Tint || t1 = Tunit || t1 = Tboolean 
  | _, Tany                 -> true
  | _, TanyRef              -> begin match t1 with
                                 | Tclasse (_, _)   -> true
                                 | Tstring | Tnull  -> true
                                 | _                -> false
                               end
  | Tclasse (c1, targst1), Tclasse (c2, targst2) ->
      (* Cas où c1 = c2, on doit vérifier les contraintes sur les paramètres de
       * type *)
      if (c1.tc_name = c2.tc_name) then begin
        let welltyped = ref true in
        (* typerType list -> typerType list -> tparam_type_classeCont list -> () *)
        let f t1 t2 = function 
          | TPTCplus  tpt -> welltyped := !welltyped && (is_sstype env t1 t2)
          | TPTCrien  tpt -> welltyped := !welltyped && (t1 = t2)
          | TPTCmoins tpt -> welltyped := !welltyped && (is_sstype env t2 t1)
        in
        let ts1   = targst1.tat_cont in
        let ts2   = targst2.tat_cont in
        let tptcs = List.map (fun tptc -> tptc.tptc_cont) c1.ttype_class_params
        in
        iter3 f ts1 ts2 tptcs;
        !welltyped
      (* Cas où c1 <> c2.
       * On distingue selon si c1 hérite de c2 ou non. *)
      end else
        if herits_from c1 c2 then
          let (c, targst) = begin match c1.tderiv with
            | Some (Tclasse (c, targst), _) -> (c, targst)
            | _ -> failwith "On ne peut que trouver une classe ici : herits_from
                      vient de renvoyer true."
          end in
          let targst' = {
            tat_cont = List.map (subst env c1 targst1) targst.tat_cont;
            tat_loc = targst.tat_loc
          } in
          is_sstype env (Tclasse (c, targst')) t2
        else
          is_sstype env t1 (List.assoc c2.tc_name env.constrs)
  | t1, t2 -> t1 = t2

let max_type env t1 t2 eloc = 
    if is_sstype env t1 t2 then t2
        else if is_sstype env t2 t1 then t1
            else raise (TypeError (eloc, "Les deux types dans cette expression
            ne sont pas comparables."))

(* Respect d'une borne *)
let check_borne env t (bo, eloc) = match bo with
  | None    -> None
  | Some b  ->
      if (match b with
        | HTinf t' -> is_sstype env t' t
        | HTsup t' -> is_sstype env t t'
      ) then None
      else (Some eloc)


(* Respect des bornes. 
 *  context -> typerType list ->
 *   tparam_type_heritage option list (i.e. liste de bornes) -> loc option
 * Rend None en l'absence d'erreur sur les bornes et (Some errloc) sinonù
 * errloc est la localisation de la première erreur trouvée *)
let check_borne_list env ts bs = 
  List.fold_left2
      (fun eloc_o t blo -> begin match eloc_o with
        | Some eloc' -> Some eloc'
        | None -> check_borne env t blo
      end) None ts bs
  

(* Types bien formés *)
let rec is_bf env = function
  | Tclasse (c, targst)  ->
      begin try 
        let _ = classe_lookup env c.tc_name in
        begin match (check_borne_list env targst.tat_cont (get_bornes_list_c c)) with
          | None   -> List.for_all (is_bf env) targst.tat_cont
          | Some _ -> false
        end
      with
        | Not_found -> raise (TypeError (c.tc_loc, "Cette instance de classe
                       n'est pas bien formee."))
      end
  | _ -> true


(* Typage à proprement parler *)

(* Typage des expressions *)
let rec type_expr env tro e = match e.e_cont with
  | Evoid         -> { te_cont = TEvoid ;
                       te_loc = e.e_loc ;
                       te_typ = Tunit }
  | Eint i        -> { te_cont = TEint i;
                       te_loc = e.e_loc ;
                       te_typ = Tint }
  | Estr s        -> { te_cont = TEstr s ;
                       te_loc = e.e_loc ;
                       te_typ = Tstring }
  | Ebool b       -> { te_cont = TEbool b;
                       te_loc = e.e_loc ;
                       te_typ = Tboolean }
  | Enull         -> { te_cont = TEnull ;
                       te_loc = e.e_loc ;
                       te_typ = Tnull }
  | Ethis         ->  begin try {
                        te_cont = TEthis;
                        te_loc = e.e_loc;
                        te_typ = (var_lookup "this" env.vars) }
                      with
                        | Not_found ->  raise (TypeError (e.e_loc, "impossible
                          de déterminer à quoi \"this\" fait référence"))
                      end
  | Eacc a      -> begin match a.a_cont with
                     | Aident id -> 
                         begin try {
                           te_cont = TEacc (tacces_of_acces a);
                           te_loc = e.e_loc;
                           te_typ = var_lookup id env.vars }
                         with
                           | Not_found ->
                                   type_expr env tro {
                                     e_cont = Eacc {
                                       a_cont = Aexpr_ident ({
                                         e_cont = Ethis;
                                         e_loc = a.a_loc
                                        }, id);
                                       a_loc = a.a_loc
                                     };
                                     e_loc = e.e_loc
                                   }
                         end
                     | Aexpr_ident (e',x) ->
                         let e'' = type_expr env tro e' in
                         (* Il faut que e'' soit une instance d'une classe *)
                         begin match e''.te_typ with
                           | Tclasse (c, targst) ->
                               (* x doit faire référence à une variable et non
                                * une méthode, decl_type_lookup soulève une
                                * erreur sinon *)
                               let t = begin try 
                                 var_lookup x c.tc_env.vars
                               with
                                 | Not_found ->  raise (TypeError
                                     (e'.e_loc, "La classe de cette expression
                                     n'a pas de champ \""^x^"\"."))
                               end in 
                               {
                                 te_cont = TEacc {
                                     ta_cont = TAexpr_ident (e'', x);
                                     ta_loc = a.a_loc
                                   };
                                 te_loc = e.e_loc;
                                 te_typ = subst env c targst t
                               }
                           | _ ->  raise (TypeError (e.e_loc, "Cette
                               expression n'est pas une instance d'une classe,
                               elle ne peut avoir de champ \""^x^"\"."))
                         end
                   end
  | Eacc_exp (a,e') -> begin match a.a_cont with
                         | Aident i -> (* cas où a est juste un nom de
                                        * variable *)
                             begin try
                               let t1 = var_lookup i env.vars in
                               (* on a bien trouvé la variable identifiée
                                * par i *)
                               let e'' = type_expr env tro e' in
                               if is_sstype env t1 e''.te_typ then
                                 (* t1 est bien un sous type de t2 *)
                                 {
                                   te_cont = TEacc_exp
                                     ((tacces_of_acces a), e'');
                                   te_loc = e.e_loc;
                                   te_typ = Tunit
                                 }
                               else (* t1 n'est pas un sous type de t2 *)
                                 raise (TypeError (e.e_loc, "le type de
                                      \""^i^"\" n'est pas compatible avec celui
                                      de l'expression qu'on lui affecte")) 
                             with
                               | Not_found -> (* On a pas trouvé la
                                   variable i dans l'environnement, on
                                   cherche this.i *)
                                   type_expr env tro {
                                     e_cont = Eacc_exp ({
                                         a_cont = Aexpr_ident({
                                           e_cont = Ethis;
                                           e_loc = a.a_loc
                                         }, i);
                                         a_loc = a.a_loc
                                       }, e');
                                     e_loc = e'.e_loc        
                                   }
                             end
                         | Aexpr_ident (e'', i) -> (* On type récursivement
                                                    * e''.i dans e''.i = e' *)
                             let e1 = type_expr env tro {
                                 e_cont = Eacc {
                                     a_cont = Aexpr_ident (e'',i);
                                     a_loc = a.a_loc
                                   };
                                 e_loc = a.a_loc
                             } in
                             let e2 = type_expr env tro e' in
                             if is_sstype env e2.te_typ e1.te_typ then
                               let a' = begin match e1.te_cont with
                                 | TEacc a'' -> a''
                                 | _         -> failwith "Comment on a fait
                                                pour en arriver là ?"
                               end in {
                                   te_cont = TEacc_exp (a', e2);
                                   te_loc = e.e_loc;
                                   te_typ = Tunit
                                 }
                             else
                               raise (TypeError (e.e_loc,"Le type de
                               la variable \""^i^"\" est incompatible avec le
                               type de l'expression qu'on lui affecte"))
                       end
  (* -e' est un entier si e' est un entier, erreur sinon *)
  | Emoins e' ->  let e'' = type_expr env tro e' in
                  begin match e''.te_typ with
                    | Tint -> {
                          te_cont = TEmoins e'';
                          te_loc = e'.e_loc;
                          te_typ = Tint
                        } 
                    | _ ->  raise (TypeError (e'.e_loc, "Cette
                            expression n'est pas entière, on ne peut pas
                            prendre son opposé"))
                  end
  (* !e' est un booléen si e' est un booléen, erreur sinon *)
  | Eneg e' ->  let e'' = type_expr env tro e' in
                begin match e''.te_typ with
                  | Tboolean -> {
                        te_cont = TEneg e'';
                        te_loc = e'.e_loc;
                        te_typ = Tboolean
                      }
                  | _ ->  raise (TypeError (e'.e_loc, "Cette
                          expression n'est pas booléenne, on ne peut pas la
                          nier"))
                end
  | Ebinop (b, e1, e2) -> 
                (* On type d'abord les deux opérandes de l'opération binaire *)
                let e1' = type_expr env tro e1 in
                let e2' = type_expr env tro e2 in
                begin match b.b_cont with
                  (* On peut comparer les classes et les chaînes de caractères
                   * avec ne et eq (chez nous : NeRef et EqRef)
                   * Le résultat est un booléen *)
                  | NeRef | EqRef -> 
                      if (is_sstype env e1'.te_typ TanyRef) &&
                         (is_sstype env e2'.te_typ TanyRef) then {
                              te_cont = TEbinop ((tbinop_of_binop b),e1',e2');
                              te_loc = e.e_loc;
                              te_typ = Tboolean
                            }
                      else raise (TypeError (b.b_loc, "Cette
                        opération n'est permise que sur les
                        classes héritant de AnyRef"))
                  (* On peut comparer les entiers avec ==, =!, <, >, <=, >=
                   * Le résultat est un booléen *)
                  | Eq | Ne | Lt | Le | Gt | Ge ->
                      if (e1'.te_typ = Tint) && (e2'.te_typ =  Tint) then {
                          te_cont = TEbinop ((tbinop_of_binop b), e1', e2');
                          te_loc = e.e_loc;
                          te_typ = Tboolean
                        }
                      else raise (TypeError (b.b_loc, "Impossible de comparer
                           des expressions non entières"))
                  (* Le calcul booléen avec && et || *)
                  | And | Or ->
                      if (e1'.te_typ = Tboolean) && (e2'.te_typ = Tboolean)
                      then {
                          te_cont = TEbinop ((tbinop_of_binop b), e1', e2');
                          te_loc = e.e_loc;
                          te_typ = Tboolean
                        }
                      else raise (TypeError (b.b_loc, "Impossible d'effectuer
                           une opération booléenne sur des expressions non
                           booléennes"))
                  (* Les opérations arithmétiques avec +, -, *, /, % *)
                  | Add | Sub | Mul | Div | Mod ->
                      if (e1'.te_typ = Tint) && (e2'.te_typ = Tint) then {
                          te_cont = TEbinop ((tbinop_of_binop b), e1', e2');
                          te_loc = e.e_loc;
                          te_typ = Tint
                        }
                      else raise (TypeError (b.b_loc, "Une des deux
                           opérandes n'est pas un entier"))
                end
  | Eprint e' ->let e'' = type_expr env tro e' in
                (* On type d'abord e' et on autorise le print si c'est un entier
                 * ou un chaîne de caractères
                 * Cette opération est de type unit *)
                begin match e''.te_typ with
                  | Tint -> {
                        te_cont = TEprint e'';
                        te_typ = Tunit;
                        te_loc = e.e_loc
                      }
                  | Tstring -> {
                        te_cont = TEprint e'';
                        te_typ = Tunit;
                        te_loc = e.e_loc
                      }
                  | _ -> raise (TypeError (e'.e_loc, "Cette
                         expression n'est pas imprimable"))
                end
  (* Structure if (eb) e1 else e2 *)
  | Eifelse (eb, e1, e2) ->
                (* On type eb, e1, e2 *)
                let eb' = type_expr env tro eb in
                let e1' = type_expr env tro e1 in
                let e2' = type_expr env tro e2 in
                (* eb' doit être un booléen
                 * Les types de e1' et e2' doivent être comparables *)
                if (eb'.te_typ = Tboolean) &&
                   ((is_sstype env e1'.te_typ e2'.te_typ) ||
                   (is_sstype env e2'.te_typ e2'.te_typ))
                then {
                      te_cont = TEifelse (eb', e1', e2');
                      te_typ = max_type env e1'.te_typ e2'.te_typ e.e_loc;
                      te_loc = e.e_loc
                    }
                else raise (TypeError (e.e_loc, "Le type de retour est mal 
                     défini : les cas if et else ont des types
                     incompatibles"))
  (* Le sucre syntaxique : 
   * On se ramène au cas précédent avec un () pour le deuxième argument
   * (Evoid chez nous) *)
  | Eif(eb, e1) ->
                type_expr env tro {
                  e_cont = Eifelse (eb,e1,{e_cont = Evoid; e_loc = e.e_loc});
                  e_loc = e.e_loc
                }
  (* Boucle while (eb) e1 *)
  | Ewhile (eb, e1) ->    
                (* il suffit juste que eb soit booléenne et que e1 soit bien
                 * typée *)
                let eb' = type_expr env tro eb in
                if eb'.te_typ = Tboolean then
                  let e1' = type_expr env tro e1 in {
                    te_cont = TEwhile (eb', e1');
                    te_typ = Tunit;
                    te_loc = e.e_loc
                  }
                else raise (TypeError (eb.e_loc, "Cette expression n'est pas
                     booléenne"))
  | Enew (i, argst , es) ->
                (* Utile pour la suite *)
                let targst = targst_of_argst env argst in
                (* On récupère d'abord la classe concernée dans l'env *)
                let c = try classe_lookup env i with
                  | Not_found -> raise (TypeError (e.e_loc, "L'identificateur
                  \""^i^"\" ne fait référene à aucune classse connue."))
                (* Vérifie que le type C[sigma] est bien formé 
                 * Il suffit de connaître la classe et les arguments type pour
                 * cela *)
                in if (is_bf env (Tclasse (c, targst))) then begin
                  let es' = List.map (type_expr env tro) es in
                  (* On verifie que le sous typage est bon.
                   * On garde la localisation de l'erreur lorsque ce n'est pas
                   * le cas.
                   * On vérifie au passage que l'utilsateur a fourni le bon
                   * nombre d'arguments : fold_left2 soulève une erreur si ce
                   * n'ets pas le cas.
                   * D'où une fonction de test un peu longue... *)
                  let (welltyped, errloc_o) = begin try (List.fold_left2 (fun (b,o) e' p -> 
                        begin match o with
                          | None -> let b' = is_sstype env e'.te_typ (subst env c targst
                                    p.tp_typ) in
                                    (b && b', if b' then None else Some
                                    e'.te_loc)
                          | Some errloc -> (false, Some errloc)  
                        end) (true, None) es' c.tparams)
                  with
                    | Invalid_argument _ -> raise (TypeError (e.e_loc, "Ce
                                            constructeur de classe n'est pas
                                            appelé avec le bon nombre
                                            d'arguments"))
                  end in
                  if welltyped then (* On peut enfin typer le new *)
                    {
                      te_cont = TEnew (i, targst, es');
                      te_typ = Tclasse (c, targst);
                      te_loc = e.e_loc
                    }
                  else begin
                    match errloc_o with
                      | None ->        failwith "On ne peut pas recevoir None ici"
                      | Some errloc -> raise (TypeError (errloc, "Le type de
                                       cette expression est incompatible avec
                                       la classe"))
                  end
                end 
                else
                  raise (TypeError (e.e_loc, "Le type \""^(string_of_class i
                  targst)^"\" n'est pas bien formé"))
  | Eacc_typ_exp (a, argst, es) ->
                begin match a.a_cont with
                  | Aident m ->
                      (* Le sucre syntaxique : m tout seul signifie this.x
                       * On s'en sort avec un appel récursif sur l'autre cas *) 
                      type_expr env tro {
                        e_cont = Eacc_typ_exp ({
                          a_cont = Aexpr_ident ({
                            e_cont = Ethis;
                            e_loc = a.a_loc
                          }, m);
                          a_loc = a.a_loc
                          }, argst, es);
                        e_loc = e.e_loc
                      }
                  | Aexpr_ident (e'', m_id) ->
                      (* On commence par typer l'expression qui appelle la
                       * méthode et on vérifie que c'est une instance de classe *) 
                      let te'' = type_expr env tro e'' in
                      begin match te''.te_typ with
                        | Tclasse(c, targst) ->
                            (* Va chercher la méthode m dans la classe de c et
                             * on type ses arguments *)
                            let es' = List.map (type_expr env tro) es in
                            let m   = begin try meth_lookup m_id c with
                              | Not_found -> raise (TypeError (a.a_loc, "Cette
                                             expression ne fait référence à
                                             aucune méthode connue."))
                            end in
                            (* On calcule les types dans argst et on stocke
                             * leur localisation au passage *)
                            let loctyps = List.map
                                    (fun t -> (typerType_of_typ env t, t.t_loc))
                                    (match argst.at_cont with
                                       | None -> []
                                       | Some a -> a
                                    ) in
                            (* On vérifie que types calculés dans loctyps sont bien
                             * formés *)
                            begin match (List.fold_left (fun errloc_o (t, tloc) ->
                                         begin match errloc_o with
                                           | None -> if is_bf env t then
                                                       None
                                                     else
                                                       Some tloc
                                           | Some errloc as o -> o
                                         end ) None loctyps) with 
                              | None ->
                              (* On extrait la liste des types des arguments de
                               *  la méthode *)
                                  let tau's = List.map (fun p -> p.tp_typ)
                                  (get_meth_params m) in
                              (* On sépare les listes des types et des
                               * positions *)
                                  let (taus, tlocs) = List.split loctyps in  
                              (* On calcule la liste des idents des Tj à
                               * substituer par les taus et on zippe les listes
                               * ensemble en une liste d'association *)
                                  let lassoc = begin try
                                      List.combine 
                                        (get_meth_type_params_id_list m)
                                        taus
                                  with
                                    | Invalid_argument _ -> raise (TypeError
                                    (m.tm_loc, "Cette méthode n'a pas reçu le
                                    bon nombre de paramètres de type")) end in
                              (* On applique la seconde substitution : c'est
                               * moins immédiat car on ne peut pas la
                               * représenter par un tclasse * targuments_type *)
                                  let rec subst' = function
                                    | Tclasse (c', targst') ->
                                        (* Si c' est un une variable de type Tj,
                                         * on la substite par le tau_j
                                         * correspondant, sinon on descend
                                         * dans ses paramètres de type *)
                                        begin try (List.assoc c'.tc_name
                                        lassoc)
                                        with   
                                          | Not_found ->
                                              Tclasse (c', {
                                                tat_cont = List.map subst'
                                                  targst'.tat_cont;
                                                tat_loc = targst'.tat_loc})
                                        end
                                    | _ as t -> t 
                                  in
                                  let tau's' = List.map subst' tau's in
                                  (* On vérifie les bornes *)
                                  begin match check_borne_list env tau's' (get_bornes_list_m
                                  m) with
                                    | None -> ()
                                    | Some errloc -> raise (TypeError (errloc,
                                    "La borne de type n'est pas respectée"))
                                  end;
                              (* On applique la première substitution *)
                                  let tau''s = List.map (subst env c targst)
                                  tau's'
                                  in
                              (* On vérifie que les bornes sont respectées *)
                                  begin match check_borne_list env tau''s (get_bornes_list_c
                                  c) with
                                    | None -> ()
                                    | Some errloc -> raise (TypeError (errloc,
                                    "La borne de type n'est pas respectée"))
                                  end;
                                  begin try 
                                    iter3 (fun t1 t2 eloc ->
                                        if is_sstype env t1 t2 then
                                            ()
                                        else 
                                            raise (TypeError (eloc, "Les types
                                            sont incompatibles")))
                                        (List.map (fun exp -> exp.te_typ) es')
                                        tau''s tlocs
                                  with
                                    | Invalid_argument _ -> raise (TypeError
                                    (e.e_loc, "Cette méthode n'a pas reçu le bon
                                    nombre d'arguments."))
                                  end;
                                  (* On applique la première substitution au
                                   * type de retour puis on vérifie les
                                   * bornes. *)
                                  {
                                    te_cont = TEacc_typ_exp ({
                                      ta_cont = TAexpr_ident(te'', m_id);
                                      ta_loc  = a.a_loc
                                    }, targst_of_argst env argst, es');
                                    te_typ = (subst env c targst) (
                                      subst' (get_meth_type m)); 
                                    te_loc = e.e_loc
                                  }
                              | Some errloc -> raise (TypeError (errloc, "Ce
                                               type n'est pas bien formé."))
                            end
                        | _ -> raise (TypeError (a.a_loc, "Cette expression
                               n'est pas une instance d'une classe, elle ne
                               peut pas avoir de méthode"))
                      end

                      
                end
  | Ereturn None -> if is_sstype env Tunit (match tro with | None -> failwith "On a oublié
                    de mettre le type de retour de la méthode qu'on type dans
                    tro" | Some tr -> tr) then
                      {
                        te_cont = TEreturn None;
                        te_typ = Tnothing;
                        te_loc = e.e_loc
                      }
                    else
                      raise (TypeError (e.e_loc, "Soit il manque un argument à
                      return soit le type de retour de cette méthode est mal
                      spécifié"))
  | Ereturn (Some e') -> let tr = begin match tro with | None -> failwith "On a
                         oublié de préciser le type de retour de la méthode
                         qu'on type" | Some tr -> tr end in
                         let e'' = type_expr env tro e' in
                         if is_sstype env e''.te_typ tr then
                           {
                             te_cont = TEreturn (Some e'');
                             te_typ = Tnothing;
                             te_loc = e.e_loc
                           }
                         else
                             raise (TypeError (e.e_loc, "Le type de la valeur
                             renvoyée n'est pas compatible avec le type de
                             retour de la méthode "))
  | Ebloc b ->  match b.bl_cont with
                | []    -> {
                             te_cont = TEbloc [];
                             te_typ = Tunit;
                             te_loc = e.e_loc
                           }
                | [Iexpr e']  -> let e'' = type_expr env tro e' in
                           {
                             te_cont = TEbloc [ TIexpr e'' ];
                             te_typ  = e''.te_typ;
                             te_loc = e.e_loc
                           }
                | ins::q -> 
                            let q' =  { e_cont = Ebloc { 
                                        bl_cont  = q ;
                                        bl_loc = b.bl_loc
                                        };
                                        e_loc = e.e_loc
                                    } in
                            begin match ins with
                              | Ivar v      -> (* Là on distingue en fonction de
                              si l'utilisateur a spécifié un type ou non. On
                              garde aussi un booléen idiquant si la variable est
                              un val ou un var. Ça nous évite d'écrire deux fois
                              le même code par la suite.*)
                                let (i, t_o, ev, isval) =  begin match v.v_cont with
                                  | Val (i', t_o', ev') -> (i', t_o', ev', true)
                                  | Var (i', t_o', ev') -> (i', t_o', ev', false)
                                end in
                                (* on type ev *)
                                let ev' = type_expr env tro ev in
                                let (env', tv) = begin match t_o with 
                                  | None   -> 
                                      (* on crée l'environnement étendu *)
                                      let tv = 
                                          {
                                            tv_cont = (if isval then 
                                                TVal (i, ev'.te_typ, ev')
                                            else
                                                TVar (i, ev'.te_typ, ev')
                                            );
                                            tv_typ = ev'.te_typ;
                                            tv_loc = v.v_loc
                                          } in
                                      (add_tvar_env env tv, tv)
                                  | Some t -> let t' = typerType_of_typ env t in
                                      if is_bf env t' then
                                        ()
                                      else
                                        raise (TypeError (t.t_loc, "Ce type
                                        n'est pas bien formé."));
                                      if is_sstype env ev'.te_typ t' then
                                        ()
                                      else
                                        raise (TypeError (t.t_loc, "Le type
                                        spécifié est incompatible avec
                                        l'expression qui suit."));
                                      let tv = 
                                          {
                                            tv_cont = (if isval then 
                                                TVal (i, t', ev')
                                            else
                                                TVar (i, t', ev')
                                            );
                                            tv_typ = ev'.te_typ;
                                            tv_loc = v.v_loc
                                          } in
                                      (add_tvar_env env tv, tv) 
                                end in
                                let eb = type_expr env' tro q' in
                                let b' = begin match eb.te_cont with
                                  | TEbloc bl -> bl 
                                  | _         -> failwith "cette variable ne pas
                                                être autre chose qu'un
                                                bloc."
                                end in      
                                {
                                  te_cont = TEbloc ( (TIvar tv) :: b');
                                  te_typ = ev'.te_typ;
                                  te_loc = e.e_loc 
                                }
                              | Iexpr  e'    -> 
                                let eb = type_expr env tro q' in
                                let b' = begin match eb.te_cont with
                                  | TEbloc bl -> bl 
                                  | _         -> failwith "cette variable ne pas
                                                 être autre chose qu'un bloc"
                                end in      
                                let e'' = type_expr env tro e' in
                                    {
                                        te_cont = (TEbloc ((TIexpr e'') :: b'));
                                        te_typ  = eb.te_typ ;
                                        te_loc  = e.e_loc
                                    }
                            end

(* context -> param_type_classe -> tparam_type_heritage option -> context *)
let ptc_add env ptc bo = match bo with
  | None -> add_classe_env env {
              tc_name = get_ptc_id ptc;
              ttype_class_params = [];
              tparams = [];
              tderiv = None;
              tdecls = [];
              tc_loc = ptc.ptc_loc;
              tc_env = env (* c'est bien ca ? *)
      }
  | Some (Hsup tau) ->
      let tau' = typerType_of_typ env tau in 
      if is_bf env tau' then
        add_classe_env env {
              tc_name = get_ptc_id ptc;
              ttype_class_params = [];
              tparams = [];
              tderiv = Some (tau', []); (* en est-on bien sur ... *)
              tdecls = [];
              tc_loc = ptc.ptc_loc;
              tc_env = env (* c'est bien ca ? *)
        }
      else
        raise (TypeError (ptc.ptc_loc, "La borne de ce parametre de type n'est
              pas bien formee"))
  | Some (Hinf tau) ->
      let tau' = typerType_of_typ env tau in 
      if is_bf env tau' then
        add_constr_env (add_classe_env env {
              tc_name = get_ptc_id ptc;
              ttype_class_params = [];
              tparams = [];
              tderiv = None; (* en est-on bien sur ... *)
              tdecls = [];
              tc_loc = ptc.ptc_loc;
              tc_env = env (* c'est bien ca ? *)
        }) (get_ptc_id ptc, tau')
      else
        raise (TypeError (ptc.ptc_loc, "La borne de ce parametre de type n'est
              pas bien formee"))
      
              
let type_classe env c =
  (* On declare des environnements mutables pour ne pas se perde *)
  let gamma  = ref env in
  let gamma' = ref env in
  (* 1. On checke les parametre de type et on les ajoute a l'env *)
  gamma' := List.fold_left2 ptc_add !gamma (match c.type_class_params with
    | None -> []
    | Some l -> l) (get_ptc_borne_list c);
  (* TODO : faire sortir du fold la liste de quoi consrtuire les tcpc du point
   * 2. *)
  (* 2. On verifie que le type dont on herite est bien forme et on l'ajoute a
   * l'env *)
  match c.deriv with
    | None -> assert false
    | Some (t, es) ->
        if (is_bf !gamma' (typerType_of_typ !gamma' t)) then
          gamma := add_classe_env !gamma {
            tc_name = c.c_name;
            ttype_class_params = assert false;
            tparams = assert false;
            tderiv = assert false;
            tdecls = [];
            tc_loc = c.c_loc;
            tc_env = assert false  
          }
        else
          raise (TypeError (t.t_loc, "Ce type n'est pas bien forme"))


  
