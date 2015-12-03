open Ast
open Tast

exception TypeError of loc * string

(* Fonctions utiles *)

(* Affichage d'une classe *)
let string_of_class i argst =
  i^"["^(
  List.fold_left (fun s t -> s^t.tt_name^", ") "" argst.tat_cont
  )^"]"

(* fonction de substitution
 * s : arguments_type -> tclasse -> tparametre -> typerType *)
let subst argst c p =
  assert false

(* Recherche de la déclaration d'une variable dans une var list *)
(* ident -> tvar list -> ttyp *)
let rec var_lookup id = function
    (* TODO pas beau : mieux = appliquer à env et non env.vars *)
  | []    ->  raise Not_found
  | v::q  ->  if id = get_var_id v then
                v.tv_typ
              else
                var_lookup id q

(* chercher une classe dans l'environnement env *)
let classe_lookup env id = 
  let rec aux = function
    | []      -> raise Not_found
    | c::q    -> if c.tc_name = id then
                   c
                 else
                   aux q
  in aux env.classes

let rec decl_lookup id = function
  | []    ->  raise Not_found
  | d::q  ->  begin match d with
                | TDvar v -> if id = get_var_id v then
                               d
                             else
                               decl_lookup id q
                | TDmeth m -> assert false
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
                  | TDmeth m ->  assert false
                                        (*
                                        if x = get_meth_id m then
                                            m.tm_typ
                                        else
                                            decl_type_lookup x q
                                            *)
              end
        
(* Sous-typage *)
let rec is_sstype t1 t2 = 
    assert false

let max_type t1 t2 = 
    assert false 

(* Types bien formés *)
let is_bf env t = 
    assert false

(* Typage à proprement parler *)

(* Typage des expressions *)
let rec type_expr env e = match e.e_cont with
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
  | Eacc a      ->  begin match a.a_cont with
                      | Aident id -> (* CACA TODO *)
                          begin try {
                            te_cont = TEacc (tacces_of_acces a);
                            te_loc = e.e_loc;
                            te_typ = var_lookup id env.vars }
                          with
                            | Not_found -> try 
                                begin match var_lookup "this" env.vars with
                                  | Tclasse (c,_) ->
                                      begin match decl_lookup id c.tdecls with
                                        | TDvar v -> {
                                              te_cont = TEacc
                                                (tacces_of_acces a);
                                              te_loc = e.e_loc ;
                                              te_typ = v.tv_typ
                                            }
                                        | TDmeth m -> assert false (*
                  TODO *)
                                      end
                                  | _ -> raise (TypeError (e.e_loc, "Le
                                         mot clef \"this\" ne fait référence
                                         à aucune classe"))    
                                end with
                                  | Not_found -> raise (TypeError
                                      (a.a_loc, "Impossible de déterminer à
                                      quoi l'identificateur \""^id^"\" fait
                                      référence, ni dans l'env local, ni
                                      dans les champs de la classe"))
                         end
                     | Aexpr_ident (e',x) ->
                         let e'' = type_expr env e' in
                         begin match e''.te_typ with
                           | Tclasse (c, _) ->
                               let t = begin try 
                                 decl_type_lookup x c.tdecls
                               with
                                 | Not_found ->  raise (TypeError
                                     (e'.e_loc, "La classe de cette expression
                                     n'a pas de champ \""^x^"\""))
                               end in 
                               {
                                 te_cont = TEacc {
                                     ta_cont = TAexpr_ident (e'', x);
                                     ta_loc = a.a_loc
                                   };
                                 te_loc = e.e_loc;
                                 te_typ = t
                               }
                           | _ ->  raise (TypeError (e.e_loc, "Cette
                               expression n'est pas une instance d'une classe,
                               elle ne peut avoir de champ \""^x^"\""))
                         end
                   end
  | Eacc_exp (a,e') -> begin match a.a_cont with
                         | Aident i -> (* cas où a est juste un nom de
                          variable *)
                             begin try
                               let t1 = var_lookup i env.vars in
                               (* on a bien trouvé la variable identifiée
                                * par i *)
                               let e'' = type_expr env e' in
                               if is_sstype t1 e''.te_typ then
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
                                   type_expr env {
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
                             let e1 = type_expr env {
                                 e_cont = Eacc {
                                     a_cont = Aexpr_ident (e'',i);
                                     a_loc = a.a_loc
                                   };
                                 e_loc = a.a_loc
                             } in
                             let e2 = type_expr env e' in
                             if is_sstype e2.te_typ e1.te_typ then
                               let a' = begin match e1.te_cont with
                                 | TEacc a'' ->  a''
                                 | _ ->          assert false
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
  | Emoins e' ->  let e'' = type_expr env e' in
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
  | Eneg e' ->  let e'' = type_expr env e' in
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
                let e1' = type_expr env e1 in
                let e2' = type_expr env e2 in
                begin match b.b_cont with
                  (* On peut comparer les classes et les chaînes de caractères
                   * avec ne et eq (chez nous : NeRef et EqRef)
                   * Le résultat est un booléen *)
                  | NeRef | EqRef -> 
                      if (is_sstype e1'.te_typ TanyRef) &&
                         (is_sstype e2'.te_typ TanyRef) then {
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
  | Eprint e' ->let e'' = type_expr env e' in
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
                let eb' = type_expr env eb in
                let e1' = type_expr env e1 in
                let e2' = type_expr env e2 in
                (* eb' doit être un booléen
                 * Les types de e1' et e2' doivent être comparables *)
                if (eb'.te_typ = Tboolean) &&
                   ((is_sstype e1'.te_typ e2'.te_typ) ||
                   (is_sstype e2'.te_typ e2'.te_typ))
                then {
                      te_cont = TEifelse (eb', e1', e2');
                      te_typ = max_type e1'.te_typ e2'.te_typ;
                      te_loc = e.e_loc
                    }
                else raise (TypeError (e.e_loc, "Le type de retour est mal 
                     défini : les cas if et else ont des types
                     incompatibles"))
  (* Le sucre syntaxique : 
   * On se ramène au cas précédent avec un () pour le deuxième argument
   * (Evoid chez nous) *)
  | Eif(eb, e1) ->
                type_expr env {
                  e_cont = Eifelse (eb,e1,{e_cont = Evoid; e_loc = e.e_loc});
                  e_loc = e.e_loc
                }
  (* Boucle while (eb) e1 *)
  | Ewhile (eb, e1) ->    
                (* il suffit juste que eb soit booléenne et que e1 soit bien
                 * typée *)
                let eb' = type_expr env eb in
                if eb'.te_typ = Tboolean then
                  let e1' = type_expr env e1 in {
                    te_cont = TEwhile (eb', e1');
                    te_typ = Tunit;
                    te_loc = e.e_loc
                  }
                else raise (TypeError (eb.e_loc, "Cette expression n'est pas
                     booléenne"))
  | Enew (i, argst , es) ->
                let c = try classe_lookup env i with
                  | Not_found -> raise (TypeError (e.e_loc, "L'identificateur
                  \""^i^"\" ne fait référene à aucune classse connue"))
                in if (is_bf env (Tclasse (c, targst_of_argst argst))) then
                  assert false
                else
                  raise (TypeError (e.e_loc, "Le type \""^(string_of_class i
                  (targst_of_argst argst))^"\" n'est pas bien formé"))
                
  | _ -> assert false


