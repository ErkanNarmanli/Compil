open Ast
open Tast

exception TypeError of loc * string

(* Fonctions utiles *)

(* Recherche de la déclaration d'une variable dans une var list *)
let rec var_lookup id = function
    | []        -> raise Not_found
    | v::q      -> begin match v.tv_cont with
                        | TVal(id', t, _) -> if id = id' then
                                                t.tt_typ
                                            else
                                                var_lookup id q
                        | TVar(id', t, _) -> if id = id' then
                                                t.tt_typ
                                            else
                                                var_lookup id q
                    end

(* Détermination de la classe courante *)
let rec get_current_class env =
    match (var_lookup "this" env.vars) with
        | Tclasse (id, _) -> (Smap.find id env.classes) (* pas censé échouer *)
        | _ -> raise Not_found 

(* Parcourt une liste de déclarations à la recherche du champ x 
 * Renvoie le type de x *)
and decl_type_lookup x = function
    | []    ->  raise Not_found
    | d::q  ->  begin match d with
                        | TDvar v ->    if x = get_var_id v then
                                            v.tv_typ
                                        else
                                            decl_type_lookup x q
                        | TDmeth m ->   if x = get_meth_id m then
                                            m.tm_typ
                                        else
                                            decl_type_lookup x q
                end
        
(* Typage à proprement parler *)

(* Typage des expressions *)
and type_expr env e = match e.e_cont with
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
    | Ethis         -> begin try {
                           te_cont = TEthis;
                           te_loc = e.e_loc;
                           te_typ = (var_lookup "this" env.vars) }
                       with
                           | Not_found ->  raise (TypeError (e.e_loc, "impossible
                           de déterminer à quoi \"this\" fait référence"))
                           | e         -> raise e
                       end
    | Eacc a        -> begin match a.a_cont with
                           | Aident id ->
                                begin try {
                                     te_cont = TEacc (tacces_of_acces a);
                                     te_loc = e.e_loc;
                                     te_typ = var_lookup id env.vars }
                                with
                                    | Not_found -> begin
                                        let c = begin try
                                            get_current_class env
                                        with
                                        | Not_found -> raise (TypeError
                                        (a.a_loc, "Impossible de déterminer à
                                        quoi l'identificateur \""^id^"\" fait
                                        référence"))
                                        | e -> raise e
                                        end in
                                        begin try {
                                            te_cont = TEacc (tacces_of_acces a);
                                            te_loc = e.e_loc;
                                            te_typ = (decl_type_lookup id
                                            c.tdecls)
                                        } with
                                            | Not_found -> raise (TypeError
                                            (a.a_loc, "Impossible de déterminer à
                                            quoi l'identificateur \""^id^"\" fait
                                            référence"))
                                            | e -> raise e
                                        end
                                    end
                                    | e -> raise e
                                end
                            | Aexpr_ident (e,i) -> (assert false)
                        end
    | _ -> assert false


                                                                                       
