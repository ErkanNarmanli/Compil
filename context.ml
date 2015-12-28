open Tast

(* L'environnement vide *)
let env0 () = 
  {
    classes = [];
    constrs = [];
    vars    = [];
    meths   = []
  }

(* Ajoute une variable à un environnement *)
(* env -> context_var -> env *)
let add_var_env env cv = 
  {
    classes = env.classes ;
    constrs = env.constrs ;
    vars    = cv::env.vars;
    meths   = env.meths 
  }

(* Idem avec un tvar
 * context -> tvar -> context *)
let add_tvar_env env tv = 
  let cv = match tv.tv_cont with
    | TVal (i, t, _) -> CVal (i, t)
    | TVar (i, t, _) -> CVar (i, t)
  in add_var_env env cv

(* Ajoute une classe a un environnement
 * context -> context_classe -> context *)
let add_classe_env env c = 
  {
    classes = c::env.classes;
    constrs = env.constrs ; 
    vars    = env.vars;
    meths   = env.meths
  }

(* Ajoute une contrainte a un environnement
 * context -> (ident * typerType) -> context *)
let add_constr_env env constr = 
  {
    classes = env.classes;
    constrs = constr::env.constrs;
    vars    = env.vars;
    meths   = env.meths
  }

(* Ajoute une méthode à un environnement.
 * context -> tmethode -> context *)
let add_tmeth_env env tm =
  {
    classes = env.classes;
    constrs = env.constrs;
    vars    = env.vars;
    meths   = tm::env.meths
  }

(* Redéfinit l'environnement local d'une méthode.
 * context -> tmethode -> tmethode *)
let set_meth_env env m = {
  tm_name         = m.tm_name;
  tm_override     = m.tm_override;
  tm_type_params  = m.tm_type_params;
  tm_params       = m.tm_params;
  tm_res_type     = m.tm_res_type;
  tm_res_expr     = m.tm_res_expr;
  tm_loc          = m.tm_loc;
  tm_env          = env
}

(* Redéfinit l'environnement local d'une classe.
 * context -> context_classe -> context_classe. *)
let set_classe_env env c = {
  cc_name   = c.cc_name;
  cc_tptcs  = c.cc_tptcs;
  cc_params = c.cc_params;
  cc_deriv  = c.cc_deriv;
  cc_env    = env;
}

(* Change la définition d'une classe dans l'environnement.
 * context_classe -> context -> context *)
let update_classe_env c env =
  let update c' = 
    if c.cc_name = c'.cc_name then
      c
    else
      c' in
  {
    classes = List.map update env.classes;
    constrs = env.constrs;
    vars    = env.vars;
    meths   = env.meths;
  }

(* chercher une classe dans l'environnement env *)
let classe_lookup env id = 
  let rec aux = function
    | []      -> raise Not_found
    | c::q    -> if c.cc_name = id then
                   c
                 else
                   aux q
  in aux env.classes

(* Recherche de la déclaration d'une variable dans une var list et renvoie son
 * type ainsi qu'on booléen indiquant si la variable est mutable. *)
(* ident -> tvar list -> (typerType * bool) *)
let var_lookup id env = 
  let rec aux = function
    | []    ->  raise Not_found 
    | cv::q ->  begin match cv with
                  | CVar (i, t) ->  if i = id then
                                      (t, true)
                                    else
                                      aux q
                  | CVal (i, t) ->  if i = id then
                                      (t, false)
                                    else
                                      aux q
    end in aux env.vars

(* ident -> tclasse -> tmethode *)
let meth_lookup m_id env =
  let rec aux = function 
    | []   -> raise Not_found
    | m::q ->
        if m.tm_name = m_id then
          m
        else
          aux q
  in aux env.meths

