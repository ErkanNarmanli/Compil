open Tast
open Misc
open Context

(* La substitution triviale *)
let subst0 () =
  let init = [
    "Any",      Tany;
    "AnyVal",   TanyVal;
    "Boolean",  Tboolean;
    "Int",      Tint;
    "Unit",     Tunit;
    "AnyRef",   TanyRef;
    "String",   Tstring;
    "Null",     Tnull;
    "Nothing",  Tnothing;
    ] in
  List.fold_left (fun m (id, t) -> Smap.add id t m) Smap.empty init  

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

(* Ajout d'une valeur à une substitution en vue d'une composition .*)
let rec add_one_key id t m = 
  let f = function
    | Tclasse (cid, s) ->
        if cid = id then
          t
        else
          Tclasse (cid, add_one_key id t s)          
    | t' -> t'
  in let m' = Smap.map f m in
  Smap.add id t m'

(* Composition des substitutions *)
let subst_compose s s' = Smap.fold add_one_key s s'

(* fonction de substitution
 * subst : substitution -> typerType -> typerType *)
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

