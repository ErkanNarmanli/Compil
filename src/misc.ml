open Tast
open Ast

module Sset = Set.Make(String)

(**************************
 * MISCELLANEOUS FUNCTIONS *
 **************************)

(* ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit *)
let rec iter3 f l1 l2 l3 = match (l1, l2, l3) with
  | ([], [], []) -> ()
  | (t1::q1, t2::q2, t3::q3) ->
      f t1 t2 t3;
      iter3 f q1 q2 q3
  | _ -> raise (Invalid_argument "Tailles incompatibles dans iter3") 

(* ('a -> 'b -> 'c -> ->'d) -> 'a -> 'b list -> 'c list -> 'd list -> 'a *)
let rec fold_left3 f x l1 l2 l3 = match (l1, l2, l3) with
  | ([], [], []) -> x
  | (t1::q1, t2::q2, t3::q3) ->
      fold_left3 f (f x t1 t2 t3) q1 q2 q3
  | _ -> raise (Invalid_argument "Tailles incompatibles dans fold_left3.")

(* Même résultat qu'un List.filter puis List.map en un seul parcours de la liste
 * ('a -> 'b option) -> 'a list -> 'b list *)
let rec filter_map f = function
  | [] -> []
  | t::q ->
      begin match f t with
        | None    -> filter_map f q
        | Some x  -> x::(filter_map f q)
      end

(* map n'agissant que dans sur les classes :
 * (ident*substitution -> typerType) -> typerType -> typerType *)
let map_class f = function
  | Tclasse (id, subst) -> f (id, subst)
  | Tany | TanyVal | Tboolean | Tint | Tunit
         | TanyRef | Tstring | Tnull | Tnothing as t' -> t'

(* map n'agissant que sur les classes avec une valeur par défaut pour les autres
 * types.
 * 'a -> (ident*substitution -> 'a) -> typerType -> 'a *) 
let map_class_def def f = function
  | Tclasse (id, subst) -> f (id, subst)
  | Tany | TanyVal | Tboolean | Tint | Tunit
         | TanyRef | Tstring | Tnull | Tnothing -> def

(* map n'agissant que sur les classes avec une valeur par défaut pour les autres
 * types.
 * 'a -> (ident*substitution -> 'a) -> typerType -> 'a *) 
let map_class_err emsg f = function
  | Tclasse (id, subst) -> f (id, subst)
  | Tany | TanyVal | Tboolean | Tint | Tunit
         | TanyRef | Tstring | Tnull | Tnothing -> failwith emsg

(* Indique si les éléments de la liste après passage par la fonction de
 * transport sont distincts deux à deux.
 * ('a -> string) -> 'a list -> bool *)
let list_uniq f l = 
  let rec aux sset = function
    | [] -> true
    | t::q ->
        let s = f t in
        if Sset.mem s sset then
          false
        else
          aux (Sset.add s sset) q in
  aux Sset.empty l

(* 'a list option -> 'a list *)
let get_list = function
  | None -> []
  | Some l -> l

(* 'a option -> bool *)
let is_none = function
  | None -> true
  | Some _ -> false

(* ('a -> 'b) -> 'a option -> 'b option *)
let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

(* 'a option -> 'a *)
let get_opt = function
  | None -> raise Not_found
  | Some x -> x


(* Rend la position d'une liste de param_typ_classe
 * param_type_classe -> loc *)
let make_ptcs_loc ptcs = 
    let rec aux = function
        | []      ->
                failwith "make_ptcs_loc n'est pas censé prendre une liste vide."
        | [ptc]   -> snd ptc.ptc_loc
        | _::q    -> aux q
    in (fst( (List.hd ptcs).ptc_loc), aux ptcs)

(*************************
 * Accesseurs pour l'AST *
 *************************)

(* param_type_classe -> param_type_heritage option *)
let get_ptc_borne ptc = match ptc.ptc_cont with
  | PTCplus  pt ->  snd pt.pt_cont
  | PTCmoins pt ->  snd pt.pt_cont
  | PTCrien  pt ->  snd pt.pt_cont

(* param_type -> ident *)
let get_pt_id pt = fst pt.pt_cont

(* param_type_classe -> ident *)
let get_ptc_id ptc = match ptc.ptc_cont with
  | PTCplus  pt ->  fst pt.pt_cont
  | PTCmoins pt ->  fst pt.pt_cont
  | PTCrien  pt ->  fst pt.pt_cont

(* methode -> param_type list *)
let get_meth_type_params m = get_list m.m_type_params

(* var -> ident *)
let get_var_id v = match v.v_cont with
  | Var(i, _, _) -> i
  | Val(i, _, _) -> i


(******************************
 * Accesseurs pour l'AST typé *
 ******************************)

(* context_var -> ident *)
let get_cv_id = function
  | CVar (i, _) -> i
  | CVal (i, _) -> i

(* tparam_type -> ident *)
let get_tpt_id tpt = fst tpt.tpt_cont

(* tparam_type_classe -> ident *)
let get_tptc_id tptc = match tptc.tptc_cont with
  | TPTCplus  tpt -> fst tpt.tpt_cont
  | TPTCmoins tpt -> fst tpt.tpt_cont
  | TPTCrien  tpt -> fst tpt.tpt_cont

(* tparam_type_classe list -> ident list *)
let get_tptc_id_list = List.map get_tptc_id


(**************
 * Conversion *
 **************)

(* binop -> tbinop *)
let tbinop_of_binop b =
    { tb_cont = b.b_cont ; tb_loc = b.b_loc }

(* tparam_type_classe -> tparam_type *)
let tpt_of_tptc tptc = match tptc.tptc_cont with
  | TPTCplus tpt  -> tpt
  | TPTCmoins tpt -> tpt
  | TPTCrien tpt  -> tpt

(* tclasse -> context_classe *)
let context_classe_of_tclasse tc = {
  cc_name   = tc.tc_name;
  cc_tptcs  = tc.ttype_class_params;
  cc_params = tc.tparams;
  cc_deriv  = tc.tderiv;
  cc_env    = tc.tc_env
}

