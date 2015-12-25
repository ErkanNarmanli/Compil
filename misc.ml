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

(* Même résultat qu'un List.filter puis List.map en un seul parcours de la liste
 * ('a -> 'b option) -> 'a list -> 'b list *)
let rec filter_map f = function
  | [] -> []
  | t::q ->
      begin match f t with
        | None    -> filter_map f q
        | Some x  -> x::(filter_map f q)
      end

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

(* Rend la position d'une liste de param_typ_classe
 * param_type_classe -> loc *)
let make_ptcs_loc ptcs = 
    let rec aux = function
        | []      -> failwith "make_ptcs_loc n'est pas censé prendre une
                     liste vide, si ?"
        | [ptc]   -> snd ptc.ptc_loc
        | a::q    -> aux q
    in (fst( (List.hd ptcs).ptc_loc), aux ptcs)

(* Remplace le premier élément de la liste vérifiant le prédicat p par x. *)
let rec replace_in_list p x = function
  | []    ->  []
  | t::q  ->  if p t then x::q else t::(replace_in_list p x q)


(*************************
 * Accesseurs pour l'AST *
 *************************)

(* acces -> ident *)
let get_acces_id a = match a.a_cont with
    | Aident i ->           i
    | Aexpr_ident (e, i) -> i       

(* param_type_classe -> ident *)
let get_ptc_id p = match p.ptc_cont with
  | PTCplus  pt -> fst pt.pt_cont
  | PTCmoins pt -> fst pt.pt_cont
  | PTCrien  pt -> fst pt.pt_cont

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

(* classe -> param_type_heritage option list *)
let get_ptc_borne_list c = match c.type_class_params with
  | None -> []
  | Some l -> List.map get_ptc_borne l

(* methode -> tparams *)
let get_meth_params m = match m.m_cont with 
  | Mblock mb -> mb.mb_params
  | Mexpr me  -> me.me_params

(* methode -> param_type list *)
let get_meth_type_params m = match m.m_cont with
  | Mblock mb -> get_list mb.mb_type_params
  | Mexpr  me -> get_list me.me_type_params

(* methode -> ident *)
let get_meth_id m = match m.m_cont with
  | Mblock mb ->  mb.mb_name
  | Mexpr me  ->  me.me_name 

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

(* tvar -> ident *)
let get_tvar_id tv = match tv.tv_cont with
    | TVal (id, _, _)   -> id
    | TVar (id, _, _)   -> id

(* tmethode -> ident list *)
let get_meth_type_params_id_list m =
  List.map (fun tpt -> fst tpt.tpt_cont) m.tm_type_params

(* tmethode -> tparam_type_heritage option list *)
let get_bornes_list_m m =
  let rec aux = function
      | []     -> []
      | tpt::q -> (snd tpt.tpt_cont, tpt.tpt_loc) :: (aux q)
  in aux m.tm_type_params

(* tparam_type -> ident *)
let get_tpt_id tpt = fst tpt.tpt_cont

(* tparam_type_classe -> ident *)
let get_tptc_id tptc = match tptc.tptc_cont with
  | TPTCplus  tpt -> fst tpt.tpt_cont
  | TPTCmoins tpt -> fst tpt.tpt_cont
  | TPTCrien  tpt -> fst tpt.tpt_cont

(* tparam_type_classe list -> ident list *)
let rec get_tptc_id_list = List.map get_tptc_id

(* tclasse -> tparam_type_heritage option list *)
let get_bornes_list_c c =
  let rec aux = function
    | []      -> []
    | tptc::q -> begin match tptc.tptc_cont with
                   | TPTCplus tpt  -> (snd tpt.tpt_cont, tpt.tpt_loc) 
                   | TPTCmoins tpt -> (snd tpt.tpt_cont, tpt.tpt_loc) 
                   | TPTCrien tpt  -> (snd tpt.tpt_cont, tpt.tpt_loc) 
                 end :: (aux q) in
  aux c.ttype_class_params 

(**************
 * Conversion *
 **************)

(* acces -> tacces *)
let tacces_of_acces a = match a.a_cont with
    | Aident i ->           { ta_cont = TAident i; ta_loc = a.a_loc}
    | Aexpr_ident (e,i) ->  failwith "On arrive jamais ici"

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


