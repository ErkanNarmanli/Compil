open Tast
open Ast


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


(******************************
 * Accesseurs pour l'AST typé *
 ******************************)

(* tvar -> ident *)
let get_var_id tv = match tv.tv_cont with
    | TVal (id, _, _)   -> id
    | TVar (id, _, _)   -> id

(* tmethode -> ident *)
let get_meth_id m = match m.tm_cont with
  | TMbloc tmb  -> tmb.tmb_name
  | TMexpr tme  -> tme.tme_name

(* tmethode -> tparams *)
let get_tmeth_params m = match m.tm_cont with 
  | TMbloc tmb -> tmb.tmb_params
  | TMexpr tme -> tme.tme_params

(* tmethode -> typerType *)
let get_meth_type m = match m.tm_cont with
  | TMbloc _   -> Tunit
  | TMexpr tme -> tme.tres_type

(* tmethode -> ident list *)
let get_meth_type_params_id_list m = match m.tm_cont with
  | TMbloc tmb -> List.map (fun tpt -> fst tpt.tpt_cont) tmb.tmb_type_params
  | TMexpr tme -> List.map (fun tpt -> fst tpt.tpt_cont) tme.tme_type_params

(* tmethode -> tparam_type_heritage option list *)
let get_bornes_list_m m =
  let rec aux = function
      | []     -> []
      | tpt::q -> (snd tpt.tpt_cont, tpt.tpt_loc) :: (aux q)
  in aux (match m.tm_cont with
    | TMbloc tmb -> tmb.tmb_type_params
    | TMexpr tme -> tme.tme_type_params
  ) 

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

