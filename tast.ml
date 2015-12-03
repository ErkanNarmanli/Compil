(* TODO : RELIRE TOUT LE TAST *)
open Ast

(* Arbre de syntaxe abstraite décoré de Mini-Scala*)

(*ON DÉFINIT DES OBJETS QUI SERONT UTILES PLUS TARD*)
type loc    = Lexing.position * Lexing.position (* début, fin *)
type ident  = string

(* Un map indexé par des chaines de caractères *)
module Smap = Map.Make(String)

type typerType =
    | Tany
    | TanyVal
    | Tboolean
    | Tint
    | Tunit
    | TanyRef
    | Tstring
    | Tnull
    | Tnothing
    | Tclasse of tclasse * targuments_type

(* Le type des contraintes >: *)
and constr = string * typerType

(* env (= environnement local) est un ensemble de classes,
 *  et un ensemble de de contraintes de type,    
 *  et une suite ordonée de déclarations de variables *)
and context = {
    classes     : tclasse list;
    constrs     : constr list; 
    vars        : tvar list
}

(*ON DÉFINIT L'ARBRE À PROPREMENT PARLER*)
and tfichier = {
    tclasses    : tclasse list ;
    tmain       : tclasse_Main ;
    tf_loc      : loc ; 
    tf_env      : context}

and tclasse = {
    tc_name             : ident ;
    ttype_class_params  : tparam_type_classe list;
    tparams             : tparametre list ; 
    tderiv              : (ttyp * texpr list) option ;
    tdecls              : tdecl list ;
    tc_loc              : loc ; 
    tc_env              : context ; }

and tdecl = 
    | TDvar     of tvar     
    | TDmeth    of tmethode 

and tvar = {
    tv_cont     : tvarCont  ;
    tv_typ      : typerType ;
    tv_loc      : loc }

and tvarCont = 
    | TVal  of ident * ttyp * texpr
    | TVar  of ident * ttyp * texpr

and tmethode = {
    tm_cont     : tmethodeCont  ;
    tm_loc      : loc           ; (* Plutôt juste la localisation du
                                   * nom de la methode, idéalement *)
    tm_env      : context        ; }

and tmethodeCont = 
    | TMbloc    of tmeth_bloc
    | TMexpr    of tmeth_expr

and tmeth_bloc = {
    tmb_name            : ident ;
    tmb_override        : bool  ;
    tmb_type_params     : (tparam_type list) option ;
    tmb_params          : tparametre list ;
    tbloc               : tbloc ; }

and tmeth_expr = {
    tme_name            : ident ;
    tme_override        : bool ;
    tme_type_params     : tparam_type list ;
    tme_params          : tparametre list ;
    tres_type           : ttyp ;
    tres_expr           : texpr ; }

and tparametre = {
    tp_name             : ident ; 
    tp_loc              : loc }

and tparam_type_heritage = 
    | HTinf of ttyp (* >: *)
    | HTsup of ttyp (* <: *)

and tparam_type = {
    tpt_cont            : tparam_typeCont ;
    tpt_loc             : loc }

and tparam_typeCont = ident * tparam_type_heritage option

and tparam_type_classe = {
    tptc_cont           : tparam_type_classeCont ;
    tptc_loc            : loc }
and tparam_type_classeCont =
    | TPTCplus  of tparam_type
    | TPTCmoins of tparam_type
    | TPTCrien  of tparam_type

and ttyp = {
    tt_name         : ident;
    targs_type      : targuments_type ;
    tt_loc          : loc }

and targuments_type = {
    tat_cont            : targuments_typeCont ;
    tat_loc             : loc }

and targuments_typeCont = ttyp list

and tclasse_Main = {
    tcM_cont    : tclasse_MainCont  ;
    tcM_loc     : loc               ; } (* idem juste la loc du mot clef
                                        * 'class Main', c'est mieux *)
and tclasse_MainCont = tdecl list

and texpr = {
    te_cont     : texprCont ;
    te_loc      : loc       ;
    te_typ      : typerType       ; }
                               
and texprCont = 
    | TEvoid
    | TEthis
    | TEnull
    | TEint      of int
    | TEstr      of string
    | TEbool     of bool
    | TEacc      of tacces
    | TEacc_exp      of tacces * texpr
    | TEacc_typ_exp  of tacces * targuments_type * texpr list
    | TEnew      of ident * targuments_type * texpr list
    | TEneg      of texpr
    | TEmoins    of texpr
    | TEbinop    of tbinop * texpr * texpr 
    | TEif       of texpr * texpr
    | TEifelse   of texpr * texpr * texpr
    | TEwhile    of texpr * texpr
    | TEreturn   of texpr option
    | TEprint    of texpr
    | TEbloc     of tbloc

and tbloc = tinstruction list (* osef de la loc d'une instruction, quid du type ? *)

and tinstruction =
    | TIvar     of tvar
    | TIexpr    of texpr

and tbinop = {
    tb_cont             : binopCont ;
    tb_loc              : loc }

and tacces = {
    ta_cont     : taccesCont    ;
    ta_loc      : loc           ;
}

and taccesCont = 
    | TAident      of ident
    | TAexpr_ident of texpr * ident

(* Des accesseurs parce que yen a marre de faire des matchs dans tous les sens *)
let get_var_id tv = match tv.tv_cont with
    | TVal (id, _, _)   -> id
    | TVar (id, _, _)   -> id

let get_meth_id m = match m.tm_cont with
    | TMbloc tmb -> tmb.tmb_name
    | TMexpr tme -> tme.tme_name

let tacces_of_acces a = match a.a_cont with
    | Aident i ->           { ta_cont = TAident i; ta_loc = a.a_loc}
    | Aexpr_ident (e,i) ->  assert false

let rec ttyp_of_typ t = 
  {
    tt_name     = t.t_name;
    targs_type = targst_of_argst t.args_type;
    tt_loc      = t.t_loc
  }

and targst_of_argst argst = 
  {
    tat_cont = begin match argst.at_cont with
        | None   -> []
        | Some l -> List.map ttyp_of_typ l 
      end;
    tat_loc = argst.at_loc
  }


let tbinop_of_binop b =
    { tb_cont = b.b_cont ; tb_loc = b.b_loc }

let get_acces_id a = match a.a_cont with
    | Aident i ->           i
    | Aexpr_ident (e, i) -> i       

let get_decl_ttyp = function
    | TDvar v ->    begin match v.tv_cont with
                        | TVal (_, t, _) ->     t
                        | TVar (_, t, _) ->     t
                    end
    | TDmeth m ->   assert false  

(* tparam_type_classe -> ident *)
let get_ptc_id p = assert false

