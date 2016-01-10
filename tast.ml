open Ast

(* Arbre de syntaxe abstraite décoré de Mini-Scala*)

(*ON DÉFINIT DES OBJETS QUI SERONT UTILES PLUS TARD*)
type loc    = Lexing.position * Lexing.position (* début, fin *)
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
    | Tclasse of ident * substitution

(* Un type pour les substitutions
 * On chosit de représenter les substitutions par des Maps *)
and substitution = typerType Smap.t

(* Le type des contraintes >: *)
and constr = ident * typerType

(* Le minimum d'information dont on a besion sur les variables dans
 * l'environnement *)
and context_var = 
  | CVar of ident * typerType
  | CVal of ident * typerType

(* Idem pour les classes *)
and context_classe = {
  cc_name       : ident;
  cc_tptcs      : tparam_type_classe list;
  cc_params     : tparametre list;
  cc_deriv      : (typerType * texpr list) option;
  cc_env        : context;
}

(* env (= environnement local) est un ensemble de classes,
 *  et un ensemble de de contraintes de type,    
 *  et une suite ordonée de déclarations de variables *)
and context = {
    classes     : context_classe list;
    constrs     : constr list; 
    vars        : context_var list;
    meths       : tmethode list
}

(*ON DÉFINIT L'ARBRE À PROPREMENT PARLER*)
and tfichier = {
    tclasses    : tclasse list ;
    tmain       : tclasse_Main ;
    f_env       : context
    }

and tclasse = {
    tc_name             : ident ;
    ttype_class_params  : tparam_type_classe list;
    tparams             : tparametre list ; 
    tderiv              : (typerType * texpr list) option ;
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
    | TVal  of ident * typerType * texpr
    | TVar  of ident * typerType * texpr

and tmethode = {
    tm_name             : ident ;
    tm_override         : bool ;
    tm_type_params      : tparam_type list ;
    tm_params           : tparametre list ;
    tm_res_type         : typerType ;
    tm_res_expr         : texpr ;
    tm_loc              : loc ;
    tm_env              : context }

and tparametre = {
    tp_name             : ident ; 
    tp_typ              : typerType;
    tp_loc              : loc }

and tparam_type_heritage = 
    | HTinf of typerType (* >: *)
    | HTsup of typerType (* <: *)

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

and targuments_type = {
    tat_cont            : targuments_typeCont ;
    tat_loc             : loc }

and targuments_typeCont = typerType list

and tclasse_Main = {
    tcM_cont    : tclasse_MainCont  ;
    tcM_loc     : loc               ;
    tcM_env     : context
} 
 
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
    | TEacc_typ_exp  of texpr * ident * targuments_type * texpr list
    | TEnew      of ident * targuments_type * texpr list
    | TEneg      of texpr
    | TEmoins    of texpr
    | TEbinop    of tbinop * texpr * texpr 
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
    tb_cont     : binopCont ;
    tb_loc      : loc }

and tacces = {
    ta_cont     : taccesCont    ;
    ta_loc      : loc           ;
}

and taccesCont = 
    | TAident      of ident
    | TAexpr_ident of texpr * ident

