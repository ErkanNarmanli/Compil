(* Arbre de syntaxe abstraite décoré de Mini-Scala*)

open Ast

(*ON DÉFINIT DES OBJETS QUI SERONT UTILES PLUS TARD*)
type loc    = Lexing.position * Lexing.position (* début, fin *)
type ident  = string

type typ =
    | Tany
    | TanyVal
    | Tboolean
    | Tint
    | Tunit
    | TanyRef
    | Tstring
    | Tnull
    | Tnothing
    | Tmethode of typ list * typ

(* Un ensemble de classes *)
module Cmap = Map.Make(String)

(* Le type des contraintes >: *)
type constr = string * typ

(* Et un ensemble de contraintes *)
module Cset = Set.Make(constr)

(* env (= environnement local) est un ensemble de classes,
 *  et un ensemble de de contraintes de type,    
 *  et une suite ordonée de déclarations de variables *)
type context = Cmap.t * Cset.t * var list
let empty_context = Cmap.empty * Cset.empty * []

(*ON DÉFINIT L'ARBRE À PROPREMENT PARLER*)
type tfichier = {
    tclasses    : tclasse list ;
    tmain       : tclasse_Main ;
    tf_loc      : loc ; }

and tclasse = {
    tc_name             : indent ;
    ttype_class_params  : (param_type_classe list) option ; (*débile ?!*)
    tparams             : (tparametre list) option ; (*débile ?!*)
    tderiv              : (tttyp * texpr list option ) option ;
    tdecls              : tdecl list ;
    tc_loc              : loc ; 
    tc_env              : context ; }
    (*TODO : les classes ont pas de type, hein ?*)

and tdecl = 
    | TDvar     of tvar     
    | TDmeth    of tmethode 

and tvar = {
    tv_cont     : tvarCont  ;
    tv_loc      : loc       ;
    tv_typ      : typ       ; }

and tvarCont = 
    | TVal  of indent * (tttyp option) * texpr
    | TVar  of indent * (tttyp option) * texpr

and tmethode = {
    tm_cont     : tmethodeCont  ;
    tm_loc      : loc           ; (* Plutôt juste la localisation du
                                   * nom de la methode, idéalement *)
    tm_typ      : typ           ; (*Ouais, hein ?*)
    tm_env      : contex        ; }

and tmethodeCont = 
    | TMbloc    of tmeth_bloc
    | TMexpr    of tmeth_expr

and tmeth_bloc = {
    tmb_name            : ident ;
    tmb_override        : bool  ;
    tmb_type_params     : (param_type list) option ;
    tmb_params          : parametre list ;
    tbloc               : tbloc ; }

and tmeh_expr = {
    tme_name            : ident ;
    tme_override        : bool ;
    tme_type_params     : (param_type list) option ;
    tme_params          : parametre list ;
    tres_type           : ttyp ;
    tres_expr           : texpr ; }


    
(* On ne refefinit pas parametre *)
(* On ne redéfinit pas param_type_heritage *)
(* On ne redéfinit pas param_type *)
(* On ne redéfinit pas param_type_classe *)
(* On ne redéfinit pas typ *)
(* On ne reféfinit pas arguments_type *)

and tclasse_Main = {
    tcM_cont    : tclasse_MainCont  ;
    tcM_loc     : loc               ; } (* idem juste la loc du mot clef
                                        * 'class Main', c'est mieux *)
and tclasse_MainCont = tdec list ;

and texr = {
    te_cont     : texprCont ;
    te_loc      : loc       ;
    te_typ      : typ       ; }
                               
and texprCont = 
    | TEvoid
    | TEthis
    | TEnull
    | TEint      of int
    | TEstr      of string
    | TEbool     of bool
    | TEacc      of tacces
    | TEacc_exp      of tacces * texpr
    | TEacc_typ_exp  of tacces * arguments_type * texpr list
    | TEnew      of ident * arguments_type * texpr list
    | TEneg      of texpr
    | TEmoins    of texpr
    | TEbinop    of binop * texpr * texpr 
    | TEif       of texpr * texpr
    | TEifelse   of texpr * texpr * texpr
    | TEwhile    of texpr * texpr
    | TEreturn   of texpr option
    | TEprint    of texpr
    | TEbloc     of tbloc

and tbloc = tinstruction list (* osef de la loc d'une instruction, quid du type ? *)

and tinstruction =
    | TIvar     of var
    | TIexpr    of texpr

(* On en redéfinit pas binop *)

and tacces = {
    ta_cont     : taccesCont    ;
    ta_loc      : loc           ;
    ta_typ      : typ           ; } (* On type, non ? *)

and taccesCont = 
    | TAident       of indent
    | TAexpr_indent of texpr * indent
