(* Arbre de syntaxe abstraite décoré de Mini-Scala*)

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


(* Un map indexé par des chaines de caractères *)
module Smap = Map.Make(String)

(* Le type des contraintes >: *)
type constr = string * typ

(* Et un ensemble de contraintes *)
module Cset = Set.Make(
    struct 
        let compare = Pervasives.compare
        type t = constr
    end
)


(*ON DÉFINIT DES OBJETS QUI SERONT UTILES PLUS TARD*)
type loc    = Lexing.position * Lexing.position (* début, fin *)
type ident  = string

(* env (= environnement local) est un ensemble de classes,
 *  et un ensemble de de contraintes de type,    
 *  et une suite ordonée de déclarations de variables *)
type context = tclasse Smap.t * Cset.t * tvar list

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
    tv_loc      : loc       ;
    tv_typ      : typ       ; }

and tvarCont = 
    | TVal  of ident * (ttyp option) * texpr
    | TVar  of ident * (ttyp option) * texpr

and tmethode = {
    tm_cont     : tmethodeCont  ;
    tm_loc      : loc           ; (* Plutôt juste la localisation du
                                   * nom de la methode, idéalement *)
    tm_typ      : typ           ; (* Ouais, hein ? Je sais plus, laissons dans
                                   * le doute *)
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
    tme_type_params     : (tparam_type list) option ;
    tme_params          : tparametre list ;
    tres_type           : ttyp ;
    tres_expr           : texpr ; }

and tparametre = {
    tp_name             : ident ; 
    tp_typ              : ttyp ;
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
    tt_name             : ident ;
    targs_type          : targuments_type ;
    tt_loc              : loc }

and targuments_type = {
    tat_cont            : targuments_typeCont ;
    tat_col             : loc }

and targuments_typeCont = ttyp list

and tclasse_Main = {
    tcM_cont    : tclasse_MainCont  ;
    tcM_loc     : loc               ; } (* idem juste la loc du mot clef
                                        * 'class Main', c'est mieux *)
and tclasse_MainCont = tdecl list

and texpr = {
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
    tb_cont             : tbinopCont ;
    tb_loc              : loc }

and tbinopCont = EqRef | NeRef | Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul |
Div | Mod | And | Or

and tacces = {
    ta_cont     : taccesCont    ;
    ta_loc      : loc           ;
    ta_typ      : typ           ; } (* On type, non ? *)

and taccesCont = 
    | TAident       of ident
    | TAexpr_ident of texpr * ident