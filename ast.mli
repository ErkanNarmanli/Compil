type ident = string

type fichier = {
    classes     : classe list ;
    main        : classe_Main ; }

and classe = {
    c_name              : ident ;
    type_class_params   : (param_type_classe list) option ;
    params              : (parametre list) option ; (*paramètres*)
    deriv               : (typ * ((expr list) option)) option;(*héritage*)
    decls               : decl list ; }

and decl =
    | Dvar of var
    | Dmeth of methode

and var =
    | Val   of ident * (typ option) * expr
    | Var   of ident * (typ option) * expr

and methode = 
    | Mblock of meth_block 
    | Mexpr  of meth_expr

and meth_block = {
    mb_name         : ident ;
    mb_override     : bool ;
    mb_type_params  : (param_type list) option ;
    mb_params       : parametre list ;
    bloc            : bloc ; }

and meth_expr = {
    me_name        : ident ;
    me_override    : bool ;
    me_type_params : (param_type list) option ;
    me_params      : parametre list ;
    res_type    : typ ;
    res_expr    : expr; }

and parametre = {
    p_name      : ident ;
    typ         : typ   ; }

and param_type = 
    | PTinf of ident * typ option (* >: *)
    | PTsup of ident * typ option (* <: *)

and param_type_classe = 
    | PTCplus   of param_type
    | PTCmoins  of param_type
    | PTCrien   of param_type

and typ = {
    t_name        : ident ;
    args_type   : arguments_type ; }

and arguments_type = typ list option

and classe_Main = decl list

and expr =
    | Evoid
    | Ethis
    | Enull
    | Eint      of int
    | Estr      of string
    | Ebool     of bool
    | Eacc      of acces
    | Eacc_exp      of acces * expr
    | Eacc_typ_exp  of acces * arguments_type * expr list
    | Enew      of ident * arguments_type * expr list
    | Eneg      of expr
    | Emoins    of expr
    | Ebinop    of binop * expr * expr 
    | Eif       of expr * expr
    | Eifelse   of expr * expr * expr
    | Ewhile    of expr * expr
    | Ereturn   of expr option
    | Epritn    of expr
    | Ebloc     of bloc

and bloc = instruction list

and instruction = 
    | Ivar  of var
    | Iexpr of expr

and binop = EqRef | NeRef | Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or
(* EqRef = eq ; NeRef = ne ; Eq = == ; Ne = != *)

and acces = 
    | Aident    of ident * ident
    | Aexpr     of expr * ident

