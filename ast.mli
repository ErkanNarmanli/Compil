type loc    = Lexing.position * Lexing.position (* debut * fin *)

type ident  = string

type fichier = {
    classes     : classe list ;
    main        : classe_Main ; 
    f_loc       : loc ; }

and classe = {
    c_name              : ident ;
    type_class_params   : (param_type_classe list) option ;
    params              : (parametre list) option ; (*paramètres*)
    deriv               : (typ * expr list option) option;(*héritage*)
    decls               : decl list ; 
    c_loc               : loc ; }

and decl = {
    decl_cont   : declCont ;
    decl_loc    : loc       ; }

and declCont =
    | Dvar of var
    | Dmeth of methode

and var = {
    v_cont      : varCont ;
    v_loc       : loc   ; }

and varCont =
    | Val   of ident * (typ option) * expr
    | Var   of ident * (typ option) * expr

and methode = {
    m_cont  : methodeCont ;
    m_loc   : loc ; }

and methodeCont = (*localisé dans meth_block et meth_expr*)
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
    p_typ       : typ   ; 
    p_loc       : loc ; }

and param_type_heritage =
    | Hinf of typ (* >: *)
    | Hsup of typ (* <: *)

and param_type = {
    pt_cont     : param_typeCont ;
    pt_loc      : loc ; }

and param_typeCont = ident * param_type_heritage option

and param_type_classe = {
    ptc_cont    : param_type_classeCont ;
    ptc_loc     : loc ; }

and param_type_classeCont = 
    | PTCplus   of param_type
    | PTCmoins  of param_type
    | PTCrien   of param_type

and typ = {
    t_name      : ident ;
    args_type   : arguments_type ; 
    t_loc       : loc ; }

and arguments_type = {
    at_cont          : arguments_typeCont ;
    at_loc           : loc }
and arguments_typeCont = typ list option

and classe_Main = {
    cM_cont         : classe_MainCont ;
    cM_loc          : loc }
and classe_MainCont = decl list

and expr = {
    e_cont          : exprCont ;
    e_loc           : loc }
and exprCont =
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
    | Eprint    of expr
    | Ebloc     of bloc

and bloc = 
    { bl_cont        : blocCont ;
      bl_loc         : loc }

and blocCont = instruction list

and instruction = 
    | Ivar  of var
    | Iexpr of expr

and binop = 
    { b_cont        : binopCont ;
      b_loc         : loc }

and binopCont = EqRef | NeRef | Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or
(* EqRef = eq ; NeRef = ne ; Eq = == ; Ne = != *)

and acces = 
    { a_cont        : accesCont ;
      a_loc         : loc }

and accesCont = 
    | Aident        of ident
    | Aexpr_ident   of expr * ident

