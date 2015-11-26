
(* Analyseur syntaxique pour mini-scala *)

%{
    open Ast
%}

(* Déclaration des tokens
 * INF   et SUP     désignent   >: et <:
 * EQ    et NE      désignent   == et !=
 * EQREF et NEREF   désignent   ne et eq
 * NOT              désigne     !
 * EQUAL            désigne     =
 * Pour le reste c'est transparent *)

%token <int>    INT
%token <string> STRING
%token <string> IDENT

%token ADD SUB MUL DIV MOD INF SUP
%token EQ NE LT LE GT GE AND OR

%token CLASS DEF ELSE EQREF EXTENDS FALSE IF NEREF NEW NULL OBJECT
%token OVERRIDE PRINT RETURN THIS TRUE VAL VAR WHILE
%token MAIN EOF
%token LPAR RPAR LACC RACC LCRO RCRO CONS EQUAL COMMA NOT DOT SEMICOLON

(* Priorités et associativité des tokens *)
%nonassoc IF 
%nonassoc ELSE
%nonassoc WHILE RETURN
%right EQUAL
%left OR
%left AND
%left EQREF NEREF EQ NE
%left GT GE LE LT
%left ADD SUB
%left MUL DIV MOD
%right NOT
%left DOT
%right unary_minus

(* Points d'entrée de la grammaire *)
%start fichier

(* Types des valeurs renvoyées par l'analyseur syntaxique *)
%type <Ast.fichier> fichier

%%

(*
 * Définitions intermédiaires utiles
 *)

(* : <type> *)
constyp:
    CONS; t = typ
        { t }
;

(* [ <type>+, ] *)
typ_l:
    LCRO; ts = separated_nonempty_list(COMMA, typ) ; RCRO
        { ts }
;

(* [ <param_type_classe>*, ] *)
param_type_classe_l:
    LCRO; ptcs = separated_nonempty_list(COMMA, param_type_classe); RCRO
        { ptcs }
;

(* [ <param_type>+, ] *)
param_type_l:
    LCRO; pars = separated_nonempty_list(COMMA, param_type); RCRO
        { pars }
;

(* ( <parametres>*, ) *)
param_l:
    LPAR; params = separated_list(COMMA, parametre); RPAR
        { params }
;

(* ( <expr>*, ) *)
expr_l:
    LPAR; exprs = separated_list(COMMA, expr); RPAR
        { exprs }
;

(* extends <type> ( <expr>*, )?
 * utile dans la définition des classes
 *)
heritage:
    EXTENDS; t = typ; expr_lo = expr_l?
        { (t, expr_lo) }
;

(* var | expr *) 
instruction:
    | v = var       { Ivar v }
    | e = expr      { Iexpr e }
;

(* 
 * Définitions de la grammaire à proprement parler
 *)

decl:
    | v = var       {{ Dvar v   ; decl_loc  = ($startpos, $endpos) }}
    | m = methode   {{ Dmeth m  ; m_loc     = ($startpos, $endpos) }}
;

var:
    | VAR; id = IDENT; ct = constyp?; EQUAL; e = expr
        {{ Var (id, ct, e) ; v_loc = ($startpos, $endpos) }}
    | VAL; id = IDENT; ct = constyp?; EQUAL; e = expr
        {{ Val (id, ct, e) ; v_loc = ($startpos, $endpos) }}
;

methode:
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; b = bloc
                {{  m_desc          = Mblock {
                    mb_name         = id    ;
                    mb_override     = o     ;
                    mb_type_params  = pts   ;
                    mb_params       = ps    ;
                    bloc            = b
                } ; m_loc           = ($startpos, $endpos)
                }}
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; CONS;
        t = typ; EQUAL; e = expr
                {{  m_desc          = Mexpr {
                    me_name         = id    ;
                    me_override     = o     ;
                    me_type_params  = pts   ;
                    me_params       = ps    ;
                    res_type        = t     ;
                    res_expr        = e
                } ; m_loc           = ($startpos, $endpos)
                }
;

typ:
    id = IDENT; argst = arguments_type
        {{  t_name      = id    ;
            args_type   = argst ;
            t_loc       = ($startpos, $endpos)}} 
;

arguments_type:
    ts = typ_l?
        {{ at_cont = ts ; ac_loc = ($startpos, $endpos) }}
;

param_type_classe:
    | ADD; p = param_type {{ PTCplus  p ; ptc_loc = ($startpos, $endpos) }}
    | SUB; p = param_type {{ PTCmoins p ; ptc_loc = ($startpos, $endpos)}}
    | p = param_type      {{ PTCrien  p ; ptc_loc = ($startpos, $endpos) }} ;

parametre:
    id = IDENT; CONS; t = typ   {{p_name    = id ; 
                                    p_typ   = t ; 
                                    p_loc   = ($startpos, $endpos) 
                                }} ;

param_type:
    | id = IDENT                {{ pt_cont = (id, None) ; 
                                pt_loc = ($startpos, $endpos) }}
    | id = IDENT; INF; t = typ  {{ pt_cont = (id, Some (Hinf t)) ; 
                                pt_loc = ($startpos, $endpos) }}
    | id = IDENT; SUP; t = typ  {{ pt_cont (id, Some (Hsup t)) ; 
                                pt_loc = ($startpos, $endpos)}} ;

classe:
    CLASS; id = IDENT; ptcs = param_type_classe_l?; ps = param_l?;
    h = heritage? LACC; decls = separated_list(SEMICOLON, decl); RACC
        {{ c_name           = id;
        type_class_params   = ptcs;
        params              = ps;
        deriv               = h;
        decls               = decls ;
        c_loc               = ($startpos, $endpos)}}
;

expr:
    | i = INT               
            {{ e_cont = Eint i; e_loc = ($startpos, $endpos) }}
    | s = STRING           
            {{ e_cont = Estr s; e_loc = ($startpos, $endpos) } }
    | TRUE 
            {{ e_cont = Ebool true; e_loc = ($startpos, $endpos) } }
    | FALSE                 
            {{ e_cont = Ebool false; e_loc = ($startpos, $endpos) } }
    | LPAR; RPAR            
            {{ e_cont = Evoid; e_loc = ($startpos, $endpos) } }
    | THIS                  
            {{ e_cont = Ethis; e_loc = ($startpos, $endpos) } }
    | NULL                  
            {{ e_cont = Enull; e_loc = ($startpos, $endpos) } }
    | LPAR; e = expr; RPAR  
            {{ e_cont = e; e_loc = ($startpos, $endpos) } }
    | a = acces             
            {{ e_cont = Eacc a; e_loc = ($startpos, $endpos) }}
    | a = acces; EQUAL; e = expr
            {{ e_cont = Eacc_exp (a, e); e_loc = ($startpos, $endpos) }}
    | a = acces; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
            {{ e_cont = Eacc_typ_exp (a, argst, exprs); e_loc =
                ($startpos,$endloc) }}
    | NEW; id = IDENT; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
            {{ e_cont = Enew (id, argst, exprs); e_loc = ($startpos, $endpos) }}
    | IF; LPAR; e1 = expr; RPAR; e2 = expr
            {{ e_cont = Eif (e1, e2); e_loc = ($startpos, $endpos) }} %prec IF
    | IF; LPAR; e1 = expr; RPAR; e2 = expr; ELSE; e3 = expr
            {{ e_cont =  Eifelse (e1, e2, e3); e_loc = ($startpos, $endpos) }}
    | RETURN; e = expr      
            {{ e_cont = Ereturn (Some e); e_loc = ($startpos, $endpos) }}
    | e1 = expr; b = binop; e2 = expr
            {{ e_cont = Ebinop (b, e1, e2); e_loc = e_loc = ($startpos, $endpos)
            }}
    | NOT; e = expr         
            {{ e_cont = Eneg e; e_pos = ($startpos, $endpos) }}  
    | SUB; e = expr         
            {{ e_cont = Emoins e; e_loc = ($startpos, $endpos) }} %prec unary_minus  
    | WHILE; LPAR; e1 = expr; RPAR; e2 = expr
            {{ e_cont = Ewhile (e1, e2); e_loc = ($startpos, $endpos) }} %prec WHILE
    | PRINT; LPAR; e = expr; RPAR
            {{ e_cont = Eprint e; e_loc = ($startpos, $endpos) }}
    | b = bloc             
            {{ e_cont = Ebloc b; e_pos = ($startpos, $endpos) }}
    | RETURN;               
            {{ e_cont = Ereturn None; e_loc = ($startpos, $endpos) }}
;


bloc:
    LACC; instrs = separated_list(SEMICOLON, instruction); RACC
            {{ bl_cont = instrs; bl_loc = loc }}
;

acces:
    | i = IDENT             {{ a_cont = Aident i;
                               a_loc = ($startpos, $endpos) }}
    | e = expr; DOT; i = IDENT
                            {{ a_cont = Aexpr_ident (e, i);
                               a_loc = ($startpos, $endpos) }}
;

%inline binop:
    | EQREF         {{ b_cont = EqRef ;
                       b_loc = ($startpos, $endpos) }}
    | NEREF         {{ b_cont = NeRef ;
                       b_loc = ($startpos, $endpos) }}
    | EQ            {{ b_cont = Eq ;
                       b_loc = ($startpos, $endpos) }}
    | NE            {{ b_cont = Ne ;
                       b_loc = ($startpos, $endpos) }}
    | LT            {{ b_cont = Lt ;
                       b_loc = ($startpos, $endpos) }}
    | LE            {{ b_cont = Le ;
                       b_loc = ($startpos, $endpos) }}
    | GT            {{ b_cont = Gt ;
                       b_loc = ($startpos, $endpos) }}
    | GE            {{ b_cont = Ge ;
                       b_loc = ($startpos, $endpos) }}
    | ADD           {{ b_cont = Add ;
                       b_loc = ($startpos, $endpos) }}
    | SUB           {{ b_cont = Sub ;
                       b_loc = ($startpos, $endpos) }}
    | MUL           {{ b_cont = Mul;
                       b_loc = ($startpos, $endpos) }}
    | DIV           {{ b_cont = Div;
                       b_loc = ($startpos, $endpos) }}
    | MOD           {{ b_cont = Mod;
                       b_loc = ($startpos, $endpos) }}
    | AND           {{ b_cont = And;
                       b_loc = ($startpos, $endpos) }}
    | OR            {{ b_cont = Or;
                       b_loc = ($startpos, $endpos) }}
;

classe_Main:
    OBJECT; MAIN; LACC; decls = separated_list(SEMICOLON, decl); RACC
        {{ cM_cont = decls; cM_loc = ($startpos, $endpos) }}
;

fichier:
    classes = classe*; main = classe_Main; EOF
        {{ classes = classes; main = main; f_loc = ($startpos, $endpos) }} 
;

