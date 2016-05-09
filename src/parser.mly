
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
%token          TRUE FALSE

(* Opérateurs arithmétiques. *)
%token ADD SUB MUL DIV MOD INF SUP

(* Opérateurs de comparaison. *)
%token EQ NE LT LE GT GE AND OR EQREF NEREF

%token CLASS EXTENDS NEW NULL OBJECT MAIN THIS DOT
%token PRINT RETURN VAL VAR DEF OVERRIDE EQUAL
%token ELSE IF WHILE
%token LPAR RPAR LACC RACC LCRO RCRO
%token EOF COMMA SEMICOLON CONS NOT

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
    | v = var       {{ decl_cont = Dvar v   ; decl_loc  = ($startpos, $endpos) }}
    | m = methode   {{ decl_cont = Dmeth m  ; decl_loc     = ($startpos, $endpos) }}
;

var:
    | VAR; id = IDENT; ct = constyp?; EQUAL; e = expr
        {{ v_cont = Var (id, ct, e) ; v_loc = ($startpos, $endpos) }}
    | VAL; id = IDENT; ct = constyp?; EQUAL; e = expr
        {{ v_cont = Val (id, ct, e) ; v_loc = ($startpos, $endpos) }}
;

methode:
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; b = bloc
        (* Sucre syntaxique : une méthode sans type de retour explicite a le
         * type de retour Unit. *)
                {
                  let l = fst b.bl_loc in
                  let res_type = {
                    t_name = "Unit";
                    args_type = {
                      at_cont = None;
                      at_loc  = (l, l);
                    };
                    t_loc = (l, l);
                  } in {
                    m_name        = id;
                    m_override    = o     ;
                    m_type_params = pts   ;
                    m_params      = ps    ;
                    m_res_type    = res_type;
                    m_res_expr    = {e_cont = Ebloc b; e_loc = b.bl_loc};
                    m_loc         = ($startpos, $endpos)
                }}
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; CONS;
        t = typ; EQUAL; e = expr
                {{
                  m_name        = id    ;
                  m_override    = o     ;
                  m_type_params = pts   ;
                  m_params      = ps    ;
                  m_res_type    = t     ;
                  m_res_expr    = e;
                  m_loc         = ($startpos, $endpos)
                }}
;

typ:
    id = IDENT; argst = arguments_type
        {{  t_name      = id    ;
            args_type   = argst ;
            t_loc       = ($startpos, $endpos)}} 
;

arguments_type:
    ts = typ_l?
        {{ at_cont = ts ; at_loc = ($startpos, $endpos) }}
;

param_type_classe:
    | ADD; p = param_type {{ ptc_cont = PTCplus  p ; ptc_loc = ($startpos, $endpos) }}
    | SUB; p = param_type {{ ptc_cont = PTCmoins p ; ptc_loc = ($startpos, $endpos)}}
    | p = param_type      {{ ptc_cont = PTCrien  p ; ptc_loc = ($startpos, $endpos) }} ;

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
    | id = IDENT; SUP; t = typ  {{ pt_cont = (id, Some (Hsup t)) ; 
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
            {{ e_cont = Estr s; e_loc = ($startpos, $endpos) }}
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
            {{ e_cont = e.e_cont; e_loc = ($startpos, $endpos) } }
    | a = acces             
            {{ e_cont = Eacc a; e_loc = ($startpos, $endpos) }}
    | a = acces; EQUAL; e = expr
            {{ e_cont = Eacc_exp (a, e); e_loc = ($startpos, $endpos) }}
    (* Suvre syntaxique pour l'appel de méthodes sans expression :
      * m[...](...) -> this.m[...](...) *)
    | a = acces; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
            {
              let (e, mid) = match a.a_cont with
                | Aident mid ->
                    let l = fst a.a_loc in
                    let this = {e_cont = Ethis; e_loc = (l, l)} in
                    (this, mid)
                | Aexpr_ident (e, mid) -> (e, mid)
              in
              { e_cont = Eacc_typ_exp (e, mid, argst, exprs); e_loc =
                ($startpos,$endpos) }}
    | NEW; id = IDENT; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
            {{ e_cont = Enew (id, argst, exprs); e_loc = ($startpos, $endpos) }}
    (* Sucre syntaxique pour le if sans else. *)
    | IF; LPAR; e1 = expr; RPAR; e2 = expr
            {{
              e_cont = Eifelse (e1, e2, {
                  e_cont = Evoid;
                  e_loc = ($endpos, $endpos)
              });
              e_loc = ($startpos, $endpos)
            }} %prec IF
    | IF; LPAR; e1 = expr; RPAR; e2 = expr; ELSE; e3 = expr
            {{ e_cont =  Eifelse (e1, e2, e3); e_loc = ($startpos, $endpos) }}
    | RETURN; e = expr      
            {{ e_cont = Ereturn (Some e); e_loc = ($startpos, $endpos) }}
    | e1 = expr; b = binop; e2 = expr
            {{ e_cont = Ebinop (b, e1, e2); e_loc = ($startpos, $endpos)
            }}
    | NOT; e = expr         
            {{ e_cont = Eneg e; e_loc = ($startpos, $endpos) }}  
    | SUB; e = expr         
            {{ e_cont = Emoins e; e_loc = ($startpos, $endpos) }} %prec unary_minus  
    | WHILE; LPAR; e1 = expr; RPAR; e2 = expr
            {{ e_cont = Ewhile (e1, e2); e_loc = ($startpos, $endpos) }} %prec WHILE
    | PRINT; LPAR; e = expr; RPAR
            {{ e_cont = Eprint e; e_loc = ($startpos, $endpos) }}
    | b = bloc             
            {{ e_cont = Ebloc b; e_loc = ($startpos, $endpos) }}
    | RETURN;               
            {{ e_cont = Ereturn None; e_loc = ($startpos, $endpos) }}
;


bloc:
    LACC; instrs = separated_list(SEMICOLON, instruction); RACC
            {{ bl_cont = instrs; bl_loc = ($startpos, $endpos) }}
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
        {{ f_classes = classes; main = main }} 
;

