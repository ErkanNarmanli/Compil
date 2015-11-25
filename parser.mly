
(* Analyseur syntaxique pour mini-scala *)

%{
    open Ast
%}

(* Déclaration des tokens
 * INF et SUP       désignent   >: et <:
 * EQ et NE         désignent   == et !=
 * EQREF et NEREF   désignent   ne et eq
 * NOT              désigne     !
 * EQUAL            désigne     =
 * Pour le reste c'est transparent *)

%token <int> INT
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
    | v = var       { Dvar v }
    | m = methode   { Dmeth m }
;

var:
    | VAR; id = IDENT; ct = constyp?; EQUAL; e = expr
        { Var (id, ct, e) }
    | VAL; id = IDENT; ct = constyp?; EQUAL; e = expr
        { Val (id, ct, e) }
;

methode:
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; b = bloc
                { Mblock {
                    mb_name = id;
                    mb_override = o;
                    mb_type_params = pts;
                    mb_params = ps;
                    bloc = b
                }
                }
    | o = boption(OVERRIDE); DEF; id = IDENT; pts = param_type_l?;
        LPAR; ps = separated_list(COMMA, parametre); RPAR; CONS;
        t = typ; EQUAL; e = expr
                { Mexpr {
                    me_name = id;
                    me_override = o;
                    me_type_params = pts;
                    me_params = ps;
                    res_type = t;
                    res_expr = e
                }
                }
;

typ:
    id = IDENT; argst = arguments_type
        {{t_name = id;
        args_type = argst }} 
;

arguments_type:
    ts = typ_l?
        { ts }
;

param_type_classe:
    | ADD; p = param_type       {PTCplus p}
    | SUB; p = param_type       {PTCmoins p}
    | p = param_type            {PTCrien p} 
;

parametre:
    id = IDENT; CONS; t = typ   {{p_name = id; p_typ = t}}
;

param_type:
    | id = IDENT                { (id, None) }
    | id = IDENT; INF; t = typ  { (id, Some (Hinf t)) }
    | id = IDENT; SUP; t = typ  { (id, Some (Hsup t)) } 
;

classe:
    CLASS; id = IDENT; ptcs = param_type_classe_l?; ps = param_l?;
    h = heritage? LACC; decls = separated_list(SEMICOLON, decl); RACC
        {{ c_name = id;
        type_class_params = ptcs;
        params = ps;
        deriv = h;
        decls = decls }}
;

expr:
    | i = INT               { Eint i }
    | s = STRING            { Estr s }
    | TRUE                  { Ebool true }
    | FALSE                 { Ebool false }
    | LPAR; RPAR            { Evoid }
    | THIS                  { Ethis }
    | NULL                  { Enull }
    | LPAR; e = expr; RPAR  { e }
    | a = acces             { Eacc a }
    | a = acces; EQUAL; e = expr
                            { Eacc_exp (a, e) }
    | a = acces; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
                            { Eacc_typ_exp (a, argst, exprs) }
    | NEW; id = IDENT; argst = arguments_type; LPAR;
            exprs = separated_list(COMMA, expr); RPAR
                            { Enew (id, argst, exprs) }
    | IF; LPAR; e1 = expr; RPAR; e2 = expr
                            { Eif (e1, e2) } %prec IF
    | IF; LPAR; e1 = expr; RPAR; e2 = expr; ELSE; e3 = expr
                            { Eifelse (e1, e2, e3) }
    | RETURN; e = expr      { Ereturn (Some e) }
    | e1 = expr; b = binop; e2 = expr
                            { Ebinop (b, e1, e2) }
    | NOT; e = expr         { Eneg e }  
    | SUB; e = expr         { Emoins e } %prec unary_minus  
    | WHILE; LPAR; e1 = expr; RPAR; e2 = expr
                            { Ewhile (e1, e2) } %prec WHILE
    | PRINT; LPAR; e = expr; RPAR
                            { Eprint e }
    | b = bloc              { Ebloc b }
    | RETURN;               { Ereturn None }
;


bloc:
    LACC; instrs = separated_list(SEMICOLON, instruction); RACC
                            { instrs }
;

acces:
    | i1 = IDENT; DOT; i2 = IDENT   { Aident (i1, i2) }
    | e = expr; DOT; i = IDENT      { Aexpr (e, i) }
;

%inline binop:
    | EQREF         { EqRef }
    | NEREF         { NeRef }
    | EQ            { Eq }
    | NE            { Ne }
    | LT            { Lt }
    | LE            { Le }
    | GT            { Gt }
    | GE            { Ge }
    | ADD           { Add }
    | SUB           { Sub }
    | MUL           { Mul }
    | DIV           { Div }
    | MOD           { Mod }
    | AND           { And }
    | OR            { Or }
;

classe_Main:
    OBJECT; MAIN; LACC; decls = separated_list(SEMICOLON, decl); RACC
        { decls }
;

fichier:
    classes = classe*; main = classe_Main; EOF
        {{ classes = classes; main = main }} 
;

