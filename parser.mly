
/* Analyseur syntaxique pour mini-scala */

%{
    open Ast
%}

/* Déclaration des tokens */

%token EOF
%token <int> INT
%token <string> STRING
%token <string> IDENT

/* Priorités et associativité des tokens */

/* Points d'entrée de la grammaire */
%start fichier

/* Types des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.fichier> fichier

%%

fichier:
    i = INT; EOF     { Eint i}
;

