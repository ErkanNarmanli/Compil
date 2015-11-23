
/* Analyseur syntaxique pour mini-scala */

%{
    open Ast
%}

/* Déclaration des tokens */

%token EOF
%token <int> INT
%token <string> STRING

/* Priorités et associativité des tokens */

/* Points d'entrée de la grammaire */

/* Types des valeurs renvoyées par l'analyseur syntaxique */


%%

prog:
    i = INT     { Eint i}
;

