
/* Analyseur syntaxique pour mini-scala */

%{
    open Ast
%}

/* Déclaration des tokens */

%token EOF

/* Priorités et associativité des tokens */

/* Points d'entrée de la grammaire */

/* Types des valeurs renvoyées par l'analyseur syntaxique */


%%

fichier:
    classes = classe*; main = classe_Main; EOF
        {classes = classes; main = main}
;
