(* Arbre de syntaxe abstraite décoré de Mini-Scala*)

open Ast

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
    | Tvar of tvar
    | Tclasse of tclasse
    | Tmethode of typ list * typ

(* Un type pour les variables *)
and tvar = {
    id              : int ;
    mutable v_def   : typ option ; }

(* Un type pour les classes *)
and tclasse = {
    name            : string ;
    mutable c_def   : typ list ; }

(*On décrit les objets décorés*)

type decor = {
    d_pos   : loc option ;
    d_typ   : typ option ; }    

type desc = 
    | DFichier  of fichier * decor
    | Dclasse   of classe * decor
    | Ddecl     of decl * decor
    | Dvar      of var * decor
    | Dmethode  of methode * decor
    | Dparametre            of parametre * decor
    | Dparam_type           of param_type * decor
    | Dparam_type_classe    of param_type_classe * decor
    | Dtyp              of typ * decor
    | Darguments_type   of arguments_type * decor
    | Dclasse_Main      of classe_Main * decor
    | Dexpr     of expr * decor
    | Dbloc     of bloc * decor
    | Dbinop        of binop * decor
    | DInstruction  of instruction * decor
    | Dacces        of acces * decor

type objet = {
    desc    : desc ;
    decor   : decor; }


(*Et on défniit chaque objet*)

type fichierD = {
    classesD    : desc list ; }
