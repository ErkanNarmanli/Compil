open Ast

exception TypeError of string

module Smap = Map.Make(String)

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

(* Une structure pour comparer les variables *) 
module V = struct
    type t = tvar
    let compare v1 v2   = Pervasives.compare v1.id v2.id
    let equal   v1 v2   = v1.id = v2.id
    let create          =   let r = ref 0
                            in  fun () -> incr r;
                                {id     = !r ;
                                v_def   = None }
end

(* Une structure pour comparer les classes *)
module C = struct
    type t = tclasse
    let compare c1 c2   = Pervasives.compare c1.name c2.name
    let equal   c1 c2   = c1.name = c2.name
    let create  name    = {name = name ; c_def = [] } 
end


(* env (= environnement local) est un ensemble de classes,
 *  et un ensemble de de contraintes de type,    
 *  et une suite ordonée de déclarations de variables *)

module Cset = Set.Make(C)


type env = {
    classes     : Cset.t ;
    (* contraites  : TODO *) 
    variables   : tvar listi ; }

(* Typage à proprement parler *)

