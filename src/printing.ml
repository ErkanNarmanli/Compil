open Tast
open Context
open Substitution
open Misc

(************************
 * PRINTING FUNCTIONS   *
 ************************)

(* Affichage d'un type.
 * context -> typerType -> string *)
let rec string_of_typ env = function
  | Tany ->         "Any"
  | TanyVal ->      "AnyVal"
  | Tboolean ->     "Boolean"
  | Tint ->         "Int"
  | Tunit ->        "Unit"
  | TanyRef ->      "AnyRef"
  | Tstring ->      "String"
  | Tnull ->        "Null"
  | Tnothing ->     "Nothing"
  | Tclasse (cid, s) ->
                    string_of_class env cid s

(* Affichage d'une classe
 * context -> ident -> substitution -> string *)
and string_of_class env i sub =
  let args = try
    let ids = get_tptc_id_list (classe_lookup env i).cc_tptcs in
    let args = List.map (subst_id sub) ids in
    List.fold_left (fun s t -> s^(string_of_typ env t)^", ") "" args
  with
    | Not_found -> "?"
  in
  i^"["^args^"]"

(* Affiche une représentation d'une substitution. 
 * context -> substitution -> unit *)
let print_subst env s = 
  print_string "{";
  let print_cor id t =
    Printf.printf "%s -> %s; " id (string_of_typ env t)
  in Smap.iter print_cor s;
  print_endline "}"

(* Affiche une représentation d'un environnement. 
 * context -> unit *)
let print_env env = 
  print_endline "Classes de l'environnement :";
  List.iter (fun c -> Printf.printf "==%s==\n" c.cc_name) env.classes;
  print_endline "Constraintes de l'environnement :";
  List.iter
    (fun (i, t) ->
      Printf.printf "%s >: %s\n" i (string_of_typ env t))
    env.constrs;
  print_endline "Variables de l'environnement :";
  List.iter (function
    | CVal(i, t) ->
        Printf.printf "val %s : %s\n" i (string_of_typ env t)
    | CVar(i, t) ->
        Printf.printf "var %s : %s\n" i (string_of_typ env t))
    env.vars;
  print_endline "Les méthodes de l'environnement :";
  List.iter (fun m -> print_endline m.tm_name) env.meths

