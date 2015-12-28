open Tast
open Context
open Substitution
open Misc

(************************
 * PRINTING FUNCTIONS   *
 ************************)

(* Affichage d'un type *)
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

(* Affichage d'une classe *)
and string_of_class env i sub =
  i^"["^(
  List.fold_left (fun s t -> s^(string_of_typ env t)^", ") ""
    (List.map (subst_id sub) (get_tptc_id_list (try classe_lookup env i with
    | Not_found -> failwith ("c'est qui "^i^" ?")).cc_tptcs))
  )^"]"

(* context -> substitution -> unit *) 
let print_subst env s = 
  print_string "{";
  let print_cor id t =
    Printf.printf "%s -> %s; " id (string_of_typ env t)
  in Smap.iter print_cor s;
  print_endline "}"
