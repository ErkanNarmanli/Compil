open Tast
open Context
open Substitution
open Misc

exception TypeError of loc * string

(* Un type pour la position d'un type. *)
type postion = Pos | Neg | Neutre
let anti = function
  | Pos -> Neg
  | Neg -> Pos
  | Neutre -> Neutre


(* Indique si un type est issu d'un paramètre de type (le cas échéant)
 * covariant, contravariant ou sans variance. Renvoie "sans variance" si le type
 * est un vrai type.
 * covariant (+)     = Some (true, pos)
 * contravariant (-) = Some (false, pos)
 * sans variance     = None
 * lo est éventuellement la liste des paramètres de type dans laquelle chercher
 * la variance. Si lo = None, on devine cette liste à partir du type de 'this'.
 * Dans tous les cas, on rend cette liste, ça permet de ne la calculer qu'une
 * seule fois.
 * context -> (tparam_type_classe list) option -> typerType ->
 *    ((bool * loc) option * tparam_type_classe list) *)
let rec get_variance' env lo t = match lo with
  | Some tptcs ->
      let default = (None, tptcs) in
      let class_action (i, _) = 
        let rec aux = function
          | [] -> (None, tptcs)
          | tptc::q -> begin match tptc.tptc_cont with
                | TPTCplus tpt ->
                    if i = fst tpt.tpt_cont then
                      (Some (true, tpt.tpt_loc), tptcs)
                    else
                      aux q
                | TPTCmoins tpt ->
                    if i = fst tpt.tpt_cont then
                      (Some (false, tpt.tpt_loc), tptcs)
                    else
                      aux q
                | TPTCrien  tpt ->
                    if i = fst tpt.tpt_cont then
                      (None, tptcs)
                    else
                      aux q
              end in
        aux tptcs in
      map_class_def default class_action t
  | None ->
      let class_action (cid, _) = 
        let c = classe_lookup env cid in
        get_variance' env (Some c.cc_tptcs) t in
      begin try
        map_class_err
          "this ne peut être qu'une instance de classe."
          class_action
          (fst (var_lookup env "this"))
      with Not_found ->
        failwith ("On ne sait pas dans quelle classe aller chercher les "^
        "paramètres de type.")
      end

(* Version abrégée : on connait déjà les paramètres de type.
 * context -> tparam_type_classe list -> typerType -> bool option *)
let get_variance env tptcs t = fst (get_variance' env (Some tptcs) t)

(* Soulève une erreur si la variance et la position ne respectent pas les
 * règles.
 * (var, eloc) est le résulat de get_variance lorsque ce n'est pas None,
 * ident -> bool -> loc -> position -> unit *)
let variance_tptc var eloc = function
  | Pos ->
      if not var then
        raise (TypeError (eloc, "Ce type est contravariant et apparaît dans "^
        "une position positive."))
  | Neg ->
      if var then
        raise (TypeError (eloc, "Ce type est covariant et apparaît dans une "^
        "position négative."))
  | Neutre ->
      if var then
        raise (TypeError(eloc, "Cette variable de type est covariante et "^
        "covariante et apparaît dans un position neutre."))
      else
        raise (TypeError(eloc, "Cette variable de type est contravariante et "^
        "apparaît dans un position neutre."))

(* Vérifie qu'une classe en position positive ou négative vérifie les règles de
 * variance. Soulève une erreur si ce n'est pas le cas, ne rend rien sinon.
 * context -> (tparam_type_classe list) option -> position -> ident ->
   * substitution -> unit *)
let rec variance_classe env tptcs pos cid s = 
  let f tptc =
    let t = subst_id s (get_tptc_id tptc) in
    begin match tptc.tptc_cont with
      | TPTCplus _  ->
          variance_type env (Some tptcs) pos t
      | TPTCmoins _ ->
          variance_type env (Some tptcs) (anti pos) t
      | TPTCrien _  ->
          variance_type env (Some tptcs) Neutre t
    end
  in let c = classe_lookup env cid in
  List.iter f c.cc_tptcs

(* Indique si un type respecte les règles de variance.
 * context -> (tparam_type_classe list) option -> position -> typerType ->
   * unit. *)
and variance_type env lo pos t =
  let v, tptcs = get_variance' env lo t in
  match v with
    | None ->
        begin match t with
          | Tclasse (cid, s) ->
              variance_classe env tptcs pos cid s
          | _ -> ()
        end
    | Some (b, eloc) ->
        variance_tptc b eloc pos

(* Vérifie que les règles de variance sont vérifiées pour un objet de type tvar.
 * Soulève une erreur si ce n'est pas le cas, ne rend rien sinon.
 * context -> tvar -> unit *)
let variance_var env tv = match tv.tv_cont with
  | TVal (_, t, _) ->
      variance_type env None Pos t
  | TVar (_, t, _) ->
      variance_type env None Neutre t


