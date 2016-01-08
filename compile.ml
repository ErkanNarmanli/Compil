open X86_64
open Tast
open Misc 
open Context

module Smap = Map.Make(String)

let (cfields : ((ident * ident), int) Hashtbl.t) = Hashtbl.create 17
let (cmeths  : ((ident * ident), int) Hashtbl.t) = Hashtbl.create 17

let new_str_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    ("str_"^(string_of_int !i)))

let rec compile_expr e = match e.te_cont with
  | TEvoid ->
      movq (imm 0) (reg rdi), nop
  | TEthis ->
      assert false
  | TEnull ->
      assert false
  | TEint n ->
      movq (imm n) (reg rdi), nop
  | TEstr s ->
      (* Création d'une chaîne de caractère dans data. *)
      let strid = new_str_id () in 
      let data = label strid ++ string s in 
      let text = 
        (* Instanciation de l'objet de la classe String. *)
        movq (imm 16) (reg rdi) ++
        call "malloc" ++
        movq (ilab "D_String") (ind rax) ++
        movq (ilab strid) (ind ~ofs:8 rax) ++
        (* On place le résultat dans %rdi. *)
        movq (reg rax) (reg rdi) in
      text, data
  | TEbool true ->
      movq (imm 1) (reg rdi), nop
  | TEbool false ->
      movq (imm 0) (reg rdi), nop
  | TEacc a ->
      assert false
  | TEacc_exp (a, e) ->
      assert false
  | TEacc_typ_exp (e, i, at, es) ->
      assert false
  | TEnew (i, at, es) ->
      assert false
  | TEneg e ->
      let text, data = compile_expr e in
      let text = text ++ notq (reg rdi) in
      text, data
  | TEmoins e ->
      let text, data = compile_expr e in
      let text = text ++ negq (reg rdi) in
      text, data
  | TEbinop (b, e1, e2) ->
      assert false
  | TEifelse (eb, e1, e2) ->
      assert false
  | TEwhile (cond, e) ->
      assert false
  | TEreturn None ->
      movq (reg rdi) (reg rax) ++ ret, nop
  | TEreturn (Some e) ->
      (* TODO : Pas sûr à 100% *)
      let text, data = compile_expr e in
      text ++ movq (reg rdi) (reg rax) ++ ret, data
  | TEprint e ->
      let code, data = compile_expr e in
      code ++ begin match e.te_typ with
        | Tint -> call "print_int"
        | Tstring ->
            movq (ind ~ofs:8 rdi) (reg rdi) ++ 
            call "printf"
      end, data
  | TEbloc b ->
      let compile_decl (text, data) ins =
        let t, d = begin match ins with
          | TIvar v -> compile_var v
          | TIexpr e -> compile_expr e
        end in
        text ++ t, data ++ d
      in List.fold_left compile_decl (nop, nop) b

and compile_var _ = assert false

let rec get_meth_owner env m c =
  if m.tm_override then
    c.cc_name
  else match c.cc_deriv with
    | None -> c.cc_name
    | Some (Tclasse (cid, _), _) ->
        let c' = classe_lookup env cid in
        get_meth_owner env m c'
    | Some _ ->
        failwith "Pas possible."

let make_meth_desc env c (text, data) m = 
  let text', data' = compile_expr m.tm_res_expr in
  text ++ label ("M_"^(get_meth_owner env m c)^"_"^m.tm_name) ++
  text' ++ ret, data ++ data'

let make_class_desc env c = 
  (* Ajout des positions des champs de la classe dans la table. *)
  let pos = ref 0 in
  List.iter (fun v ->
      incr pos;
      Hashtbl.add cfields (c.cc_name, get_cv_id v) (8 * (!pos))
      ) c.cc_env.vars; 
  (* Nom de la classe et de la classe mère. *)
  let head = begin
    label ("D_"^c.cc_name) ++
    match c.cc_deriv with
      | None ->
          address ["D_AnyRef"]
      | Some (Tclasse (cid, _), _) -> 
          address ["D_"^cid]
      | _ -> 
          failwith "On n'arrive pas là."
  end in
  let meths = 
    List.fold_left (fun ms m ->
        let cname = get_meth_owner env m c in
        ms ++ address ["M_"^cname^"_"^m.tm_name]
        ) nop c.cc_env.meths in
  head ++ meths

let make_env_code env = 
  let one_class (text, data) c =
    let methst, methsd =
      List.fold_left (make_meth_desc env c) (nop, nop) c.cc_env.meths in 
    text ++ methst, data ++ (make_class_desc env c) ++ methsd in
  List.fold_left one_class (nop, nop) env.classes

let main_code m =
  let space = (List.length m.tcM_env.vars)*8 in
  glabel "main" ++
  movq (imm space) (reg rdi) ++
  call "malloc" ++
  movq (ilab "D_Main") (ind rax) ++
  (* FIXME : il peut y avoir des vars *)
  call "M_Main_main" ++
  xorq (reg rax) (reg rax) ++
  ret

let builtin_classes =
  label "D_Any" ++
  address ["D_Any"] ++ 
  label "D_AnyRef" ++
  address ["D_Any"] ++
  label "D_String" ++
  address ["D_AnyRef"]

let print_int_code = 
  label "print_int" ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab ".Sprint_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  ret

let compile_fichier f ofile =
  let text, data = make_env_code f.f_env in
  let prog = {
    text = 
      (* main *)
      (main_code f.tmain) ++
      (* Le reste du code *)
      text ++
      print_int_code;
    data = data ++
      builtin_classes ++ 
      label ".Sprint_int" ++ string "%d\n";
  } in
  print_in_file ~file:ofile prog



