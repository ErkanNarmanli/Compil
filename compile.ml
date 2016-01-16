open X86_64
open Ast
open Tast
open Misc 
open Context

module Smap = Map.Make(String)

let (cfields : ((ident * ident), int) Hashtbl.t) = Hashtbl.create 17

let rec popn = function
  | 0 -> nop
  | n when n < 0 ->
      failwith "WTF"
  |n -> popq rsi ++ (popn (n-1))

let new_str_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    "str_"^(string_of_int !i))

let new_lazy_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    "lazy_"^(string_of_int !i))

let new_while_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    let i = string_of_int !i in
    "while_beg"^i, "while_end"^i)

let new_if_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    let i = string_of_int !i in
    "else_"^i, "end_if_"^i)

let lazyop = function
  | And | Or -> true
  | _ -> false

(* Produit le code compilant une expression. Le résulat est placé dans %rdi.
 * texpr -> text * data * int *)
let rec compile_expr env cid stack_size e = match e.te_cont with
  | TEvoid ->
      (* On rend 0. *)
      xorq (reg rdi) (reg rdi), nop
  | TEthis ->
      (* On cherche l'adresse de this et on la rend. *)
      let ofs = begin try
        Smap.find "this" env
      with
        | Not_found ->
            failwith "Impossible de localiser 'this'."
      end in
      movq (ind ~ofs:ofs rbp) (reg rdi), nop
  | TEnull ->
      (* On rend 0. *)
      xorq (reg rdi) (reg rdi), nop
  | TEint n ->
      (* On place la valeur immédiate n dans %rdi. *)
      movq (imm n) (reg rdi), nop
  | TEstr s ->
      (* Création d'une chaîne de caractère dans data. *)
      let strid = new_str_id () in 
      let data = label strid ++ string s in 
      let text =
        (* Instanciation de l'objet de la classe String. *)
        movq (imm 16) (reg rdi) ++
        call "malloc" ++
        movq (ilab "D_String") (reg rdx) ++ movq (reg rdx) (ind rax) ++
        movq (ilab strid) (reg rdx) ++ movq (reg rdx) (ind ~ofs:8 rax) ++
        (* On place le résultat dans %rdi. *)
        movq (reg rax) (reg rdi) in
      text, data
  | TEbool true ->
      movq (imm 1) (reg rdi), nop
  | TEbool false ->
      xorq (reg rdi) (reg rdi), nop
  | TEacc a ->
      begin match a.ta_cont with
        | TAident i ->
            (* On va chercher la position de la variable locale i sur la pile et
             * on place sa valeur dans %rdi. *)
            let ofs = begin try
              Smap.find i env
            with
              | Not_found ->
                  begin try
                    Hashtbl.find cfields (cid, i)
                  with
                    | Not_found ->
                        failwith (cid^"."^i^" inconnu.");
                  end
            end in
            movq (ind ~ofs:ofs rbp) (reg rdi), nop
        | TAexpr_ident (e, i) ->
            (* On compile l'objet de l'expression e puis on va chercher la
             * valeur de son champ i. *)
            let text, data = compile_expr env cid stack_size e in
            let cname = begin match e.te_typ with
              | Tclasse (cid, _) -> cid
              | _ -> failwith "WTF"
            end in
            let ofs = Hashtbl.find cfields (cname, i) in
            let text = text ++
              movq (ind ~ofs:ofs rdi) (reg rdi) in
            text, data
      end
  | TEacc_exp (a, e) ->
    let et, ed = compile_expr env cid stack_size e in
    begin match a.ta_cont with
        | TAident i ->
            let ofs = begin try 
              Smap.find i env
            with
              | Not_found ->
                  Hashtbl.find cfields (cid, i)
            end in
            et ++ movq (reg rdi) (ind ~ofs:ofs rbp), ed
        | TAexpr_ident (e, i) ->
            let text, data = compile_expr env cid (stack_size+8) e in
            let cname = begin match e.te_typ with
              | Tclasse (cid, _) -> cid
              | _ -> failwith "WTF"
            end in
            let ofs = Hashtbl.find cfields (cname, i) in
            let text = 
              pushq (reg rdi) ++ text ++
              popq rsi ++ movq (reg rsi) (ind ~ofs:ofs rdi) in
            et ++ text, ed ++ data
    end
  | TEacc_typ_exp (e, m, _, es) ->
      (* On compile l'expression qui appelle la méthode et on la met
      * sur la pile. *)
      let text, data = compile_expr env cid stack_size e in
      let text = text ++ pushq (reg rdi) in 
      (* On empile ensuite les arguments de la méthode. *)
      let text, data, tot = List.fold_left (fun (text, data, i) e ->
        let et, ed = compile_expr env cid (stack_size + 8*i) e in
        text ++ et ++ pushq (reg rdi), data ++ ed, i+1
        ) (text, data, 1) es in
      (* On appelle la méthode. *)
      let cname = begin match e.te_typ with
        | Tclasse (cid, _) -> cid
        | _ -> failwith "WTF"
      end in 
      let text = text ++ call ("M_"^cname^"_"^m) in
      (* On dépile les arguments et on place le résulat dans %rdi. *)
      let text = text ++ movq (reg rax) (reg rdi) ++ (popn tot) in
      text, data
  | TEnew (i, _, es) ->
      (* On place this sur la pile. *)
      let ofs = Smap.find "this" env in
      let text = 
        movq (ind ~ofs:ofs rbp) (reg rdi) ++ pushq (reg rdi) in
      (* On compile les arguments du constructeur. *)
      let comp_arg (text, data, i) e =
        let t, d = compile_expr env cid (stack_size+i*8) e in
        text ++ t ++ pushq (reg rdi), data ++ d, i+1 in
      let text, data, tot =
        List.fold_left comp_arg (text, nop, 1) es in
      (* On appelle le constructeur. *)
      let text = text ++ call ("M_"^i^"_new") in
      (* On dépile les arguments et le this. *)
      text ++ (popn tot) ++ movq (reg rax) (reg rdi), data
  | TEneg e ->
      (* Négation d'un booléen. *)
      let text, data = compile_expr env cid stack_size e in
      text ++ xorq (imm 1) (reg rdi), data
  | TEmoins e ->
      (* Opposé d'un entier. *)
      let text, data = compile_expr env cid stack_size e in
      text ++ negq (reg rdi), data
  | TEbinop (b, e1, e2) when not (lazyop b.tb_cont)->
      let code1, data1 = compile_expr env cid (stack_size+8) e1 in
      let code2, data2 = compile_expr env cid stack_size e2 in
      let op = begin match b.tb_cont with
        | Add -> addq (reg rsi) (reg rdi)
        | Sub -> subq (reg rsi) (reg rdi)
        | Mul -> imulq (reg rsi) (reg rdi)
        | Div ->
            movq (reg rdi) (reg rax) ++
            xorq (reg rdx) (reg rdx) ++
            idivq (reg rsi) ++
            movq (reg rax) (reg rdi)
        | Mod ->
            movq (reg rdi) (reg rax) ++
            xorq (reg rdx) (reg rdx) ++
            idivq (reg rsi) ++
            movq (reg rdx) (reg rdi)
        | Eq | EqRef ->
            cmpq (reg rsi) (reg rdi) ++
            sete (reg dil) ++
            movzbq (reg dil) rdi
        | Ne | NeRef ->
            cmpq (reg rsi) (reg rdi) ++
            setne (reg dil) ++
            movzbq (reg dil) rdi
        | Le -> 
            cmpq (reg rsi) (reg rdi) ++
            setle (reg dil) ++
            movzbq (reg dil) rdi
        | Ge -> 
            cmpq (reg rsi) (reg rdi) ++
            setge (reg dil) ++
            movzbq (reg dil) rdi
        | Lt -> 
            cmpq (reg rsi) (reg rdi) ++
            setl (reg dil) ++
            movzbq (reg dil) rdi
        | Gt -> 
            cmpq (reg rsi) (reg rdi) ++
            setg (reg dil) ++
            movzbq (reg dil) rdi
        | And | Or -> failwith "Ces opérateurs ne doivent pas être gérés ici."
      end in
      let code =
        code2 ++ pushq (reg rdi) ++
        code1 ++
        popq rsi ++ op in
      code, data1 ++ data2
  | TEbinop (b, e1, e2) ->
      let t1, d1 = compile_expr env cid stack_size e1 in
      let t2, d2 = compile_expr env cid stack_size e2 in
      let next = new_lazy_id () in
      let text = begin match b.tb_cont with
        | And ->
            t1 ++ cmpq (imm 0) (reg rdi) ++
            je next ++
            t2 ++ cmpq (imm 0) (reg rdi) ++
            setne (reg dil) ++ movzbq (reg dil) rdi ++
            label next
        | Or  ->
            t1 ++ cmpq (imm 0) (reg rdi) ++
            jne next ++
            t2 ++
            label next    
        | _ -> failwith "On ne traite ici que les opérateurs paresseux."
      end in
      text, d1 ++ d2
  | TEifelse (eb, e1, e2) ->
      let ct, cd = compile_expr env cid stack_size eb in
      let e1t, e1d = compile_expr env cid stack_size e1 in
      let e2t, e2d = compile_expr env cid stack_size e2 in
      let lelse, lend = new_if_id () in
      let code = ct ++ cmpq (imm 0) (reg rdi) ++ je lelse ++
        e1t ++ jmp lend ++
        label lelse ++ e2t ++ label lend in
      code, cd ++ e1d ++ e2d
  | TEwhile (cond, e) ->
      let lbeg, lend = new_while_id () in
      let ctext, cdata = compile_expr env cid stack_size cond in
      let etext, edata = compile_expr env cid stack_size e in
      let code = 
        label lbeg ++ ctext ++ cmpq (imm 0) (reg rdi) ++ je lend ++
        etext ++ jmp lbeg ++ label lend in
      code, cdata ++ edata
  | TEreturn res ->
      let text, data = begin match res with
        | None ->
            xorq (reg rax) (reg rax), nop
        | Some e ->
            let text, data = compile_expr env cid stack_size e in
            text ++ movq (reg rdi) (reg rax), data
      end in
      let text = text ++
        movq (reg rbp) (reg rsp) ++
        popq rbp in 
      text ++ ret, data
  | TEprint e ->
      let code, data = compile_expr env cid stack_size e in
      code ++ begin match e.te_typ with
        | Tint -> call "print_int"
        | Tstring ->
            movq (ind ~ofs:8 rdi) (reg rdi) ++ 
            xorq (reg rax) (reg rax) ++
            call "printf"
        | _ -> failwith "Le typeur aurait dû refuser ce programme."
      end, data
  | TEbloc b ->
      let text, data, _, new_stack  =
        List.fold_left (compile_instr cid) (nop, nop, env, stack_size) b in
      text ++ popn ((new_stack - stack_size)/8), data

and compile_instr cid (text, data, env, stack_size) = function
  | TIvar v ->
      let t, d, env = compile_var env cid stack_size v in
      (text ++ t, data ++ d, env, stack_size+8)
  | TIexpr e ->
      let t, d = compile_expr env cid stack_size e in
      (text ++ t, data ++ d, env, stack_size)

and compile_var env cid stack_size v =
  let i, e =  match v.tv_cont with
    | TVar(i, _, e) -> i, e
    | TVal(i, _, e) -> i, e in
  let text, data = compile_expr env cid stack_size e in
  let text = text ++ pushq (reg rdi) in
  let stack_size = stack_size + 8 in
  text, data, Smap.add i (-stack_size) env

let compile_meth cid (text, data) = function
  | TDvar _ -> (text, data)
  | TDmeth m ->
      (* L'étiquette de la méthode. *)
      let text = text ++ label ("M_"^cid^"_"^m.tm_name) in
      (* On sauvegarde %rbp *)
      let text = text ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) in
      (* Construction de l'environnement pour les variables sur la pile. *)
      let len = List.length m.tm_params in
      let env, _ = List.fold_left (fun (env, i) p ->
        Smap.add p.tp_name (8*(len-i+1)) env, i+1
        ) (Smap.singleton "this" (8*(len+2)), 0) m.tm_params in
      (* Compilation du corps de la méthode. *)
      let et, ed = compile_expr env cid 0 m.tm_res_expr in
      let text, data = text ++ et ++ movq (reg rdi) (reg rax), data ++ ed in
      (* On restore %rbp et %rsp. *)
      let text = text ++
        movq (reg rbp) (reg rsp) ++
        popq rbp in
      text ++ ret, data

let rec get_meth_owner env m c =
  if m.tm_override then
    c.cc_name
  else match c.cc_deriv with
    | None -> c.cc_name
    | Some (Tclasse (cid, _), _) ->
        let c' = begin try 
            classe_lookup env cid
        with
          | Not_found -> failwith
          "Echec de la recherche de la classe parente."
        end in
        get_meth_owner env m c'
    | Some _ ->
        failwith "Pas possible."

let main_code =
  glabel "main" ++
  movq (reg rsp) (reg rbp) ++
  call "M_Main_new" ++
  (* Appel de la méthode main. *)
  pushq (reg rax) ++ pushq (imm 0) ++
  call "M_Main_main" ++
  popq rdi ++ popq rdi ++
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

let make_constr c =
  (* L'étiquette de la méthode. *)
  let text = label ("M_"^c.tc_name^"_new") in
  (* On sauvegarde %rbp. *)
  let text = text ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) in
  (* On alloue la place sur le tas. *)
  let text = text ++
    movq (imm (8*(List.length c.tc_env.vars))) (reg rdi) ++
    call "malloc" ++ movq (reg rax) (reg rbx) in
  (* On remplit les champs de l'objet. *)
  let len = List.length c.tparams in
  let text = text ++ 
    (* this. *)
    pushq (reg rbx) ++ 
    (* Étiquette du descripteur du classe. *)
    movq (ilab ("D_"^c.tc_name)) (reg rdx) ++ movq (reg rdx) (ind rbx)  in
  let text, env, _ = List.fold_left (fun (t, env, i) p ->
    (* Arguments du constructeur. *)
    let ofs = Hashtbl.find cfields (c.tc_name, p.tp_name) in
    t ++ movq (ind ~ofs:(8*(len-i+1)) rbp) (reg rdi) ++
    movq (reg rdi) (ind ~ofs:ofs rbx),
    Smap.add p.tp_name (8*(len-i+1)) env,
    i+1
    ) (text, Smap.singleton "this" (-8), 0) c.tparams in
    (* Variables du corps de la classe. *)
  let text, data = List.fold_left (fun (t, d) decl -> match decl with 
      | TDvar v ->
          let i, e = begin match v.tv_cont with
            | TVar (i, _, e) -> i, e
            | TVal (i, _, e) -> i, e
          end in
          let vt, vd = compile_expr env c.tc_name 8 e in
          let ofs = Hashtbl.find cfields (c.tc_name, i) in
          let text = t ++ vt ++ movq (reg rdi) (ind ~ofs:ofs rbx) in
          text, d ++ vd
      | TDmeth _  -> (t, d)
    ) (text, nop) c.tdecls in
  (* On place this dans %rax et on restore %rbp et %rsp. *)
  let text = text ++
    movq (reg rbx) (reg rax) ++
    movq (reg rbp) (reg rsp) ++
    popq rbp in 
  text ++ ret, data


let compile_class (t, d) c =
  (* Ajout des positions des champs de la classe dans la table. *)
  let pos = ref 0 in
  List.iter (fun v ->
      incr pos;
      Hashtbl.add cfields (c.tc_name, get_cv_id v) (8 * (!pos))
      ) c.tc_env.vars; 
  (* Nom de la classe et de la classe mère. *)
  let head = begin
    label ("D_"^c.tc_name) ++
    match c.tderiv with
      | None ->
          address ["D_AnyRef"]
      | Some (Tclasse (cid, _), _) -> 
          address ["D_"^cid]
      | _ -> 
          failwith "On n'arrive pas là."
  end in
  (* Étiquettes des méthodes *)
  let meths = 
    List.fold_left (fun ms m ->
        let c' = begin try
          classe_lookup c.tc_env c.tc_name
        with
          | Not_found ->
              failwith "Echec de l'étiquetage d'une méthode."
        end in
        let cname = get_meth_owner c.tc_env m c' in
        ms ++ address ["M_"^cname^"_"^m.tm_name]
        ) nop c.tc_env.meths in
  (* Compilation du corps de la classe. *)
  let new_t, new_d = make_constr c in
  let meths_t, meths_d =
    List.fold_left (compile_meth c.tc_name) (nop, nop) c.tdecls in
  (t ++ new_t ++ meths_t, d ++ head ++ meths ++ new_d ++ meths_d)

let compile_classM (t, d) cm = 
  compile_class (t, d) {
    tc_name             = "Main";
    ttype_class_params  = [];
    tparams             = [];
    tderiv              = None;
    tdecls              = cm.tcM_cont;
    tc_loc              = cm.tcM_loc;
    tc_env              = cm.tcM_env
  }

let compile_fichier f ofile =
  (* let text, data = make_env_code f.f_env in *)
  let text, data = List.fold_left compile_class (nop, nop) f.tclasses in 
  let text, data = compile_classM (text, data) f.tmain in
  let prog = {
    text = 
      (* main *)
      main_code ++
      (* Le reste du code *)
      text ++
      print_int_code;
    data = data ++
      builtin_classes ++ 
      label ".Sprint_int" ++ string "%d\n";
  } in
  print_in_file ~file:ofile prog



