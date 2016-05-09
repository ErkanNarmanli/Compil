open X86_64
open Ast
open Tast
open Misc 
open Context

module Smap = Map.Make(String)

(* Stocke les positions des champs des objets. *)
let (cfields : ((ident * ident), int) Hashtbl.t) = Hashtbl.create 17

(* Stocke les positions des étiquettes des méthodes. *)
let (cmeths : ((ident * ident), int) Hashtbl.t) = Hashtbl.create 17 

(* Stocke les étiquettes des méthodes, à une classe et un nom de méthode, on
 * associe une étiquette unique. *)
let (mlabs : ((ident * ident), string) Hashtbl.t) = Hashtbl.create 17

(* L'environnement de la classe qu'on est en train de compiler (gamma'). *)
let cur_class_env = ref (env0 ())

(***
 * Fonctions attribuant des étiquettes uniques à différentes structures
 * (if/while/méthodes...)
 ***)

(* Chaînes de caractères. *)
let new_str_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    "str_"^(string_of_int !i))

(* Opérations paresseuses. *)
let new_lazy_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    "lazy_"^(string_of_int !i))

(* Boucles while. *)
let new_while_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    let i = string_of_int !i in
    "while_beg"^i, "while_end"^i)

(* Structures conditionnelles. *)
let new_if_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    let i = string_of_int !i in
    "else_"^i, "end_if_"^i)

(* Méthodes. *)
let new_meth_id = 
  let i = ref (-1) in
  (fun () ->
    incr i;
    "M_"^(string_of_int !i)) 

(***
 * Autres fonctions utiles.
 ***)

(* Indique si un opérateur binaire doit avoir un comportement paresseux.
 * Ast.binop -> bool *)
let lazyop = function
  | And | Or -> true
  | _ -> false

(* Renvoie le code assembleur dépilant n valeurs.
 * int -> text *)
let rec popn = function
  | 0 -> nop
  | n when n < 0 ->
      failwith "On ne dépile pas un nombre négatif de valeurs."
  |n -> popq rsi ++ (popn (n-1))

(* Indique le nom de la méthode ayant défini un méthode pour la dernière fois.
 * context -> tmethode -> context_classe -> ident *)
let rec get_meth_owner env m c =
  if m.tm_override then
    c.cc_name
  else match c.cc_deriv with
    | None -> c.cc_name
    | Some (Tclasse (cid, _), _) ->
        let c'= begin try 
          classe_lookup env cid
        with
          | Not_found -> failwith
              "Echec de la recherche de la classe parente."
        end in
        begin try
          let m' = meth_lookup c'.cc_env m.tm_name in
          get_meth_owner c'.cc_env m' c'
        with
          | Not_found -> c.cc_name
        end
    | Some _ ->
        failwith ("La classe "^c.cc_name^" hérite d'un type builtin, le "^
        "typeur est malade.")

(* Cherche la position de l'étiquette d'une méthode dans la description d'une
 * classe.
 * ident -> ident -> int *)
let rec get_mofs cname mid = 
  try
    Hashtbl.find cmeths (cname, mid)
  with
    | Not_found ->
        let c = begin try
          classe_lookup !cur_class_env cname
        with
          | Not_found ->
              failwith "Classe inconnue..."
        end in
        begin match c.cc_deriv with
          | None -> raise Not_found
          | Some (Tclasse (cid, _), _) ->
              get_mofs cid mid
          | _ ->
              failwith "On hérite d'un type builtin..."
        end

(* Calcule la hauteur de la pile lors de l'appel au constructeur d'une classe.
 * context_classe -> int *)
let rec extends_nb_aux c = match c.cc_deriv with
  | None -> 1
  | Some (Tclasse (cid, _), _) ->
      let c = classe_lookup c.cc_env cid in
      1 + extends_nb_aux c
  | Some _ -> failwith "Non mais sérieux."

(* Idem pour les tclasses. *)
let extends_nb c =
  let c = classe_lookup c.tc_env c.tc_name in
  extends_nb_aux c



(***
 * Production de code.
 ***)


(* Produit le code compilant une expression.
   * env donne la postion des variables locales par rapport à %rbp sur la pile.
   * cid est l'identifiant de la classe qu'on est en train de compiler.
   * stack_size est la hauteur de la pile à partir de %rbp.
 * Le résulat est placé dans %rdi.
 * À la sortie, la pile est à la même heuteur qu'à l'entrée.
 * int Smap.t -> ident -> int -> texpr -> text * data *)
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
      comment "this" ++
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
        movq (ilab "D_String") (ind rax) ++
        movq (ilab strid) (ind ~ofs:8 rax) ++
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
            let text = begin try
              let ofs = Smap.find i env in
              movq (ind ~ofs:ofs rbp) (reg rdi)
            with
              | Not_found ->
                  (* Si on ne trouve pas, c'est que i désigne this.i *)
                  begin try
                    let ofs = Hashtbl.find cfields (cid, i) in
                    let this = Smap.find "this" env in
                    movq (ind ~ofs:this rbp) (reg rsi) ++
                    movq (ind ~ofs:ofs rsi) (reg rdi)
                  with
                    | Not_found ->
                        failwith (cid^"."^i^" inconnu.");
                  end
            end in
            text, nop
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
      (* On commence par compiler l'expression à droite du signe = *)
      let et, ed = compile_expr env cid stack_size e in
      begin match a.ta_cont with
          | TAident i ->
              (* Premier cas : i désigne une variable locale. *)
              let text = begin try 
                let ofs = Smap.find i env in
                et ++ movq (reg rdi) (ind ~ofs:ofs rbp)
              with
                | Not_found ->
                    (* Si ce n'est pas le cas, c'est this.i *)
                    let this = Smap.find "this" env in
                    let ofs = Hashtbl.find cfields (cid, i) in
                    et ++ movq (ind ~ofs:this rbp) (reg rsi) ++ 
                    movq (reg rdi) (ind ~ofs:ofs rsi)
              end in
              (* On a mis la valeur de l'expression à la bonne adresse avec
               * succès, on rend 0. *)
              text ++ xorq (reg rdi) (reg rdi), ed
          | TAexpr_ident (e, i) ->
              (* On compile l'expression dont on modifie un champ et on place la
               * valeur calculée plus haut à la bonne adresse. *)
              let text, data = compile_expr env cid (stack_size+8) e in
              let cname = begin match e.te_typ with
                | Tclasse (cid, _) -> cid
                | _ -> failwith
                    "Seules les classes de l'utilisateur ont des champs."
              end in
              let ofs = Hashtbl.find cfields (cname, i) in
              let text = 
                pushq (reg rdi) ++ text ++
                popq rsi ++ movq (reg rsi) (ind ~ofs:ofs rdi) in
              et ++ text, ed ++ data
      end
  | TEacc_typ_exp (e, m, _, es) ->
      (* e.m[...](e1, e2, ... = es)  *)
      (* On compile l'expression qui appelle la méthode et on la met
       * sur la pile (c'est le this dans la méthode). *)
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
        | _ -> failwith
            "Seules les classes de l'utilisateur ont des méthodes."
      end in 
      let mofs = begin try
        get_mofs cname m
      with
        | Not_found ->
            failwith ("La méthode "^m^" de "^cname^" est inconnue.")
      end in
      let text = text ++
        movq (ind ~ofs:(-(stack_size+8)) rbp) (reg rbx) ++
        movq (ind rbx) (reg r8) ++
        call_star (ind ~ofs:mofs r8) in
      (* On dépile les arguments et on place le résulat dans %rdi. *)
      let text = text ++ movq (reg rax) (reg rdi) ++ (popn tot) in
      text, data
  | TEnew (cid', _, es) ->
      (* On alloue la place nécessaire sur le tas. Il suffit de compter le
       * nombre de bindings associés à la classe en question dans la tables des
       * champs. *)
      let len = ref 1 in
      Hashtbl.iter (fun (cid, _) _ -> if cid = cid' then incr len) cfields;
      let text =
        movq (imm (8*(!len))) (reg rdi) ++
        call "malloc" in
      (* Étiquette du descripteur du classe. *)
      let text = text ++ movq (ilab ("D_"^cid')) (ind rax) in
      (* On sauvegarde le pointeur vers l'objet sur la pile. *)
      let text = text ++ pushq (reg rax) in
      (* On compile les arguments du constructeur et on place leurs valeurs sur
       * le tas. *)
      let c = classe_lookup !cur_class_env cid' in
      let comp_arg (text, data, i) e =
        let arg = (List.nth c.cc_params i).tp_name in
        let ofs = Hashtbl.find cfields (cid', arg) in
        let t, d = compile_expr env cid (stack_size+8) e in
        let text = text ++ t ++
          movq (ind ~ofs:(-stack_size-8) rbp) (reg rbx) ++
          movq (reg rdi) (ind ~ofs:ofs rbx) in
        text, data ++ d, i+1 in
      let text, data, _ =
        List.fold_left comp_arg (text, nop, 0) es in
      (* On appelle le constructeur. *)
      let text = text ++
        popq rbx ++
        movq (reg rbx) (reg rdi) ++ call ("C_"^cid') in
      (* On place l'adresse de l'objet dans %rdi. *)
      text ++ movq (reg rax) (reg rdi), data
  | TEneg e ->
      (* Négation d'un booléen. *)
      let text, data = compile_expr env cid stack_size e in
      text ++ xorq (imm 1) (reg rdi), data
  | TEmoins e ->
      (* Opposé d'un entier. *)
      let text, data = compile_expr env cid stack_size e in
      text ++ negq (reg rdi), data
  | TEbinop (b, e1, e2) when not (lazyop b.tb_cont)-> (* Cas non paresseux. *)
      (* On compile d'abord la deuxième expression pour calculer directement le
       * résultat dans %rdi. *)
      let code2, data2 = compile_expr env cid stack_size e2 in
      let code1, data1 = compile_expr env cid (stack_size+8) e1 in
      let op = begin match b.tb_cont with
        | Add -> addq (reg rsi) (reg rdi)
        | Sub -> subq (reg rsi) (reg rdi)
        | Mul -> imulq (reg rsi) (reg rdi)
        | Div ->
            movq (reg rdi) (reg rax) ++
            cqto ++
            idivq (reg rsi) ++
            movq (reg rax) (reg rdi)
        | Mod ->
            movq (reg rdi) (reg rax) ++
            cqto ++
            idivq (reg rsi) ++
            movq (reg rdx) (reg rdi)
        | Eq | EqRef ->
            (* 'eq' correspiond à l'égalité des pointeurs, on le compile donc
             * comme un '==' *)
            cmpq (reg rsi) (reg rdi) ++
            sete (reg dil) ++
            movzbq (reg dil) rdi
        | Ne | NeRef ->
            (* Cf. remarque ci dessus. *)
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
      (* On compile les expressions à comparer. Dans un cas, le code de la
       * deuxième ne sera jamais exécuté. *)
      let t1, d1 = compile_expr env cid stack_size e1 in
      let t2, d2 = compile_expr env cid stack_size e2 in
      (* On  besoin d'une étiquette. *)
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
      (* On compile la condition, l'expression du then et celle du else. *)
      let ct, cd = compile_expr env cid stack_size eb in
      let e1t, e1d = compile_expr env cid stack_size e1 in
      let e2t, e2d = compile_expr env cid stack_size e2 in
      (* on a besoin d'une étiquette. *)
      let lelse, lend = new_if_id () in
      let code =
        comment "if" ++ ct ++
        cmpq (imm 0) (reg rdi) ++ je lelse ++
        comment "then" ++ e1t ++ jmp lend ++
        comment "else" ++ label lelse ++ e2t ++ label lend in
      code, cd ++ e1d ++ e2d
  | TEwhile (cond, e) ->
      (* Étiquette et compilation de la condition et du code. *)
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
            (* 'return' tout seul -> on rend 0 pour () *)
            xorq (reg rax) (reg rax), nop
        | Some e ->
            (* On place la valeur de 'e' dans %rax. *)
            let text, data = compile_expr env cid stack_size e in
            text ++ movq (reg rdi) (reg rax), data
      end in
      (* On restaure %rbp et %rbx. On restaure le sommet de la pile.
       * On signale la fin de la méthode par un ret. *)
      let text = text ++
        movq (ind ~ofs:(-8) rbp) (reg rbx) ++
        movq (reg rbp) (reg rsp) ++
        popq rbp in 
      text ++ ret, data
  | TEprint e ->
      (* On compile l'expression à afficher. *)
      let text, data = compile_expr env cid stack_size e in
      let text = text ++
      begin match e.te_typ with
        | Tint -> call "print_int"
        | Tstring ->
            (* Dans le cas d'une chaîne de caractères, on va chercher
             * l'étiquette de la chaîne dans la chaîne instanciée sur le tas. *)
            movq (ind ~ofs:8 rdi) (reg rdi) ++ 
            xorq (reg rax) (reg rax) ++
            call "printf"
        | _ ->
            failwith ("On ne peut imprimer autre chose qu'une chaîne de "^
            "caractères ou un entier.")
      end in
      text ++ xorq (reg rdi) (reg rdi) , data
  | TEbloc b ->
      (* On compile toutes les instructions du bloc. *)
      let text, data, _, new_stack  =
        List.fold_left (compile_instr cid) (nop, nop, env, stack_size) b in
      (* On dépile les variables locales. *)
      text ++ popn ((new_stack - stack_size)/8), data

(* Compile une instruction d'un bloc.
 * ident -> (text * data * int Smap.t * int) -> instruction ->
   * (text * data * int Smap.t * int) *)
and compile_instr cid (text, data, env, stack_size) = function
  | TIvar v ->
      let t, d, env = compile_var env cid stack_size v in
      (text ++ t, data ++ d, env, stack_size+8)
  | TIexpr e ->
      let t, d = compile_expr env cid stack_size e in
      (text ++ t, data ++ d, env, stack_size)

(* Compile une variable locale et place le résultat sur la pile.
 * int Smap.t -> ident -> int -> tvar -> (text * data * int Smap.t) *)
and compile_var env cid stack_size v =
  let i, e =  match v.tv_cont with
    | TVar(i, _, e) -> i, e
    | TVal(i, _, e) -> i, e in
  let text, data = compile_expr env cid stack_size e in
  let text = text ++
    pushq (reg rdi) ++ xorq (reg rdi) (reg rdi) in
  let stack_size = stack_size + 8 in
  text, data, Smap.add i (-stack_size) env

(* Écrit le code d'une méthode.
 * ident -> (text * data) -> tdecl -> (text * data) *)
let compile_meth cid (text, data) = function
  | TDvar _ -> (text, data)
  | TDmeth m ->
      (* L'étiquette de la méthode. *)
      let text = text ++ label (Hashtbl.find mlabs (cid, m.tm_name)) in
      (* On sauvegarde %rbp et %rbx. *)
      let text = text ++
        pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
        pushq (reg rbx) in
      (* Construction de l'environnement pour les variables sur la pile. *)
      let len = List.length m.tm_params in
      let env, _ = List.fold_left (fun (env, i) p ->
        Smap.add p.tp_name (8*(len-i+1)) env, i+1
        ) (Smap.singleton "this" (8*(len+2)), 0) m.tm_params in
      (* Compilation du corps de la méthode. *)
      let et, ed = compile_expr env cid 8 m.tm_res_expr in
      let text, data = text ++ et ++ movq (reg rdi) (reg rax), data ++ ed in
      (* On restore %rbp, %rsp et %rbx. *)
      let text = text ++
        movq (ind ~ofs:(-8) rbp) (reg rbx) ++
        movq (reg rbp) (reg rsp) ++
        popq rbp in
      text ++ ret, data

(* Écrit le code d'un constructeur.
 * tclasses -> (text * data) *)
let make_constr c =
  (* L'étiquette de la méthode. *)
  let text = label ("C_"^c.tc_name) in
  (* Petite optimisation utile pour le debug : quand le constructeur ne fait
   * rien, il ne fait rien. *)
  let empty = ref true in 
  Hashtbl.iter (fun (id, x) _ -> if
    id = c.tc_name && x <> "this" then empty := false)
    cfields;
  if !empty && is_none c.tderiv then
    text ++ movq (reg rdi) (reg rax) ++ ret, nop
  else begin
    (* On sauvegarde %rbp et %rbx.
    * On place l'adresse de l'objet sur la pile. *)
    let text = text ++
      pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
      pushq (reg rbx) ++
      pushq (reg rdi) in
    (* On appelle le constructeur de la superclasse. *)
    let text, data = match c.tderiv with
      | None -> text, nop
      | Some (Tclasse (cid, _), es) ->
          (* À expliquer peut-être un jour dans un futur lointain. *)
          let stack_size = 8*(extends_nb c) in
          (* On cherche la classe mère. *)
          let c' = classe_lookup c.tc_env cid in
          (* Un fonction qui compile les arguments du constructeur de la
           * superclasse et ls ajoute à leur place sur le tas. *)
          let comp_arg (text, data, i) e =
            let arg = (List.nth c'.cc_params i).tp_name in
            let ofs = Hashtbl.find cfields (cid, arg) in 
            let t, d = compile_expr (Smap.singleton "this" (-stack_size))
              c.tc_name stack_size e in
            let text = text ++ t ++
              (* Le this. *)
              movq (ind ~ofs:(-16) rbp) (reg rbx) ++
              movq (reg rdi) (ind ~ofs:ofs rbx) in
            text, data ++ d, i+1 in
          let text, data, _ =
            List.fold_left comp_arg (text, nop, 0) es in
          let text = text ++
            movq (ind ~ofs:(-16) rbp) (reg rdi) ++
            call ("C_"^cid) in
          text, data
      | _ ->
          failwith "On ne peut hériter d'un type builtin."
    in
    (* Variables du corps de la classe. *)
    let text, data = List.fold_right (fun decl (t, d) -> match decl with 
        | TDvar v ->
            let i, e = begin match v.tv_cont with
              | TVar (i, _, e) -> i, e
              | TVal (i, _, e) -> i, e
            end in
            let vt, vd =
              compile_expr (Smap.singleton "this" (-16)) c.tc_name 16 e in
            let ofs = Hashtbl.find cfields (c.tc_name, i) in
            let text = t ++ vt ++
              movq (ind ~ofs:(-16) rbp) (reg rbx) ++
              movq (reg rdi) (ind ~ofs:ofs rbx) in
            text, d ++ vd
        | TDmeth _  -> (t, d)
      ) c.tdecls (text, data) in
    (* On restore %rbp et %rsp, on place l'adresse de l'objet dans %rax. *)
    let text = text ++
      movq (reg rbx) (reg rax) ++
      movq (ind ~ofs:(-8) rbp) (reg rbx) ++
      movq (reg rbp) (reg rsp) ++
      popq rbp in 
    text ++ ret, data
  end

(* Compile une classe.
 * (text * data) -> tclasse -> (text * data) *)
let compile_class (t, d) c =
  (* On met tout de suite l'environnement local de la classe dans cur_class_env.
   *)
  cur_class_env := c.tc_env;
  (* Ajout des positions des champs de la classe dans la table. *)
  let pos = ref 0 in
  List.iter (fun v ->
    let vid = get_cv_id v in
    if not (vid = "this") then begin
      incr pos;
      Hashtbl.add cfields (c.tc_name, vid) (8 * (!pos))
    end) (List.rev c.tc_env.vars); 
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
  let meths, _ = 
    List.fold_right (fun m (ms, i) ->
        let c' = begin try
          classe_lookup c.tc_env c.tc_name
        with
          | Not_found ->
              failwith "Echec de l'étiquetage d'une méthode."
        end in
        let cname = get_meth_owner c.tc_env m c' in
        let mname = if cname = c.tc_name then begin
          let id = new_meth_id () in
          Hashtbl.add mlabs (c.tc_name, m.tm_name) id;
          id
        end else
          Hashtbl.find mlabs (cname, m.tm_name)
        in
        Hashtbl.add cmeths (c.tc_name, m.tm_name) i;
        ms ++ address [mname], i+8
        ) c.tc_env.meths (nop, 8) in
  (* Compilation du corps de la classe. *)
  let new_t, new_d = make_constr c in
  let meths_t, meths_d =
    List.fold_left (compile_meth c.tc_name) (nop, nop) c.tdecls in
  (t ++ new_t ++ meths_t, d ++ head ++ meths ++ new_d ++ meths_d)

(* Compilation de la classe Main. *)
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

(* Génère le code de la fonction 'main' de l'assembleur.
 * unit -> text *)
let main_code () =
  let len = ref 1 in
  Hashtbl.iter (fun (cid, _) _ -> if cid = "Main" then incr len) cfields;
  (* label 'main' *)
  glabel "main" ++ movq (reg rsp) (reg rbp) ++
  (* On alloue de la mémoire sur le tas. *)
  movq (imm (8*(!len))) (reg rdi) ++
  call "malloc" ++ movq (reg rax) (reg rdi) ++
  (* On met l'étiquette du descripteur de classe. *)
  movq (ilab "D_Main") (ind rdi) ++
  (* On appelle le constructeur de Main. *)
  call "C_Main" ++
  (* On appelle la méthode main. *)
  pushq (reg rax) ++ pushq (imm 0) ++
  call (begin try Hashtbl.find mlabs ("Main", "main") with | Not_found ->
    failwith "Lolilol" end) ++
  popq rdi ++ popq rdi ++
  xorq (reg rax) (reg rax) ++
  ret

(* Compile le programme complet.
 * tfichier -> string -> unit *)
let compile_fichier f ofile =
  let text, data = List.fold_left compile_class (nop, nop) f.tclasses in 
  let text, data = compile_classM (text, data) f.tmain in
  let prog = {
    text = 
      (* main *)
      main_code () ++
      (* Le reste du code *)
      text ++
      (* Fonction print_int *)
      label "print_int" ++
      movq (reg rdi) (reg rsi) ++
      movq (ilab ".Sprint_int") (reg rdi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++
      ret;
    data = data ++
      (* Classes builtin *)
      label "D_Any" ++
      address ["D_Any"] ++ 
      label "D_AnyRef" ++
      address ["D_Any"] ++
      label "D_String" ++
      address ["D_AnyRef"] ++
      (* Chaîne pour la fonction print_int. *)
      label ".Sprint_int" ++ string "%d";
  } in
  print_in_file ~file:ofile prog


