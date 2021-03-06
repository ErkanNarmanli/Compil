open Ast

let print_ident = print_string

(* ('a -> unit) -> string -> 'a list -> unit *)
let rec print_int_list func sep = function
    | []    -> ()
    | [a]   -> func a
    | a::q  -> func a ; print_string sep ; print_int_list func sep q

(* ('a -> unit) -> string -> string -> string -> 'a list -> unit *)
let print_list func deb sep fin xs =
    print_string deb ;
    print_int_list func sep xs ;
    print_string fin

(* ('a -> unit) -> 'a option -> unit *)
let print_o func = function
    | None      -> ()
    | Some x    -> func x

(* ('a -> unit) -> string -> string -> string -> ('a list) option -> unit *)
let print_list_o func deb sep fin = print_o (print_list func deb sep fin)

let rec print_fichier fich =
    print_list print_classe "" "\n" "" fich.f_classes ;
    print_classe_Main fich.main

and print_classe cl = 
    print_string "class " ;
    print_ident cl.c_name ;
    print_list_o print_param_type_classe "\n\t[" ", " "]" cl.type_class_params ;
    print_list_o print_parametre "\n\t(" ", " ")" cl.params ;
    begin match cl.deriv with
        | None      -> ()
        | Some (a,b)->  print_string "\n\textends" ;
                        print_typ a ;
                        print_list_o print_expr " (" ", " ")" b 
    end ;
    print_list print_decl "\n\t{" ", " "}" cl.decls

and print_decl d = match d.decl_cont with 
    | Dvar v   -> print_var v
    | Dmeth m  -> print_methode m

and print_var v = match v.v_cont with
    | Val (i, o_t, e)   ->  print_string "val " ;
                            print_ident i ;
                            begin match o_t with 
                            | None      -> ()
                            | Some t    ->  print_string " : " ;
                                            print_typ t
                            end ;
                            print_string " =" ;
                            print_expr e
    | Var (i, o_t, e)   ->  print_string "var " ;
                            print_ident i ;
                            begin match o_t with 
                            | None      -> ()
                            | Some t    ->  print_string " : " ;
                                            print_typ t
                            end ;
                            print_string " =" ;
                            print_expr e

and print_methode m =
    if m.m_override then print_string "override " ;
    print_string "def " ;
    print_ident m.m_name ;
    print_list_o print_param_type " [" ", " "]" m.m_type_params ;
    print_list print_parametre " (" ", " ")" m.m_params ;
    print_string " : " ;
    print_typ m.m_res_type ;
    print_string " = " ;
    print_expr m.m_res_expr

and print_parametre p = 
    print_ident p.p_name ;
    print_string " : ";
    print_typ p.p_typ

and print_smile = function
    | Hinf t    -> print_string " >: "; print_typ t
    | Hsup t    -> print_string " <: "; print_typ t

and print_param_type {pt_cont = (id, ho); pt_loc = _ } = 
    print_string id;
    match ho with
        | None          -> ()
        | Some smile    -> print_smile smile

and print_param_type_classe ptc = match ptc.ptc_cont with
    | PTCplus p     -> print_string "+";
                       print_param_type p
    | PTCmoins p    -> print_string "-";
                       print_param_type p
    | PTCrien p     -> print_param_type p

and print_typ typ = Printf.printf "%s " typ.t_name;
                    print_arguments_type typ.args_type

and print_arguments_type {at_cont = argts; at_loc = _ } = print_list_o print_typ "[" "," "]" argts

and print_classe_Main { cM_cont = cm; cM_loc = _ } = print_list print_decl "object Main {" ";\n" "}\n" cm

and print_expr e = match e.e_cont with
    | Evoid             -> print_string "()"
    | Ethis             -> print_string "this"
    | Enull             -> print_string "null"
    | Eint i            -> print_int i
    | Estr s            -> print_char '"';
                           print_string s;
                           print_char '"'
    | Ebool b           -> print_string (string_of_bool b)
    | Eacc acc          -> print_acces acc
    | Eacc_exp (acc, e) -> print_acces acc;
                           print_string " = ";
                           print_expr e
    | Eacc_typ_exp (e', mid, argt, es) ->
        print_expr e'; 
        print_char '.';
        print_string mid;
        print_arguments_type argt;
        print_list print_expr "(" ", " ")" es
    | Enew (id, argt, es) -> Printf.printf "new %s " id;
                             print_arguments_type argt;
                             print_list print_expr " (" ", " ")" es
    | Eneg e            -> print_string "! "; print_expr e
    | Emoins e          -> print_string "-"; print_expr e
    | Ebinop (b, e1, e2)-> print_expr e1;
                           print_char ' ';
                           print_binop b;
                           print_char ' '; 
                           print_expr e2
    | Eifelse (e1, e2, e3) -> print_string "if (";
                             print_expr e1;
                             print_string ") then\n";
                             print_string "\t";
                             print_expr e2;
                             print_string "\nelse\n\t";
                             print_expr e3
    | Ewhile (e1, e2)   -> print_string "while (";
                           print_expr e1;
                           print_string ")\n\t";
                           print_expr e2
    | Ereturn eo        -> print_string "return ";
                           print_o print_expr eo;
                           print_string "\n"
    | Eprint e          -> print_string "print(";
                           print_expr e;
                           print_string ")"
    | Ebloc b           -> print_bloc b

and print_bloc b = print_list print_instruction "{" ";\n" "}" b.bl_cont

and print_instruction = function
    | Ivar v    -> print_var v
    | Iexpr e   -> print_expr e

and print_binop b  = match b.b_cont with
    | EqRef     -> print_string "eq"
    | NeRef     -> print_string "ne"
    | Eq        -> print_string "=="
    | Ne        -> print_string "!="
    | Lt        -> print_string "Lt"
    | Le        -> print_string "Le"
    | Gt        -> print_string "Gt"
    | Ge        -> print_string "Ge"
    | Add       -> print_string "+"
    | Sub       -> print_string "-"
    | Mul       -> print_string "*"
    | Div       -> print_string "/"
    | Mod       -> print_string "%"
    | And       -> print_string "&&"
    | Or        -> print_string "||"

and print_acces a = match a.a_cont with
    | Aident i              -> print_string i
    | Aexpr_ident (e, i)    -> print_expr e; print_char '.'; print_string i

let print f = print_fichier f; print_endline ""

