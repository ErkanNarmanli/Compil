open Ast
open Printf

let print_ident = print_string

let print_list func deb sep fin xs =
    print_string deb ;
    List.iter (fun x -> func x ; print_string sep) xs ;
    print_string fin

let print_o func = function
    | None      -> ()
    | Some x    -> func x

let print_list_o func deb sep fin = print_o (print_list func deb sep fin)

let rec print_fichier fich =
    print_list print_classe "" "\n" "" fich.classes ;
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

 and print_decl = function
    | Dvar v   -> print_var v
    | Dmeth m  -> print_methode m

and print_var = function
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

and print_methode = function 
    | Mblock mb -> print_meth_bloc mb
    | Mexpr me  -> print_meth_expr me

and print_meth_bloc mb =
    if mb.mb_override then print_string "override " ;
    print_string "def " ;
    print_ident mb.mb_name ;
    print_list_o print_param_type " [" ", " "]" mb.mb_type_params ;
    print_list print_parametre " (" ", " ")" mb.mb_params ;
    print_bloc mb.bloc
    
and print_meth_expr me =
    if me.me_override then print_string "override " ;
    print_string "def " ;
    print_ident me.me_name ;
    print_list_o print_param_type " [" ", " "]" me.me_type_params ;
    print_list print_parametre " (" ", " ")" me.me_params ;
    print_string " : " ;
    print_typ me.res_type ;
    print_string " = " ;
    print_expr me.res_expr

and print_parametre p = 
    print_ident p.p_name ;
    print_typ p.p_typ

and print_smile = function
    | Hinf t    -> print_string " >: "; print_typ t
    | Hsup t    -> print_string " <: "; print_typ t

and print_param_type (id, ho) = 
    print_string id;
    match ho with
        | None          -> ()
        | Some smile    -> print_smile smile

and print_param_type_classe = function
    | PTCplus p     -> print_string "+";
                       print_param_type p
    | PTCmoins p    -> print_string "-";
                       print_param_type p
    | PTCrien p     -> print_param_type p

and print_typ typ = Printf.printf "%s " typ.t_name;
                    print_arguments_type typ.args_type

and print_arguments_type argts = print_list_o print_typ "[" "," "]" argts

and print_classe_Main cm = print_list print_decl "object Main {" ";\n" "}\n" cm

and print_expr = function
    | Evoid             -> print_string "()"
    | Ethis             -> print_string "this"
    | Enull             -> print_string "null"
    | Eint i            -> print_int i
    | Estr s            -> print_string s
    | Ebool b           -> print_string (string_of_bool b)
    | Eacc acc          -> print_acces acc
    | Eacc_exp (acc, e) -> print_acces acc;
                           print_string " = ";
                           print_expr e
    | Eacc_typ_exp (a, argt, es) -> print_acces a; 
                                    print_char ' ';
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
    | Eif (e1, e2)      -> print_string "if (";
                           print_expr e1;
                           print_string ") then\n";
                           print_string "\t";
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

and print_bloc b = print_list print_instruction "{" ";\n" "}" b

and print_instruction = function
    | Ivar v    -> print_var v
    | Iexpr e   -> print_expr e

and print_binop = function
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

and print_acces = function
    | Aident(i1, i2)    -> print_string i1; print_char '.'; print_string i2
    | Aexpr(e, i)       -> print_expr e; print_char '.'; print_string i

let print f = print_fichier f; print_endline ""
