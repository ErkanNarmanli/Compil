open Ast

let print_ident = print_string

let print_list func deb sep fin xs =
    print_string deb ;
    List.iter (fun x -> func x ; print_string sep) xs ;
    print_string fin

let print_o func = function
    | None      -> ()
    | Some x    -> func xs

let print_list_o func deb sep fin = print_o (print_list func deb sep fin)

let print_fichier fich =
    print_list "" "\n" "" fich.classes ;
    print_main fich.main

and print_classe cl = 
    print_string "class " ;
    print_indent cl.c_name ;
    print_list_o print_param_type_classe "\n\t[" ", " "]" cl.type_class_params ;
    print_list_o print_parametre "\n\t(" ", " ")" cl.params ;
    begin match cl.deriv with
        | None      -> ()
        | Some (a,b)->  print "\n\textends" ;
                        print_typ a ;
                        print_list_o print_expr " (" ", " ")" b 
    end ;
    print_list print_decl "\n\t{" ", " "}" cl.decls

 and print_decl = function
    | Dvar v   -> print_var v
    | Dmeth m  -> print_mehtode m

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
    if mb.override then print_string "override " ;
    print_string "def " ;
    print_ident mb.mb_name ;
    print_list_o print_param_type " [" ", " "]" mb.mb_type_params ;
    print_list print_parametre " (" ", " ")" mb.mb_params ;
    print_bloc mb.bloc
    
and print_meth_expr me =
    if me.override then print_string "override " ;
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
    print_typ p.typ


