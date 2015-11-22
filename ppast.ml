open Ast
open Printf

and print_param_type = function
    | PTinf (i, typo)   -> print_string i;
                           begin match typo with
                               | None   -> ()
                               | Some t -> print_string " >: "; print_typ t
                           end
    | PTsup (i, typo)   -> print_string i;
                           begin match typo with
                               | None   -> ()
                               | Some t -> print_string " <: "; print_typ t
                           end
                           

and print param_type_classe = function
    | PTCplus p     -> print_string "+";
                       print_param_type p
    | PTCmoins p    -> print_string "-";
                       print_param_type p
    | PTCrien p     -> print_param_type p

and print_typ typ = Printf.printf "%s " typ.t_name;
                    print_arguments_type

and print_arguments_type = print_list_o print_typ "[" "," "]"

and print_classe_Main = print_list print_decl "object Main {" ";\n" "}\n"

and rec print_expr = function
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
    | Enew (id, argt, es) -> Printf.printf "new %s ";
                             print_arguments_type argt;
                             print_string print_list print_expr " (" ", " ")" es
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
    | Eielse (e1, e2, e3) -> print_string "if (";
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

and print_bloc = print_list print_instruction "{" ";\n" "}"

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
    | Gt        -> print_string "Gt"
    | Add       -> print_string "+"
    | Sub       -> print_string "-"
    | Mul       -> print_string "*"
    | Div       -> print_string "/"
    | Mod       -> print_string "%"
    | And       -> print_string "&&"
    | Or        -> print_strinh "||"

and print_acces = function
    | Aident(i1, i2)    -> print_string i1; print_char '.'; print_string i2
    | Aexpr(e, i)       -> print_expr e; print_char '.'; print_string i
