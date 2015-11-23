
{
    open Lexing
    open Parser

    (* Exception signalant une erreur lexicale *)
    exception Lexing_error of string * position

    (* retour à la ligne *)
    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum
        }

    let keywords = [
        "class",        CLASS;
        "def",          DEF;
        "else",         ELSE;
        "eq",           EQ;
        "extends",      EXTENDS;
        "false",        FALSE;
        "if",           IF;
        "ne",           NE;
        "new",          NEW
        "null",         NULL;
        "object",       OBJECT;
        "override",     OVERRIDE;
        "print",        PRINT;
        "return",       RETURN;
        "this",         THIS;
        "true",         TRUE;
        "val",          VAL;
        "var",          VAR;
        "while",        WHILE
    ]


}
    
    let digit = ['0' - '9']
    let alpha = ['a' - 'z'] | ['A' - 'Z']
    let ident = alpha (alpha | digit | '_')


rule token = parse
    | [' ' '\t']+       { token lexbuf }
    | '\n'              { newline lexbuf; token lexbuf }
    | "//"              { short_comment lexbuf }
    | "/*"              { long_comment lexbuf }
    | digit as i        { INT (int_of_string i) }
    | _                 { assert false "TODO1" } 

and short_comment = parse
    | '\n'              { newline lexbuf; token lexbuf }
    | eof               { EOF }
    | _                 { short_comment }

and long_comment = parse
    | '\n'              { newline lexbuf; long_comment lexbuf }
    | "*/"              { token lexbuf }
    | eof               { raise Lexing_error "Commentaire non terminé" }
    | _                 { long_comment lexbuf }