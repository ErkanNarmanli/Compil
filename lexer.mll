
{
    open Lexing
    open Parser

    (* Exception signalant une erreur lexicale *)
    exception Lexing_error of string

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
        "eq",           EQREF;
        "extends",      EXTENDS;
        "false",        FALSE;
        "if",           IF;
        "ne",           NEREF;
        "new",          NEW;
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
    
    let check_kw s = 
        try
            List.assoc s keywords
        with
        | Not_found _ -> IDENT s  

}
    
    let digit = ['0' - '9']
    let alpha = ['a' - 'z'] | ['A' - 'Z']
    let ident = alpha (alpha | digit | '_')


rule token = parse
    | [' ' '\t']+       { token lexbuf }
    | ['\n' '\r']+      { newline lexbuf; token lexbuf }
    | "//"              { short_comment lexbuf }
    | "/*"              { long_comment lexbuf }
    | digit+ as i       { INT (int_of_string i) }
    | ident as s        { check_kw s }
    | eof               { EOF }
    | _                 { assert false } 

and short_comment = parse
    | '\n'              { newline lexbuf; token lexbuf }
    | eof               { EOF }
    | _                 { short_comment lexbuf }

and long_comment = parse
    | '\n'              { newline lexbuf; long_comment lexbuf }
    | "*/"              { token lexbuf }
    | eof               { raise (Lexing_error "Commentaire non terminé") }
    | _                 { long_comment lexbuf }

