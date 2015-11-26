
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
        "while",        WHILE;
        "Main",         MAIN
    ]

    let delimitors = [
        '(',            LPAR;
        ')',            RPAR;
        '[',            LCRO;
        ']',            RCRO;
        '{',            LACC;
        '}',            RACC
    ]

    let check_kw s = 
        try
            List.assoc s keywords
        with
        | Not_found -> IDENT s  
        | e         -> raise e

    let check_del d = 
        try
            List.assoc d delimitors
        with 
        | _ ->  assert false
}
    
    let digit   = ['0' - '9']
    let symbol  = ['!' '#' '$' '%' '&' ''' '+' ',' '-' '.' ':' ';' '<' '>' '=' '?' '@' '^' '_' '`' '|' '~' '\\' ]
    let limits  = ['(' ')' '[' ']' '{' '}']
    let alpha   = ['a' - 'z'] | ['A' - 'Z']
    let car     = digit | symbol | alpha | limits | ' ' | '\n' | '/' | '*' | "\\\""
    let ident   = alpha (alpha | digit | '_')*


rule token = parse
    | [' ' '\t']+           { token lexbuf }
    | ['\n' '\r']           { newline lexbuf; token lexbuf }
    | "//"                  { short_comment lexbuf }
    | "/*"                  { long_comment lexbuf }
    | '/'                   { DIV }
    | '+'                   { ADD }
    | '-'                   { SUB }
    | '*'                   { MUL }
    | '%'                   { MOD }
    | ">:"                  { INF }
    | "<:"                  { SUP }
    | "=="                  { EQ }
    | "="                   { EQUAL }
    | "!="                  { NE } 
    | "<="                  { LE }
    | ">="                  { GE } 
    | "<"                   { LT } 
    | ">"                   { GT } 
    | "&&"                  { AND } 
    | "||"                  { OR }
    | ","                   { COMMA }
    | "."                   { DOT }
    | ";"                   { SEMICOLON }
    | ":"                   { CONS }
    | "!"                   { NOT }
    | digit+ as i           { INT (int_of_string i) }
    | ident as s            { check_kw s }
    | '"' (car* as s) '"'   { STRING s } 
    | limits as d           { check_del d }
    | eof                   { EOF }
    | _                     { raise (Lexing_error "Caractère inconnu") } 


and short_comment = parse
    | '\n'                  { newline lexbuf; token lexbuf }
    | eof                   { EOF }
    | _                     { short_comment lexbuf }

and long_comment = parse
    | '\n'                  { newline lexbuf; long_comment lexbuf }
    | "*/"                  { token lexbuf }
    | eof                   { raise (Lexing_error "Commentaire non terminé") }
    | _                     { long_comment lexbuf }

