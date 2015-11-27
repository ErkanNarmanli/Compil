
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


    let implode l =
          let result = String.create (List.length l) in
          let rec imp i = function
          | [] ->       result
          | c :: l ->   result.[i] <- c; imp (i + 1) l in
                        imp 0 l

    let check_size_int s =
            let i = int_of_string s
            in  if ((i <= 2147483648 - 1) && (i >= -2147483648))
                then INT i
                else raise (Lexing_error "Entier trop grand")

}

    let digit   = ['0' - '9']
    let entier  = '0' | (['1' - '9'] digit*)
    let symbol  = ['\\' '\n' '\t' '!' '#' '$' '%' '&' ''' '+' ',' '-' '.' ':' ';' '<' '>' '='
    '?' '@' '^' '_' '`' '|' '~' ' ' '/' '*' ]
    let limits  = ['(' ')' '[' ']' '{' '}']
    let alpha   = ['a' - 'z'] | ['A' - 'Z']
    let car     = digit | symbol | alpha | limits  
    let ident   = alpha (alpha | digit | '_')*


rule token = parse
    | [' ' '\t']+           { token lexbuf }
    | ['\n' '\r']           { newline lexbuf; token lexbuf }
    | '"'                   { lex_chaine "" lexbuf } 
    | entier as i           { check_size_int i }
    | ident as s            { check_kw s }
    | limits as d           { check_del d }
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
    | eof                   { EOF }
    | _                     { raise (Lexing_error "Caractère inconnu") } 

and lex_chaine s = parse
    | '"'                   { STRING s }
    | '\\' 't'              { lex_chaine (s^"\t") lexbuf }   
    | '\\' 'n'              { lex_chaine (s^"\n") lexbuf }   
    | '\\' '\\'             { lex_chaine (s^"\\") lexbuf }
    | '\\' '"'              { lex_chaine (s^"\"") lexbuf }
    | car as c              { lex_chaine (s^(String.make 1 c)) lexbuf }
    | eof                   { raise (Lexing_error "chaine de caractère non terminée") }
    | _                     { raise (Lexing_error "caractère non reconnu")}

and short_comment = parse
    | '\n'                  { newline lexbuf; token lexbuf }
    | eof                   { EOF }
    | _                     { short_comment lexbuf }

and long_comment = parse
    | '\n'                  { newline lexbuf; long_comment lexbuf }
    | "*/"                  { token lexbuf }
    | eof                   { raise (Lexing_error "Commentaire non terminé") }
    | _                     { long_comment lexbuf }

