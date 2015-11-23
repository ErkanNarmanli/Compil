
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

}
    
    let digit = ['0' - '9'] in
    let alpha = ['a' - 'z'] | ['A' - 'Z'] in
    let ident = alpha (alpha | digit | '_')

rule token = parse
    | [' ' '\t']+       { token lexbuf }
    | '\n'              { newline lexbuf; token lexbuf }
    | "//"              { short_comment lexbuf }
    | "/*"              { long_comment lexbuf }
    | _                 { assert false "TODO1" } 

and short_comment = parse
    | '\n'              { newline lexbuf; token lexbuf }
    | eof               { EOF }
    | _                 { short_comment }

and long_comment = parse
    | '\n'              { newline lexbuf; long_comment lexbuf }
    | "*/"              { token lexbuf }
    | eof               { EOF }
    | _                 { long_comment lexbuf }
