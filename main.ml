
(* Fichier principal de l'interprète mini-Scala *)

open Format
open Lexing
open Typer

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let pprint = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

(* Les options du compilateur que l'on affiche avec --help *)
let options =
  ["-parse-only", Arg.Set parse_only,
   "  Pour ne faire uniquement que la phase d'analyse syntaxique";
   "-pprint", Arg.Set pprint, " Pour n'afficher que les erreurs"
  ]

let usage = "usage: main [option] file.scala"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

(* Idem en plus précis *)
let localisation2 (p1, p2) =
  let l1 = p1.pos_lnum in
  let c1 = p1.pos_cnum - p1.pos_bol + 1 in
  let c2 = p2.pos_cnum - p1.pos_cnum + c1 + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l1 c1 c2 

let () =
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  (* Ce fichier doit avoir l'extension .scala *)
  if not (Filename.check_suffix !ifile ".scala") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .scala\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in

  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: la fonction  Parser.fichier transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.fichier pour obtenir
       le prochain token. *)
   
    let p = Parser.fichier Lexer.token buf in
    if !pprint then
      Ppast.print p; 
    
    close_in f;
    
    
    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    
    let _ = type_fichier p in ()

    (*Interp.fichier p*)
  with
    | Lexer.Lexing_error c ->
	(* Erreur lexicale. On récupère sa position absolue et
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
    eprintf "Erreur lexicale: %s@." c;
	exit 1
    | Parser.Error ->
	(* Erreur syntaxique. On récupère sa position absolue et on la
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur syntaxique@.";
	exit 1
    | Typer.TypeError (l, s) ->
        (* Erreur de typage. On affiche la localisation de l'erreur et un
         * message descriptif *)
        localisation2 l;
        eprintf "Erreur de typage: %s@." s;
        exit 1
(*    | Interp.Error s->
	(* Erreur pendant l'interprétation *)
	eprintf "Erreur : %s@." s;
	exit 1*)

