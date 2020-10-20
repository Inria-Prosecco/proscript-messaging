open Str
open Error
open Printf
open Pretty
open Globals
open Typecheck

let basename s =
  try String.sub s 0 (String.rindex s '.')
  with Not_found -> s;;

let out = (fun s -> output := Unix.out_channel_of_descr (Unix.openfile s [Unix.O_WRONLY;Unix.O_CREAT] 0o644)) in
let inp = (fun s -> (ifiles := s :: !ifiles)) in 
  Arg.parse [
    ("--check", Arg.Unit (fun () -> typecheck := true), "check defensiveness of input");
    ("--subtypes", Arg.Unit (fun () -> print_subtypes := true), "print the infered type of every expression");
    ("--closed", Arg.Unit (fun () -> closed := true), "do not allow free variables");
    ("--preserve", Arg.Unit (fun () -> preserve := true), "preserve typing env when checking multiple files");
    ("--pv", Arg.Unit (fun () -> pv := true), "convert to ProVerif");
    ("-v", Arg.Unit (fun () -> verbose := true), "verbose output");
    ("--escape-unicode", Arg.Unit (fun ()->escape_unicode:=true), "escape all Unicode characters (ASCII output)");
    ("--indent", Arg.Int (fun n->pad_len:=abs n), "n: number of spaces per indent level");
    ("-o", Arg.String out, "<file>: output file (defaults to stdout)")
  ] inp "Parses JavaScript and check defensiveness.";;

let run fn =
  if not !preserve then free_env := [];
  ifile := fn; col := 0; ln := 1;
  input := (match fn with "<stdio>" -> stdin | s -> Unix.in_channel_of_descr (Unix.openfile s [Unix.O_RDONLY] 0));
  let inbuf = Ulexing.from_utf8_channel !input in
  try
    let parsed = menhir_with_ulex Lexer.main Parser.main inbuf in
    if !typecheck or !pv then
    (
      check_prog env_init parsed; 
      (if !pv then eprintf else printf) "Typing successful, CPU time: %dms.\n--- Free variables ---\n" (truncate (1000.*.Sys.time()));
      List.iter (fun (i,(t,_)) -> (if !pv then eprintf else printf) "%s: %s\n%!" i (print_type (head t.t))) (List.rev !free_env);
      if !pv then printf "%s" (Proverif.pv parsed) 
    )
    else printf "%s" (pretty_print 0 parsed)
  with
    | LexingError (loc, msg) -> fprintf stderr "%s at %s\n%!" msg (format_position (lexpos_of_loc loc))
    | Parser.Error -> fprintf stderr "Unexpected token <%s> at %s\n%!" !lasttok (format_position (lexpos_of_loc (getloc ())))
    | Utf8.MalFormed -> fprintf stderr "Invalid UTF-8 input character at %s\n%!" (format_position (lexpos_of_loc (getloc ())))

let _ =
  let files = match !ifiles with [] -> ["<stdio>"] | l -> List.rev l in
  List.iter run files
