open Ulexing
open Htmlparser
open Printf
open Globals

let c b = (lasttok := utf8_lexeme b; col := !col + lexeme_length b) 
let nl () = lasttok := "\\n"; ln:=!ln+1; col := 0 
let subs s i = String.sub s i ((String.length s)-i)

let regexp tagname = ['a'-'z' 'A'-'Z' '-' '0'-'9']
let regexp whitespace = ['\t' ' ']
let regexp newline = "\r\n" | ['\n' '\r']

let intag = ref false

let rec main lexbuf = 
  if !intag then tag lexbuf else token lexbuf

and token = lexer
  | "</" -> c lexbuf; intag := true; TAG_SLASH_OPEN
  | "<" -> c lexbuf; intag := true; TAG_OPEN
  | "<!DOCTYPE" -> c lexbuf; intag:=true; DOCTYPE
  | newline -> nl (); token lexbuf
  | eof -> EOF
  | _ -> c lexbuf; STRING(utf8_lexeme lexbuf)

and tag = lexer
  | whitespace* -> c lexbuf; tag lexbuf
  | newline -> nl (); tag lexbuf
  | ">" -> c lexbuf; intag := false; TAG_CLOSE
  | "/" -> c lexbuf; SLASH
  | "=" -> c lexbuf; EQUAL
  | tagname+ -> c lexbuf; STRING(utf8_lexeme lexbuf)
  | "\"" -> c lexbuf; STRING(string_scanner lexbuf)
  | eof -> lerr "Unclosed tag"

and string_scanner = lexer
  | "\\\"" -> c lexbuf; "\""^(string_scanner lexbuf)
  | "\"" -> ""
  | eof -> lerr "Unclosed string literal"
  | newline -> lerr "Line break in quoted string"
  | _ -> let s = utf8_lexeme lexbuf in c lexbuf; s^(string_scanner lexbuf)

