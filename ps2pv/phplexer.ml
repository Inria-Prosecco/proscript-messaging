open Ulexing
open Phparser
open Printf
open Globals

let c b = (lasttok := utf8_lexeme b; col := !col + lexeme_length b) 
let nl () = lasttok := "\\n"; ln:=!ln+1; col := 0 
let subs s i = String.sub s i ((String.length s)-i)

(* Whitespace and line breaks *)
let regexp whitespace = ['\t' ' ']
let regexp newline = "\r\n" | ['\n' '\r']
let regexp notnewline = [^ '\r' '\n']
let regexp blank = whitespace | newline

(* Numeric constants *)
let regexp digits = ['0'-'9']
let regexp hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let regexp lnum = digits+
let regexp anum = digits*
let regexp dnum = (anum "." lnum)|(lnum "." anum)
let regexp hnum = "0x" hex_digit+
let regexp bnum = "0b" ['0'-'1']+
let regexp expnum = (lnum|dnum) ['e' 'E'] ['+' '-']? lnum 

(* Escaping functions *)
let escape_single = function "b"->"\x08" | "t"->"\x09" | "n"->"\x0A" | "v"->"\x0B" | "f"->"\x0C" | "r"->"\x0D" | "0" -> "\x00" | _ as s -> s
let escape_hex x = string_of_bytelist [int_of_string ("0" ^ x)] 
let escape_unicode x = String.set x 0 'x'; let c = int_of_string ("0"^x) in utf8_char c

let regexp hex_escape_seq = 'x' hex_digit hex_digit
let regexp single_escape_seq = ['\'' '"' '\\' 'f' 'n' 'r' 't' 'v' '0']

let regexp keyword = [';' ':' ',' '.' '\\' '[' ']' '{' '}' '(' ')' '|' '^' '&' '+' '-' '/' '*' '=' '%' '!' '~' '$' '<' '>' '?' '@']
  | "++" | "--" | "===" | "!==" | "==" | "!=" | "<>" | "<=" | ">=" | "+=" | "-=" | "/=" | ".=" | "%=" | "<<="
  | ">>=" | "&=" | "|=" | "^=" | "||" | "&&" | "OR" | "AND" | "XOR" | "<<" | ">>" | "=>" | "::" | "->"
  | "exit" | "die" | "function" | "const" | "return" | "yield" | "try" | "catch" | "finally" | "throw" | "if" | "elseif"
  | "else" | "while" | "do" | "for" | "foreach" | "declare" | "instanseof" | "as" | "switch" | "case" | "default" | "break"
  | "continue" | "goto" | "echo" | "print" | "class" | "interface" | "trait" | "extends" | "implements" | "new" | "clone"
  | "var" | "eval" | "include" | "include_once" | "require" | "require_once" | "namespace" | "use" | "insteadof" | "global"
  | "isset" | "empty" | "__halt_compiler" | "static" | "abstract" | "final" | "private" | "protected" | "public" | "unset"
  | "list" | "array" | "callable" | "__FUNCTION__" | "__METHOD__" | "__CLASS__" | "__LINE__" | "__FILE__" | "__DIR__"

(* String literals *)
let regexp squote = '\''
let regexp dquote = '"'
let regexp bquote = '`'
let regexp label = ['a'-'z' 'A'-'Z' '_' '\x7f'-'\xff']['a'-'z' 'A'-'Z' '0'-'9' '_' '\x7f'-'\xff']*

(* Token constructors *)
let k2c = function
  | ";" -> SEMI | ":" -> COLON | "," -> COMMA | "." -> DOT | "\\" -> BACKSL | "[" -> LB | "]" -> RB
  | "{" -> LC | "}" -> RC | "(" -> LP | ")" -> RP | "|" -> BOR | "^" -> BXOR | "&" -> BAND | "+" -> PLUS
  | "-" -> MINUS | "/" -> DIV | "*" -> MUL | "=" -> ASSIGN | "%" -> MOD | "!" -> LNOT | "~" -> BNOT
  | "$" -> DOLLAR | "<" -> LT | ">" -> GT | "?" -> QM | "@" -> AT | "++" -> INCR | "--" -> DECR
  | "===" -> SEQUAL | "!==" -> NSEQUAL | "==" -> EQUAL | "!=" -> NEQUAL | "<>" -> NEQUAL | "<=" -> LE
  | ">=" -> GE | "+=" -> PLUS_ASSIGN | "-=" -> MINUS_ASSIGN | "/=" -> DIV_ASSIGN | ".=" -> DOT_ASSIGN
  | "%=" -> MOD_ASSIGN | "<<=" -> LSH_ASSIGN | ">>=" -> RSH_ASSIGN | "&=" -> AND_ASSIGN | "|=" -> OR_ASSIGN
  | "^=" -> XOR_ASSIGN | "||" -> LOR | "&&" -> LAND | "OR" -> LOR | "AND" -> LAND | "XOR" -> LXOR
  | "*=" -> MUL_ASSIGN | "<<" -> LSH | ">>" -> RSH | "=>" -> DBLARROW | "::" -> DBLCOLON | "->" -> ARROW
  | "die" | "exit" -> EXIT | "function" -> FUNCTION | "const" -> CONST | "return" -> RETURN
  | "yield" -> YIELD | "try" -> TRY | "catch" -> CATCH | "finally" -> FINALLY | "throw" -> THROW
  | "if" -> IF | "elseif" -> ELSEIF | "else" -> ELSE | "while" -> WHILE | "do" -> DO | "for" -> FOR
  | "foreach" -> FOREACH | "declare" -> DECLARE | "instanceof" -> INSTANCEOF | "as" -> AS
  | "switch" -> SWITCH | "case" -> CASE | "default" -> DEFAULT | "break" -> BREAK | "continue" -> CONTINUE
  | "goto" -> GOTO | "echo" -> ECHO | "print" -> PRINT | "class" -> CLASS | "interface" -> INTERFACE
  | "trait" -> TRAIT | "extends" -> EXTENDS | "implements" -> IMPLEMENTS | "new" -> NEW | "clone" -> CLONE
  | "var" -> VAR | "eval" -> EVAL | "include" -> INCLUDE | "include_once" -> INCLUDE_ONCE | "require" -> REQUIRE
  | "require_once" -> REQUIRE_ONCE | "namespace" -> NAMESPACE | "use" -> USE | "insteadof" -> INSTEADOF
  | "global" -> GLOBAL | "isset" -> ISSET | "empty" -> EMPTY | "__halt_compiler" -> HALT | "static" -> STATIC
  | "abstract" -> ABSTRACT | "final" -> FINAL | "private" -> PRIVATE | "protected" -> PROTECTED
  | "public" -> PUBLIC | "unset" -> UNSET | "list" -> LIST | "array" -> ARRAY | "callable" -> CALLABLE
  | "__FUNCTION__" -> SFUNCTION | "__METHOD__" -> SMETHOD | "__CLASS__" -> SCLASS | "__LINE__" -> SLINE
  | "__FILE__" -> SFILE | "__DIR__" -> SDIR | _ -> EOF

let script = ref false
let implicitsemi = ref false
let lookahead = ref None

let rec main lexbuf =
  match !lookahead with Some(t) -> t
  | None -> if !script then token lexbuf else toplevel lexbuf

and toplevel ?pre:(acc="") = lexer
  | "<?=" | "<%=" -> c lexbuf; script:=true; implicitsemi := true; ECHO
  | "<?" | "<?php" | "<%" | "<script language=\"php\">" -> c lexbuf; script:=true; token lexbuf
  | newline -> nl (); toplevel ~pre:(acc^(utf8_lexeme lexbuf)) lexbuf
  | eof -> EOF
  | _ -> c lexbuf; let s = acc^(utf8_lexeme lexbuf) in INLINE_HTML(s)

and token = lexer
  | whitespace* | ("//" notnewline* ) | ("#" notnewline* ) -> c lexbuf; token lexbuf
  | "/*" -> c lexbuf; comment_scanner lexbuf; 
  | newline -> nl (); token lexbuf
  | keyword -> let s = utf8_lexeme lexbuf in c lexbuf; k2c s
  | ['\'' '"' '`'] -> let s = string_scanner (utf8_lexeme lexbuf) lexbuf in c lexbuf; STRING(s)
  | lnum -> let i = int_of_string (utf8_lexeme lexbuf) in c lexbuf; LNUMBER(i)
  | dnum | expnum -> let i = float_of_string (utf8_lexeme lexbuf) in c lexbuf; DNUMBER(i)
	| '$' label -> let s = utf8_lexeme lexbuf in c lexbuf; VARIABLE(subs s 1)
  | label -> let id = utf8_lexeme lexbuf in c lexbuf; LABEL(id)
  | '(' whitespace* "int" "eger"? whitespace* ')' -> c lexbuf; INT_CAST
  | '(' whitespace* "bool" "ean"? whitespace* ')' -> c lexbuf; BOOL_CAST
  | '(' whitespace* ("string" | "binary") whitespace* ')' -> c lexbuf; STRING_CAST
  | '(' whitespace* ("real"|"double"|"float") whitespace* ')' -> c lexbuf; FLOAT_CAST
  | '(' whitespace* "array" whitespace* ')' -> c lexbuf; ARRAY_CAST
  | '(' whitespace* "object" whitespace* ')' -> c lexbuf; OBJECT_CAST
  | '(' whitespace* "unset" whitespace* ')' -> c lexbuf; UNSET_CAST
  | "?>" | "%>" | "</scritp>" -> c lexbuf; script:=false;
    if !implicitsemi then (implicitsemi := false; SEMI) else toplevel lexbuf
  | eof -> EOF
  | _ -> lerr (sprintf "Unexpected token: <%s>" (utf8_lexeme lexbuf))

and string_scanner quote = lexer
  | ['\'' '"' '`'] -> let q = utf8_lexeme lexbuf in incr col; if q=quote then "" else q^(string_scanner quote lexbuf)
  | '\\' -> incr col; string_escape_scanner quote lexbuf
  | newline -> let s = utf8_lexeme lexbuf in nl(); s ^ (string_scanner quote lexbuf)
  | eof -> lerr "Unclosed string literal"   
  | _ -> let s = utf8_lexeme lexbuf in c lexbuf; s ^ (string_scanner quote lexbuf)

and string_escape_scanner quote = lexer
  | newline -> nl (); string_scanner quote lexbuf
  | single_escape_seq -> let s = escape_single (utf8_lexeme lexbuf) in c lexbuf; s ^ (string_scanner quote lexbuf)
  | hex_escape_seq -> let s = escape_hex (utf8_lexeme lexbuf) in c lexbuf; s ^ (string_scanner quote lexbuf)
  | _ -> c lexbuf; let s = utf8_lexeme lexbuf in lwar (sprintf "Bad escape sequence: \\%s" s); s ^ (string_scanner quote lexbuf)  

and comment_scanner = lexer
  | newline -> nl (); comment_scanner lexbuf
  | "*/" -> c lexbuf; token lexbuf
  | eof -> lerr "Unclosed multiline comment"
  | _ -> c lexbuf; comment_scanner lexbuf
