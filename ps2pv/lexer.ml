(**
 Javascript token syntax (from ECMA 262 rev 5)
**)
open Parser
open Ulexing
open Printf
open Globals
 
let c b = (lasttok := utf8_lexeme b; col := !col + lexeme_length b) 
let nl () = lasttok := "\\n"; ln:=!ln+1; col := 0
let subs s i = String.sub s i ((String.length s)-i)

(* Whitespace and line breaks *)
let regexp whitespace = ['\t' '\x0B' '\x0C' ' ' '\xA0' 0x1680 0x180E 0x2000-0x200A 0x202F 0x205F 0x3000 0xFEFF]
let regexp newline = "\r\n" | ['\010' '\013' 0x2028 0x2029]
let regexp notnewline = [^ '\010' '\013' 0x2028 0x2029]
let regexp blank = whitespace | newline

(* Numeric literals §7.8.3 *)
let regexp nonz_digit = ['1'-'9']
let regexp digit = '0' | nonz_digit
let regexp hex_digit = digit | ['a'-'f' 'A'-'F']
let regexp hex_literal = '0' ['x' 'X'] hex_digit+
let regexp signed_integer = ['-' '+']? digit+
let regexp exponent = ['e' 'E'] signed_integer
let regexp decimal_int_literal = '0' | (nonz_digit digit* )

let regexp decimal_literal = (decimal_int_literal ('.' (digit* ))? exponent?) | ('.' (digit+) (exponent?))   
let regexp numeric_literal = hex_literal | decimal_literal 

(* Escape sequences *)
let regexp unicode_escape_seq = 'u' hex_digit hex_digit hex_digit hex_digit
let regexp hex_escape_seq = 'x' hex_digit hex_digit
let regexp single_escape_seq = ['\'' "\"" '\\' 'b' 'f' 'n' 'r' 't' 'v' '0']

(* Escaping functions *)
let escape_single = function "b"->"\x08" | "t"->"\x09" | "n"->"\x0A" | "v"->"\x0B" | "f"->"\x0C" | "r"->"\x0D" | "0" -> "\x00" |_ as s -> s
let escape_hex x = string_of_bytelist [int_of_string ("0" ^ x)] 
let escape_unicode x = Bytes.set x 0 'x'; let c = int_of_string ("0"^(Bytes.to_string x)) in utf8_char c

(* Keywords and reserved words *)
let regexp reserved = "class" | "enum" | "extends" | "super" | "const" | "export" | "import"
let regexp future_reserved = "implements" | "let" | "private" | "public" | "yield" | "interface" | "package" | "protected" | "static"

let regexp right_keyword = "typeof" | "new" | "var" | "const" | "do" | "case" | "void" | "for" | "switch"
  | "while" | "delete" | "function" | "with" | "default" | "if" | "else" | "try"
let regexp bi_keyword = "instanceof" | "in" | "finally" | "catch"
let regexp keyword = "debugger" | "this" | "continue" | "break" | "return" | "throw" | "++" | "--"

let regexp bi_punctuator = "<=" | ">=" | "==" | "!=" | "===" | "!==" | ">>" | "<<" | ">>>" | "&&" | "||"  | "+=" | "-=" | "*=" 
  | "%=" | "<<=" | ">>=" | ">>>=" | "&=" | "|=" | "^=" | [';' ',' '<' '>' '+' '-' '*' '%' '=' '&' '^' '|' '?' ':' '(' '[']
let regexp right_punctuator = ['!' '~' '{']
let regexp left_punctuator = [')' ']']

let regexp bi_kw = bi_punctuator | bi_keyword
let regexp right_kw = right_keyword | right_punctuator

(* Literal values *)
let regexp value_literal = "true" | "false" | "null" (*| "undefined" | "NaN" | "Infinity" *)

(* String literals *)
let regexp squote = '\''
let regexp dquote = '"'

(* Conservative identifier: actual checking is done with PCRE *)
(* Damnit why doesn't Ulex support class difference r1 # r2? This is awful. *)
let regexp identifier = ([^ '{' '}' '(' ')' '[' ']' '.' ';' ',' '<' '>' '+' '-' '*' '/' '%' '!' '&' '^' '|' '?' ':' '~' '=' '\'' "\"" '\t' '\x0B' '\x0C' ' ' '\xA0' 0x1680 0x180E 0x2000-0x200A 0x202F 0x205F 0x3000 0xFEFF '\010' '\013' 0x2028 0x2029])+

(* Token constructors *)
let k2c = function
  | "true" -> BOOL(true) | "false" -> BOOL(false) | "null" -> NULL (*| "undefined" -> UNDEFINED*)
  (* | "NaN" -> NUMBER(nan) | "Infinity" -> NUMBER(infinity) *)
  | "do" -> DO | "for" -> FOR | "while" -> WHILE | "break" -> BREAK | "continue" -> CONTINUE
  | "instanceof" -> INSTANCEOF | "typeof" -> TYPEOF
  | "new" -> NEW | "var" -> VAR | "const" -> CONST | "delete" -> DELETE
  | "switch" -> SWITCH | "case" -> CASE | "default" -> DEFAULT
  | "debugger" -> DEBUGGER
  | "function" -> FUNCTION | "this" -> THIS | "return" -> RETURN | "void" -> VOID
  | "with" -> WITH | "in" -> IN
  | "if" -> IF | "else" -> ELSE
  | "try" -> TRY | "throw" -> THROW | "catch" -> CATCH | "finally" -> FINALLY
	| "{" -> LC | "(" -> LP | "[" -> LB | "]" -> RB | ")" -> RP | "}" -> RC
	| "." -> DOT | ";" -> SEMI | "," -> COMMA | "?" -> QM | ":" -> COLON | "!" -> LNOT | "~" -> BNOT
	| "<" -> LT | "<=" -> LE | ">" -> GT | ">=" -> GE | "==" -> EQ | "!=" -> NEQ | "===" -> SEQ | "!==" -> SNEQ
	| "+" -> PLUS | "-" -> MINUS | "*" -> TIMES | "%" -> MOD | "&" -> BAND | "|" -> BOR | "^" -> XOR
	| "++" -> INCR | "--" -> DECR | ">>" -> RSH | "<<" -> LSH | ">>>" -> ARSH | "&&" -> LAND | "||" -> LOR
	| "=" -> ASSIGN | "+=" -> PLUS_ASSIGN | "-=" -> MINUS_ASSIGN | "*=" -> TIMES_ASSIGN | "|=" -> OR_ASSIGN
	| "&=" -> AND_ASSIGN | "^=" -> XOR_ASSIGN | ">>=" -> RSH_ASSIGN | "<<=" -> LSH_ASSIGN | ">>>=" -> ARSH_ASSIGN
	| "%=" -> MOD_ASSIGN | "/" -> DIV | "/=" -> DIV_ASSIGN
	| "class" | "enum" | "extends" | "super" | "const" | "export" | "import" | "implements"
  | "let" | "private" | "public" | "yield" | "interface" | "package" | "protected" | "static" as s -> RESERVED(s)
  | _ as s -> KEYWORD(s)

(* Decoding of Unicode escape sequence in identifiers *)
let decode_unicode ?allowkw:(kw=false) =
  let urex = Pcre.regexp "\\\\(u[0-9a-fA-F]{4})" in
	(* Defer identifier checking to PCRE to take advantage of Unicode class support *)
	let irex = Pcre.regexp ~flags:[`UTF8] "^([$_]|\\p{Lu}|\\p{Ll}|\\p{Lt}|\\p{Lm}|\\p{Lo}|\\p{Nl})([$_]|\\p{Lu}|\\p{Ll}|\\p{Lt}|\\p{Lm}|\\p{Lo}|\\p{Nl}|\\p{Mn}|\\p{Mc}|\\p{Nd}|\\p{Pc}|\\x{200C}|\\x{200D})*$" in
    let rep s = let y = (Bytes.of_string s) in Bytes.set y 1 '0'; escape_unicode y in
  fun is -> let s = Pcre.substitute ~rex:urex ~subst:rep is in
	  match Pcre.pmatch ~rex:irex s with
			| true ->
			begin 
				match k2c s with
				| KEYWORD(_) -> s
				| _ -> if kw then s else lerr (sprintf "Can't use keyword or reserved word <%s> as identifier" s)
		  end
      | false -> lerr (sprintf "Invalid identifier name <%s>" is)

let trim =
	let rex = Pcre.regexp ~flags:[`UTF8] "[ \\t\\x0B\\x0C\\xA0\\x{1680}\\x{180E}\\x{2000}-\\x{200A}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}]"
	and rex2 = Pcre.regexp ~flags:[`UTF8] "(\\r\\n|[\\r\\n\\x{2028}\\x{2029}])" in
	fun s -> let sw = Pcre.replace ~rex:rex ~templ:"" s in
    Pcre.substitute ~rex:rex2 ~subst:(fun l->nl (); "") sw
	   

let cnl =
	let rex = Pcre.regexp ~flags:[`UTF8] "(\\r\\n|\\r|\\n|\\x{2028}|\\x{2029})" in
	fun s -> try let r = Pcre.exec_all ~rex:rex s in Array.length r
	  with Not_found->0

let memory = ref None
let cut_right = ref false
let finished = ref false

(* The Tokenizer has a lookahead of 2 because of automatic semicolon insertion *)
let rec main lexbuf =  
  let res = match !memory with
		| Some RC -> memory := Some EOL; RC
    | Some t -> memory := None; t
    | None -> let t = token lexbuf in
      match !memory with Some EOL -> memory := Some t; EOL | _ -> t
  in res

and token = lexer
  | whitespace* | ("//" notnewline* ) -> c lexbuf; token lexbuf
  | "/*" -> c lexbuf; comment_scanner lexbuf; 
  | newline -> nl (); if not !cut_right then memory := Some EOL; token lexbuf
  | left_punctuator -> let s = utf8_lexeme lexbuf in c lexbuf; re := false; cut_right := false; memory := None; k2c (trim s)
  | '}' -> c lexbuf; re := true; memory := Some RC; cut_right := false; EOL
  | bi_kw blank* -> let s = utf8_lexeme lexbuf in c lexbuf; re := true; cut_right := true; memory := None; k2c (trim s)
  | '.' blank* -> c lexbuf; re := true; cut_right := true; memory := Some(dot_scanner lexbuf); DOT
  | right_kw blank* -> let s = utf8_lexeme lexbuf in c lexbuf; re := true; cut_right := true; k2c (trim s)
  | keyword -> let s = utf8_lexeme lexbuf in c lexbuf; re := true; cut_right := false; k2c s
	| value_literal -> let s = utf8_lexeme lexbuf in c lexbuf; re := false; cut_right := false; k2c s
  | reserved | future_reserved -> let s = utf8_lexeme lexbuf in c lexbuf; re := true; cut_right := false; RESERVED(s)
  | ['\'' "\"" "`"] -> let s = string_scanner (utf8_lexeme lexbuf) lexbuf in incr col; re := false; cut_right := false; STRING(s)
  | '/' '='? -> c lexbuf; let t = if !re then
      (re:=false; cut_right := false; REGEXP(regexp_scanner (subs (utf8_lexeme lexbuf) 1) lexbuf))
			else (memory := None; cut_right := true; re:=true; k2c (utf8_lexeme lexbuf)) in t
  | decimal_literal -> let i = float_of_string (utf8_lexeme lexbuf) in c lexbuf; re := false; cut_right := false; NUMBER(i)
  | hex_literal -> let i = int_of_string (utf8_lexeme lexbuf) in c lexbuf; re := false; cut_right := false; BYTE(i)
  | identifier -> let id = decode_unicode (utf8_lexeme lexbuf) in c lexbuf; re := false; cut_right := false; IDENTIFIER(id)
  | eof -> if !finished or !memory=Some EOL then EOF else (finished := true; EOL)
  | _ -> lerr (sprintf "Unexpected token: <%s>" (utf8_lexeme lexbuf))

and dot_scanner = lexer
  | whitespace* | ("//" notnewline* ) -> c lexbuf; dot_scanner lexbuf
  | "/*" -> c lexbuf; comment_scanner ~return:dot_scanner lexbuf;
  | identifier -> let id = decode_unicode ~allowkw:true (utf8_lexeme lexbuf) in
      c lexbuf; re := false; cut_right := false; IDENTIFIER(id)
	| _ -> lerr (sprintf "Invalid property for dot notation: <%s>" (utf8_lexeme lexbuf))

and string_scanner quote = lexer
  | newline | eof -> lerr "Unexpected end of string literal"
  | ['\'' "\""] -> let q = utf8_lexeme lexbuf in incr col; if q=quote then "" else q^(string_scanner quote lexbuf)
  | ['`'] -> let q = utf8_lexeme lexbuf in incr col; if q="`" then "" else q^(string_scanner quote lexbuf) 
  | '\\' -> incr col; string_escape_scanner quote lexbuf
  | [^ '\'' "\"" '\\' '\010' '\013' 0x2028 0x2029]+ -> let s = utf8_lexeme lexbuf in c lexbuf; s ^ (string_scanner quote lexbuf)   

and string_escape_scanner quote = lexer
  | newline -> nl (); string_scanner quote lexbuf
  | single_escape_seq -> let s = escape_single (utf8_lexeme lexbuf) in c lexbuf; s ^ (string_scanner quote lexbuf)
  | unicode_escape_seq -> let s = escape_unicode (Bytes.of_string (utf8_lexeme lexbuf)) in c lexbuf; s ^ (string_scanner quote lexbuf)
  | hex_escape_seq -> let s = escape_hex (utf8_lexeme lexbuf) in c lexbuf; s ^ (string_scanner quote lexbuf)
  | _ -> c lexbuf; let s = utf8_lexeme lexbuf in lwar (sprintf "Bad escape sequence: \\%s" s); s ^ (string_scanner quote lexbuf)  

and regexp_scanner acc = lexer
  | newline | eof -> lerr "Unexpectend end of regexp literal"
  | "\\" -> c lexbuf; regexp_escape_scanner acc false lexbuf
  | '[' -> c lexbuf; regexp_class_scanner (acc^"[") lexbuf
  | '/' ['a'-'z' 'A'-'Z']* -> c lexbuf; let s = utf8_lexeme lexbuf in (acc, subs s 1)
  | [^ '/' '\\'] -> c lexbuf; let s = acc^(utf8_lexeme lexbuf) in regexp_scanner s lexbuf

and regexp_escape_scanner = (
 fun acc cl -> let tk = (if cl then regexp_class_scanner else regexp_scanner) in lexer
  | newline | eof -> lerr "Unexpectend end of regexp literal"
  | unicode_escape_seq -> let s = escape_unicode (Bytes.of_string (utf8_lexeme lexbuf)) in c lexbuf; tk (acc^s) lexbuf
  | hex_escape_seq -> let s = escape_hex (utf8_lexeme lexbuf) in c lexbuf; tk (acc^s) lexbuf
  | ['/'] -> let s = acc^(utf8_lexeme lexbuf) in c lexbuf; tk s lexbuf
  | ['\\' '[' ']' '{' '}' '(' ')' '+' '.' '*' '?' '|' '^' '$' 'B' 'D' 'S' 'W' 'c' 'd' 's'-'x' '0'-'9' '-']
     -> let s = acc^"\\"^(utf8_lexeme lexbuf) in c lexbuf; tk s lexbuf
  | single_escape_seq -> let s = escape_single (utf8_lexeme lexbuf) in c lexbuf; tk (acc^s) lexbuf  
  | _ -> c lexbuf; let s = utf8_lexeme lexbuf in lwar (sprintf "Bad escape sequence in regexp: \\%s" s); tk (acc^s) lexbuf
 )

and regexp_class_scanner acc = lexer
  | newline | eof -> lerr "Unexpectend end of class in regexp literal"
	| "\\" -> c lexbuf; regexp_escape_scanner acc true lexbuf
  | ']' -> c lexbuf; regexp_scanner (acc^"]") lexbuf
	| _ -> let s = utf8_lexeme lexbuf in c lexbuf; regexp_class_scanner (acc^s) lexbuf

and comment_scanner ?return:(tk=token) = lexer
  | newline -> nl (); if not !cut_right then memory := Some EOL; comment_scanner lexbuf
  | "*/" -> c lexbuf; tk lexbuf
  | eof -> lerr "Unclosed multiline comment"
  | _ -> c lexbuf; comment_scanner lexbuf
