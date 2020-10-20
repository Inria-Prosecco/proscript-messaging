open Lexing
open Printf

let input = ref stdin
let output = ref stdout
let verbose = ref false
let typecheck = ref false
let closed = ref false
let preserve = ref false
let re = ref true
let pv = ref false
let escape_unicode = ref false
let print_subtypes = ref false
let pad_len = ref 2
let ifile = ref ""
let ifiles : string list ref = ref []
let ln = ref 1
let col = ref 0
let lasttok = ref ""

(* Error and lexer position handling *)
type location = int * int
exception LexingError of location * string
let getloc () = (!ln, !col)
let lerr msg = raise (LexingError (getloc (), msg))
let lwar msg = fprintf stderr "Tokenizer warning: %s on line %d, character %d\n%!" msg !ln !col

let lexpos_of_loc (l : location) =
    { pos_fname = !ifile; pos_lnum = fst l; pos_bol = 0; pos_cnum = snd l; }

let cutdot = let rex = Pcre.regexp "\\.$"
  in fun s->Pcre.replace ~rex:rex ~templ:"" s 

let rec string_of_bytelist = function
  | h::t -> (String.make 1 (char_of_int h)) ^ (string_of_bytelist t)
  | [] -> ""

let utf8_char c =
  if c < 128 then string_of_bytelist [c]
  else if c < 2048 then string_of_bytelist [(c lsr 6) lor 192; (c land 63) lor 128]
  else string_of_bytelist [(c lsr 12) lor 224; ((c lsr 6) land 63) lor 128; (c land 63) lor 128]

exception Bad_char

let recode_utf8 ?regexp:(eslash=false) s =
  let rec peek c i a =
    if c = 0 then a else
    if i >= String.length s then raise Bad_char else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 || n >= 0xc0 then raise Bad_char else
    peek (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
  let chr = function
    | 34 when not eslash -> "\\\"" | 92 when not eslash -> "\\\\" | 9 -> "\\t" | 10 -> "\\n"
    | 11 -> "\\v" | 12 -> "\\f" | 8 -> "\\b" | 47 when eslash -> "\\/" | 13 -> "\\r"
    | n when n < 0x20 -> "\\x"^(sprintf "%02x" n)
    | _ as n -> if (!escape_unicode && n>=127) or n=0x2028 or n=0x2029 then "\\u"^(sprintf "%04x" n) else utf8_char n in
  let rec main i acc =
   if i >= String.length s then acc else
   let n = Char.code (String.unsafe_get s i) in
   try
    if n < 0x80 then
     let s = chr n in main (i + 1) (acc^s)
    else if n < 0xc2 then raise Bad_char
    else if n <= 0xdf then 
      let nn = peek 1 (i + 1) (n - 0xc0) in
			if nn < 0x80 then raise Bad_char else 
      main (i + 2) (acc^(chr nn))
    else if n <= 0xef then 
      let nn = peek 2 (i + 1) (n - 0xe0) in
			if nn < 0x800 then raise Bad_char else 
      main (i + 3) (acc^(chr nn))
    else if n <= 0xf7 then 
      let nn = peek 3 (i + 1) (n - 0xf0) in
			if nn < 0x10000 then raise Bad_char else
      main (i + 4) (acc^(chr nn))
    else raise Bad_char
   with Bad_char -> main (i+1) (acc^("\\x"^(sprintf "%02x" n)))
  in
    main 0 ""

let menhir_with_ulex lexer menhir_parser lexbuf =
  let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
  let lexer_maker () =
    let pre = lexpos_of_loc (getloc ()) in
                let tok = lexer lexbuf in
                let post = lexpos_of_loc (getloc ()) in
    (tok, pre, post)
  in revised_parser lexer_maker
;;

