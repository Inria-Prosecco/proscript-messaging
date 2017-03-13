open Lexing
open Printf
open Lexer
open Globals

let lexpos_of_loc (l : location) =
    { pos_fname = !ifile; pos_lnum = fst l; pos_bol = 0; pos_cnum = snd l; }

let format_position = function
    | { pos_fname = fn; pos_lnum = line; pos_bol = 0; pos_cnum = pos; } ->
        sprintf "File \"%s\", line %d, character %d" fn line pos
    | _ -> "<unknown position>"
;;

let format_loc (l1,l2) = sprintf "file %s, line %d:%d to %d:%d" l1.pos_fname l1.pos_lnum l1.pos_cnum l2.pos_lnum l2.pos_cnum

type errin_t = EE of Ast.expression_t | ES of Ast.statement_t

let type_error code msg =
	let scode,spos = (match code with 
		| EE(e) -> (Pretty.pretty_print_exp 0 e),(format_loc (snd e))
		| ES(s) -> (Pretty.pretty_print_statement 0 s),(format_loc (snd s)))
	in printf "Cannot type the following expression at %s:\n%s\n%s\n%!" spos scode msg;
	exit 1

let syn_error loc msg = printf "%s at %s\n%!" msg (format_loc loc); exit 1
