open Printf

type loc = Lexing.position * Lexing.position
type 'a located = 'a * loc

(* Type of Javascript program *)
type t = source_t located list 

and source_t = [
	| `Statement of statement_t
	| `FunctionDeclaration of function_t 
]

and statement_t = stm_t located

and stm_t = [
	| `Empty
	| `Debugger
	| `Return of expression_t option
	| `Throw of expression_t
	| `Continue of identifier_t option
	| `Break of identifier_t option
	| `Try of statement_t * (identifier_t * statement_t) option * statement_t option 
	| `Block of statement_t list
	| `Label of identifier_t * statement_t
	| `Expression of expression_t
	| `Declaration of (identifier_t * expression_t option) list
	| `Const of (identifier_t * expression_t option) list
	| `If of expression_t * statement_t * statement_t option
	| `Do of statement_t * expression_t
	| `While of expression_t * statement_t
	| `For of forinit_t option * expression_t option * expression_t option * statement_t
	| `Forin of forinit_t * expression_t * statement_t
	| `With of expression_t * statement_t
	| `Switch of expression_t * statement_t list option * (expression_t * statement_t list) list
]

and forinit_t = finit_t located 

and finit_t = [
	| `Expression of expression_t
	| `Declaration of (identifier_t * expression_t option) list
]

and identifier_t = string

and function_t = identifier_t option * (identifier_t list) * t

and object_prop_t = oprop_t located

and oprop_t = [
	| `Property of string * expression_t
	| `Getter of string * function_t
	| `Setter of string * function_t
]

and expression_t = expr_t located

and expr_t = [
	| `This
	| `Null
	| `Undefined
	| `Elision
	| `Bool of bool
	| `Byte of int
	| `Number of float
	| `String of string
	| `Regexp of (string * string)
	| `Identifier of identifier_t
	| `Array of expression_t list
	| `Object of object_prop_t list
	| `New of expression_t * (expression_t list) option
	| `Typeof of expression_t
	| `Delete of expression_t
	| `Void of expression_t
	| `Function of function_t
	| `Plus of expression_t
	| `Preincr of expression_t
	| `Postincr of expression_t
	| `Predecr of expression_t
	| `Postdecr of expression_t
	| `Minus of expression_t
	| `Lnot of expression_t
	| `Bnot of expression_t
	| `Conditional of (expression_t * expression_t * expression_t)
	| `Sequence of expression_t list
	| `Assign of (expression_t * expression_t)
	| `Ashassign of (expression_t * expression_t)
	| `Property of (expression_t * expression_t)
	| `Dot of (expression_t * identifier_t)
	| `In of (expression_t * expression_t)
	| `Instanceof of (expression_t * expression_t)
	| `Add of (expression_t * expression_t)
	| `Sub of (expression_t * expression_t)
	| `Multiply of (expression_t * expression_t)
	| `Mod of (expression_t * expression_t)
	| `Divide of (expression_t * expression_t)
	| `Lsh of (expression_t * expression_t)
	| `Rsh of (expression_t * expression_t)
	| `Ash of (expression_t * expression_t)
	| `Lor of (expression_t * expression_t)
	| `Land of (expression_t * expression_t)
	| `Bor of (expression_t * expression_t)
	| `Band of (expression_t * expression_t)
	| `Bxor of (expression_t * expression_t)
	| `Equal of (expression_t * expression_t)
	| `Lt of (expression_t * expression_t)
	| `Gt of (expression_t * expression_t)
	| `Le of (expression_t * expression_t)
	| `Ge of (expression_t * expression_t)
	| `Sequal of (expression_t * expression_t)
	| `Call of expression_t * expression_t list
]
