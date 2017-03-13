open Printf

type loc = Lexing.position * Lexing.position
type 'a located = 'a * loc

(* Type of PHP program *)
type t = source_t list 

and source_t = [
 | `Topstatement of topstatement_t
 | `Namespace of ns_t
 | `Namespaceblock of ns_t option * t
 | `Use of (ns_t * string option) list
 | `Constant of (string * scalar_t) list
] located

(* Namespace \a\b\c *)
and ns_t = string list

and topstatement_t = [
 | `Halt
 | `Statement of statement_t
 | `Fundecl of string * bool * param_t list * topstatement_t list
 | `Classdecl of class_t
] located

and statement_t = [
 | `Block of topstatement_t list
 | `Break of int option
 | `Continue of int option
 | `Declare of (string * scalar_t) list * statement_t
 | `Do of statement_t * expr_t
 | `Echo of expr_t list
 | `Empty
 | `Expression of expr_t
 | `For of expr_t list * expr_t list * expr_t list * statement_t
 | `Foreach of expr_t * foreach_var_t * foreach_var_t option * statement_t
 | `Global of global_t list
 | `Goto of string
 | `If of expr_t * statement_t * (expr_t * statement_t) list * statement_t option
 | `Inlinehtml of string
 | `Label of string
 | `Return of expr_t option
 | `Static of (string * scalar_t option) list
 | `Switch of expr_t * case_t list
 | `Throw of expr_t
 | `Try of topstatement_t list * catch_t list * topstatement_t list option
 | `Unset of var_t list
 | `While of expr_t * statement_t
 | `Yield of yield_t
] located

and case_t = [
 | `Case of expr_t * topstatement_t list
 | `Default of topstatement_t list
]

and foreach_var_t = unit

and var_t = [
 | `Static
 | `Classname of ns_t
 | `Property of string
 | `Varproperty of expr_t
 | `Objprop of var_t * var_t
 | `Arrprop of var_t * expr_t option
 | `Metcall of var_t * callarg_t
 | `Variable of string
 | `Variablevar of var_t
 | `Compound of expr_t
 | `Staticmet of var_t * var_t
 | `Call of var_t * callarg_t
] located

and catch_t = (ns_t * string) * topstatement_t list

and param_t = string * bool * classhint_t option * scalar_t option

and classhint_t = [
 | `Array
 | `Callable
 | `Class of ns_t
]

and class_t = [
 | `Interface of string * ns_t list * clstatement_t list
 | `Class of string * classtype_t * ns_t option * ns_t list * clstatement_t list
]

and classtype_t = [
 | `Abstract
 | `Final
 | `Trait
 | `Class
]

and callarg_t = [
 | `Arglist of arg_t list
 | `Argyield of yield_t
]

and arg_t = [
 | `Argexpr of expr_t
 | `Argref of var_t
]

(* Class statement (property and method definitions) *)
and clstatement_t = unit

and macro_t = [`Line | `File | `Dir | `Trait | `Method | `Function | `Namespace | `Class]

and scalar_t = [
 | `Number of int
 | `Float of float
 | `String of string
 | `Macro of macro_t
 | `Array of (expr_t option * expr_t) list
 | `Const of ns_t
 | `Staticval of var_t * string
 | `Classconst of var_t * string
] located

and expr_t = [
 | `Closure of param_t list * bool * bool * (string * bool) list * topstatement_t list
 | `Reference of var_t
 | `Scalar of scalar_t
 | `Isset of var_t list
 | `Isempty of var_t
 | `Eval of expr_t
 | `Exit of expr_t option
 | `Print of expr_t
 | `Include of expr_t
 | `Includeo of expr_t
 | `Require of expr_t
 | `Requireo of expr_t
 | `Variable of var_t
 | `Listassign
 | `Assign of var_t * expr_t
 | `Plus_assign of var_t * expr_t
 | `Minus_assign of var_t * expr_t
 | `Mul_assign of var_t * expr_t
 | `Div_assign of var_t * expr_t
 | `Mod_assign of var_t * expr_t
 | `Dot_assign of var_t * expr_t 
 | `Lsh_assign of var_t * expr_t
 | `Rsh_assign of var_t * expr_t
 | `And_assign of var_t * expr_t
 | `Or_assign of var_t * expr_t
 | `Xor_assign of var_t * expr_t
 | `Assignref of var_t * var_t
 | `Clone of expr_t
 | `New of var_t
 | `Postincr of var_t
 | `Postdecr of var_t
 | `Preincr of var_t
 | `Predecr of var_t
 | `Land of expr_t * expr_t
 | `Lor of expr_t * expr_t
 | `Lxor of expr_t * expr_t
 | `Add of expr_t * expr_t
 | `Sub of expr_t * expr_t
 | `Dot of expr_t * expr_t
 | `Multiply of expr_t * expr_t
 | `Divide of expr_t * expr_t
 | `Mod of expr_t * expr_t
 | `Band of expr_t * expr_t
 | `Bor of expr_t * expr_t
 | `Bxor of expr_t * expr_t
 | `Lsh of expr_t * expr_t
 | `Rsh of expr_t * expr_t
 | `Plus of expr_t
 | `Minus of expr_t
 | `Lnot of expr_t
 | `Bnot of expr_t
 | `At of expr_t
 | `Equal of expr_t * expr_t
 | `Nequal of expr_t * expr_t
 | `Sequal of expr_t * expr_t
 | `Nsequal of expr_t * expr_t
 | `Gt of expr_t * expr_t
 | `Lt of expr_t * expr_t
 | `Le of expr_t * expr_t
 | `Ge of expr_t * expr_t
 | `Instanceof of expr_t * var_t
 | `Ternary of expr_t * expr_t option * expr_t
 | `Intcast of expr_t
 | `Boolcast of expr_t
 | `Stringcast of expr_t
 | `Floatcast of expr_t
 | `Arraycast of expr_t
 | `Objectcast of expr_t
 | `Unsetcast of expr_t
 | `Yieldexpr of yield_t option
] located

and global_t = [
 | `Variable of string
 | `Variablevar of var_t
 | `Variableexpr of expr_t
]

and yield_t = expr_t * expr_t option
