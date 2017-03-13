open Printf

type loc = Lexing.position * Lexing.position
type 'a located = 'a * loc

(* Type of PHP program *)
type t = node_t list 

and node_t = [
 | `Text of string
 | `Tag of string * attr_t list * t
] located

and attr_t = (string * string) located
