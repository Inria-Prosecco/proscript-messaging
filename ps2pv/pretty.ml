open Ast
open Printf
open Globals

let pad = (fun n->String.make (!pad_len*n) ' ')

let rec pretty_print d (program:Ast.t) = 
  let print acc = function
    | (`Statement(s),loc) -> acc^pretty_print_statement d s
    | (`FunctionDeclaration(f),loc) -> acc^(pad d)^(pretty_print_function d loc f)^"\n\n"
  in List.fold_left print "" program

and pretty_print_statement ?nopad:(np=false) d ((p,loc):statement_t) =
  let pl d ((s,loc):statement_t) = match s with `Block(_) -> d | _ -> d+1 in 
  let f = function
  | `Empty -> ";"
  | `Debugger -> "debugger;"
  | `Return(e) -> "return"^(match e with None->"" | Some v->" "^(pretty_print_exp d v))^";"
  | `Throw(e) -> "throw "^(pretty_print_exp d e)^";"
  | `Break(i) -> "break"^(match i with None->"" | Some v->" "^v)^";"
  | `Continue(i) -> "continue"^(match i with None->"" | Some v->" "^(recode_utf8 v))^";"
  | `Block(l) -> "{\n"^(pretty_print_statements (d+1) l)^(pad d)^"}";
  | `Label(l, s) -> (recode_utf8 l)^":\n"^(pretty_print_statement (d+1) s)
  | `Expression(e) -> (pretty_print_exp d e)^";"
  | `If(c,t,f) -> "if("^(pretty_print_exp d c)^")\n"^(pretty_print_statement (pl d t) t)
      ^(match f with None -> "" | Some s->(pad d)^"else"
      ^(match s with (`If(_),loc) -> " "^(pretty_print_statement ~nopad:true d s)
        | _ -> "\n"^(pretty_print_statement (pl d s) s)))
  | `Do(s,e) -> "do\n"^(pretty_print_statement (pl d s) s)^(pad d)^"while("^(pretty_print_exp d e)^");\n"
  | `While(e,s) -> "while("^(pretty_print_exp d e)^")\n"^(pretty_print_statement (pl d s) s)
  | `For(i,c,l,s) -> "for("^(match i with None->" " | Some (e,_) ->
      (match e with `Expression(f)->pretty_print_exp d ~inless:true f 
        | `Declaration(l)->"var "^List.fold_right (fun (i,v) b ->(recode_utf8 i) 
        ^(match v with None->""|Some v->" = "^(pretty_print_exp d ~commaless:true v))
        ^(if b="" then b else ", "^b)) l ""))^"; "
      ^(match c with None->"" | Some e->pretty_print_exp d e)^"; "
      ^(match l with None->"" | Some e->pretty_print_exp d e)^")\n"
      ^(pretty_print_statement (pl d s) s)
  | `Forin((i,_),e,s) -> "for("^(match i with `Expression(f)->pretty_print_exp d ~inless:true f 
      | `Declaration(l)->let (i,v)=List.hd l in "var "^(recode_utf8 i)^(match v with None->""|Some v->"="^(pretty_print_exp d ~inless:true v)))
      ^" in "^(pretty_print_exp d e)^")\n"^(pretty_print_statement (pl d s) s)
  | `With(e,s) -> "with("^(pretty_print_exp d e)^")\n"^(pretty_print_statement (pl d s) s)
  | `Switch(e,def,cases) -> "switch("^(pretty_print_exp d e)^")\n"^(pad d)^"{\n"
      ^(List.fold_left (fun a (e,l)->a^(pad (d+1))^"case "^(pretty_print_exp d e)^":\n"^(pretty_print_statements (d+2) l)) "" cases)
      ^(match def with None->"" | Some l->(pad (d+1))^"default:\n"^(pretty_print_statements (d+2) l))^(pad d)^"}\n"
  | `Try(b,c,f) -> "try\n"^(pretty_print_statement (pl d b) b)
      ^(match c with Some (i,c)->(pad d)^"catch("^i^")\n"^(pretty_print_statement (pl d c) c) | None->"")
      ^(match f with Some f->(pad d)^"finally\n"^(pretty_print_statement (pl d f) f) | None->"")
  | `Declaration(l) -> "var " ^ (List.fold_right
      (fun (i,v) b -> (recode_utf8 i)^(match v with None->""
      | Some v->" = "^(pretty_print_exp (d+1) ~commaless:true v))^(if b="" then b else ",\n"^(pad (d+1))^b)) l "")^";"
  | `Const(l) -> "const " ^ (List.fold_right
      (fun (i,v) b -> (recode_utf8 i)^(match v with None->""
      | Some v->" = "^(pretty_print_exp (d+1) ~commaless:true v))^(if b="" then b else ",\n"^(pad (d+1))^b)) l "")^";"
  in (if np then "" else pad d)^(f p)^"\n"

and pretty_print_statements d l = List.fold_left (fun a b->a^(pretty_print_statement d b)) "" l

and pt s b = if b then "("^s^")" else s

and pretty_print_exp ?commaless:(cl=false) ?inless:(il=false) d =
  let rec ppe d (input, loc) = match input with
  | `This -> ("this", 0)
  | `Null -> ("null", 0)
  | `Undefined -> ("undefined", 0)
  | `Elision -> ("undefined", 0)
  | `Bool(b) -> ((if b then "true" else "false"), 0) 
  | `Number(n) -> (cutdot (sprintf "%F" n), 0)
  | `String(s) -> ("\""^(recode_utf8 s)^"\"", 0)
  | `Regexp(r,f) -> ("/"^(recode_utf8 ~regexp:true r)^"/"^f, 0)
  | `Identifier(id) -> (recode_utf8 id, 0)
  | `Array(l) -> ((if List.length l > 0 then "[" ^ (pretty_print_elist (d+1) l) ^ "]" else "[]"), 0)
  | `Object(l) -> ((if List.length l > 0 then "{\n"^(pretty_print_object (d+1) l)^(pad d)^"}" else "{}"), 0)
  | `Function(f) -> ("("^(pretty_print_function (d+1) loc f)^")", 1)
  | `Dot(e,i) -> let (s,p)=ppe d e in ((pt s (p>1))^"."^(recode_utf8 i), 1)
  | `Property(e,f) -> let (s,p)=ppe d e in ((pt s (p>1))^"["^(pretty_print_exp d f)^"]",1)
  | `Call(f, l) -> let (s,p) = ppe d f in ((pt s (p>1))^"("^(pretty_print_elist d l)^")",1)
  | `New(e, f) -> let (s,p)=ppe d e in ("new "^(pt s (p>2))
      ^(match f with Some l->"("^(pretty_print_elist d l)^")" |None->""), 2)
  | `Preincr(e) -> let (s,p)=ppe d e in ("++"^(pt s (p>3)), 3)
  | `Predecr(e) -> let (s,p)=ppe d e in ("--"^(pt s (p>3)), 3)
  | `Postincr(e) -> let (s,p)=ppe d e in ((pt s (p>3))^"++", 3)
  | `Postdecr(e) -> let (s,p)=ppe d e in ((pt s (p>3))^"--", 3)
  | `Plus(e) -> let (s,p)=ppe d e in ("+"^(pt s (p>3)), 3)
  | `Minus(e) -> let (s,p)=ppe d e in ("-"^(pt s (p>3)), 3)
  | `Bnot(e) -> let (s,p)=ppe d e in ("~"^(pt s (p>3)), 3)
  | `Lnot(e) -> let (s,p)=ppe d e in ("!"^(pt s (p>3)), 3)
  | `Typeof(e) -> let (s,p)=ppe d e in ("typeof "^(pt s (p>3)), 3)
  | `Delete(e) -> let (s,p)=ppe d e in ("delete "^(pt s (p>3)), 3)
  | `Void(e) -> let (s,p)=ppe d e in ("void "^(pt s (p>3)), 3)
  | `Mod(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>6))^" % "^ (pt s2 (p2>3)), 4)
  | `Divide(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>6))^" / "^ (pt s2 (p2>4)), 5)
  | `Multiply(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>6))^" * "^ (pt s2 (p2>6)), 6)
  | `Add(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>7))^" + "^ (pt s2 (p2>7)), 7)
  | `Sub(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>7))^" - "^ (pt s2 (p2>7)), 7)
  | `Lsh(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>8))^" << "^ (pt s2 (p2>7)), 8)
  | `Rsh(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>8))^" >> "^ (pt s2 (p2>7)), 8)
  | `Ash(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>8))^" >>> "^ (pt s2 (p2>7)), 8)
  | `Lt(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>9))^" < "^ (pt s2 (p2>8)), 9)
  | `Gt(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>9))^" > "^ (pt s2 (p2>8)), 9)
  | `Le(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>9))^" <= "^ (pt s2 (p2>8)), 9)
  | `Ge(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>9))^" >= "^ (pt s2 (p2>8)), 9)
  | `In(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in (pt ((pt s1 (p1>9))^" in "^ (pt s2 (p2>8))) il, 9)
  | `Instanceof(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>9))^" instanceof "^ (pt s2 (p2>8)), 9)
  | `Equal(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>10))^" == "^ (pt s2 (p2>9)), 10)
  | `Sequal(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>10))^" === "^ (pt s2 (p2>9)), 10)
  | `Band(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>11))^" & "^ (pt s2 (p2>10)), 11)
  | `Bxor(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>12))^" ^ "^ (pt s2 (p2>11)), 12)
  | `Bor(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>13))^" | "^ (pt s2 (p2>12)), 13)
  | `Land(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>14))^" && "^ (pt s2 (p2>13)), 14)
  | `Lor(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>15))^" || "^ (pt s2 (p2>14)), 15)
  | `Conditional(c,e,f) -> let (s1,p1)=ppe d c and (s2,p2)=ppe d e and (s3,p3)=ppe d f in
    ((pt s1 (p1>16))^" ? "^ (pt s2 (p2>16)) ^ " : " ^ (pt s3 (p3>16)), 16)
  | `Assign(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>16))^" = "^ (pt s2 (p2>17)), 17)
	| `Ashassign(e,f) -> let (s1,p1)=ppe d e and (s2,p2)=ppe d f in ((pt s1 (p1>16))^" >>>= "^ (pt s2 (p2>17)), 17)
  | `Sequence(e) -> (pt (pretty_print_elist d e) cl, 18)
  | _ -> ("<unknown expression>", 0)
  in fun e -> fst (ppe d e)

and pretty_print_elist d = function
  | [h] -> (pretty_print_exp d ~commaless:true h)
  | h::t  -> (pretty_print_exp d ~commaless:true h)^", "^(pretty_print_elist d t)
  | [] -> ""

and pretty_print_function d ?decl:(h=true) loc ((n,args,b):function_t) = 
  (if h then "function "^(match n with Some(i)->(recode_utf8 i) | None->"") else "")
  ^"("^(pretty_print_elist d (List.map (fun s->(`Identifier(s),loc)) args))^")"
  ^(match List.length b with 0->"{" | _ -> "\n"^(pad d)^"{\n"^(pretty_print (d+1) b)^(pad d))^"}"

and pretty_print_object d = function
  | (h,loc)::t -> let p = match h with
    | `Property(p, v) -> "\""^(recode_utf8 p)^"\": "^(pretty_print_exp d ~commaless:true v)
    | `Getter(p,f) -> "get "^(recode_utf8 p)^(pretty_print_function d ~decl:false loc f)
    | `Setter(p,f) -> "set "^(recode_utf8 p)^(pretty_print_function d ~decl:false loc f)
    in (pad d) ^ p ^ (if List.length t>0 then "," else "") ^ "\n" ^ pretty_print_object d t
  | [] -> ""
