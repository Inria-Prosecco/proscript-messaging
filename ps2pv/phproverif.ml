open Phpast
open Printf
open Globals
open Error
open Url

let ua = ref []
let pn = ref ""
let number_c = ref [0,("number_zero",true)]
let string_c = ref ["",("string_empty",true)]
let float_c = ref [0.,("float_zero",true)]
let spec_c = ref ""

let pstyle n = String.capitalize (String.lowercase (Filename.chop_extension (Filename.basename n)))
type continuation = {pad:int; mem: string; ret: string}
let uk ?pad:(p=None) ?mem:(m=None) ?ret:(r=None) k =
  {pad = (match p with None->k.pad | Some i->i);
   mem = (match m with None->k.mem | Some l->l);
   ret = (match r with None->k.ret | Some r->r)}
let init_k = {pad=0; mem=""; ret=""}

let http_get = ref []
let http_post = ref []
let http_param t p =
  let (tag,l) = (match t with
    | "_GET" -> "get_", http_get
    | "_POST" |_ -> "post_", http_post) in
  let p = tag^p in
  (try let _ = List.find ((=) p) !l in () with Not_found -> l := p::!l); p

let mktu s = fun t l->
  try let _ = List.assoc t !s in ()
  with Not_found -> s := (t,l)::!s

let concat l = List.fold_left (^) "" l
let rec implode c = function
	| [] -> "" | [x] -> x | h::t -> h^c^(implode c t)  

let tables = ref []
let uptables = mktu tables
let rconst = ref []
let uprconst = mktu rconst

let mkconst s = sprintf "dataConst_%d" (Hashtbl.hash s)
let mkpconst p = let r = mkconst (implode "&" (List.sort compare p)) in uprconst r p; r

let cache_fresh prefix =
  fun l v ->  try fst (List.assoc v !l)
	with Not_found -> let t = sprintf "%s%d" prefix (Hashtbl.hash v)
	  in l := (v,(t,false))::!l; t 

let float_lit = let f = cache_fresh ("flt"^(!pn)^"_") float_c in fun x -> f x
let number_lit = let f = cache_fresh ("num"^(!pn)^"_") number_c in fun x -> f x
let string_lit = let f = cache_fresh ("str"^(!pn)^"_") string_c in fun x -> f x

let mkctr prefix = let c = ref 0 in fun () -> incr c; sprintf "%s_%d" prefix !c
let valloc = mkctr "val"

let was_if = ref false
let pad = (fun n->String.make (!pad_len*n) ' ')

let nop = function
  | `Mod(_) -> "modulo" | `Divide(_) -> "divide" | `Multiply(_) -> "multiply"
  | `Sub(_) -> "substract" | `Lsh(_) -> "left_shift" | `Rsh(_) -> "right_shift"
  | `Band(_) -> "bit_and" | `Bxor(_) -> "bit_xor" | `Bor(_) -> "bit_or"
  | `Add(_) -> "add" | `Dot(_) -> "concat" | _ -> "error"

let rec pv_prog k (program:t) =
  let l = List.map (fun (s,loc)->match s with `Topstatement(s)->s
    | _ -> syn_error loc "Namespaces and constants are not supported") program in
  pv_statements k l

and pv_topstatement k ((s,loc):topstatement_t) = match s with
  | `Statement(s) -> pv_statement k s
  | _ -> failwith "Unsupported top level statement"

and pv_statement k stm =
  let pp = pad (k.pad) in 
  if !was_if then failwith "Cannot compose if blocks sequencially, use ternary for expressions";
  let rec f ((p,loc):statement_t) = match p with
  | `Empty -> ""
  | `Echo([x]) -> let (u,v) = pv_exp k x in
    u^pp^"out(httpServerResponse, (url,httpOk("^v^"),cookie_jar, corr));\n"
  | `Expression(e) -> (fst (pv_exp k e))
  | `Block(l) -> pp^"(\n"^(pv_statements (uk ~pad:(Some(k.pad+1)) k) l)^pp^")\n"
  | `If((`Variable(`Call((`Classname(["get_table"]),_),`Arglist([`Argexpr((`Scalar(`Array((None,(`Scalar(`String(table),_),_))::l),_),_))])),_),_), s, [], Some t) ->
		let (_,l) = List.split l in
    let l = List.map (function (`Reference(v),_)->pv_var k v | e->let (u,v)=pv_exp k e in (u,"="^v)) l in
    let (u,v) = List.split l in
    let u = concat u and v = implode "," v in
    let res = u^pp^"get "^table^"("^v^") in\n"^(f s)^pp^"else\n"^(f t) in
    was_if := true; uptables table l; res
  | `If((`Isset(l),_),s,[], Some t) ->
    let n = List.length !http_get and m = List.length !http_post in
    let _= List.split (List.map (pv_var k) l) in
    let u = List.length !http_get and v = List.length !http_post in
    if n*(u-n) + m*(v-m) > 0 then syn_error loc "cannot initialize script parameters in multiple steps";
		if u>n && v>m then syn_error loc "cannot initialize get and post variables together";
		let res = pp^"let "
     ^(if u>n then sprintf "%s(%s) = query_string" (mkpconst !http_get) (implode "," !http_get)
       else sprintf "httpPost(%s(%s)) = method" (mkpconst !http_post) (implode "," !http_post))
     ^" in\n"^(f s)^pp^"else\n"^(f t)
		in was_if := true; res
  | `If(c,s,[],Some t) ->
    let (u,v) = pv_cond k c in
    let res = u^pp^"if "^v^" then\n"^(f s)^pp^"else\n"^(f t) in
    was_if := true; res
  | _ -> syn_error loc "unsupported statement"
  in (f stm)

and pv_statements k l =
  was_if := false;
  let res = List.fold_left (fun a b->a^(pv_topstatement k b)) "" l
  in if !was_if then res else res^(pad k.pad)^"0\n"

and pv_cond k (input,loc) =
  let compose e f t pvf =
    let (a,b) = pvf k e and (u,v) = pvf k f in (a^u, "("^b^") "^t^" ("^v^")")
  in match input with
  | `Equal(e,f) | `Sequal(e,f) -> compose e f "=" pv_exp
  | `Nequal(e,f) | `Nsequal(e,f) -> compose e f "<>" pv_exp
  | `Land(e,f) -> compose e f "&&" pv_cond
  | `Lor(e,f) -> compose e f "||" pv_cond
  | `Lnot(e) -> let (u,v) = pv_cond k e in (u,"not ("^v^")")
  | _ -> syn_error loc "invalid boolean condition"

and pv_exp k expr =
 let ct (u,e) (v,f) c = (u^v, sprintf "%s(%s,%s)" c e f) in
 let pp = pad (k.pad) in
 let rec pve ((input, loc):expr_t) = match input with
  | `Require(_) | `Include(_) | `Requireo(_) | `Includeo(_) -> ("","string_empty()")
  | `Scalar(s) -> ("",pv_scalar k s)

  | `Add(e,f) | `Sub(e,f) | `Multiply(e,f) | `Mod(e,f) | `Divide(e,f)
  | `Lsh(e,f) | `Rsh(e,f) | `Bor(e,f) | `Band(e,f) | `Bxor(e,f) | `Dot(e,f) ->
    let op = nop input in ct (pve e) (pve f) op

  | `Variable(v) -> pvv v
	| `Assign((`Arrprop((`Variable("_SESSION"),_),Some(`Scalar(`String(p),_),_)),_), e) -> 
		let (u,v) = pve e in (u^pp^"insert serverSessions(host, session_cookie, sessionPair("^(string_lit p)^","^v^"));\n", "bool_true()")
  | `Assign(v,e) -> let (u,v) = pve e and (a,b) = pvv v in
    (u^a^pp^"let "^b^" = "^v^" in\n", b)
  | `Exit(_) -> (pp^"out(httpServerResponse, (url,httpError(),cookie_jar, corr));\n","string_empty()")
  | _ -> syn_error loc "unsupported expression"
 and pvv = pv_var k in pve expr

and pv_var k ((input, loc):var_t) =
  let pp = pad (k.pad) and pve = pv_exp k and pvv = pv_var k in
  match input with
  | `Variable(v) -> ("","var_"^v)
  | `Call((`Classname([f]),_),`Arglist(l)) ->
    let lp = List.map (function `Argexpr(e) -> pve e | `Argref(v) -> pvv v) l in
    let (al, bl) = List.split lp in
    let a = concat al and b = implode "," bl in
    (match f with
    | "template" -> (match l with
      | (`Argexpr(`Scalar(`String(tpl),_),_))::l -> let c = mkconst tpl in
        let lp = List.map (function `Argexpr(e) -> pve e | `Argref(v) -> pvv v) l in
        let (a, b) = List.split lp in pvhtml c (List.map (fun _->valloc()) l) tpl;
        uprconst c b; (concat a, c^"("^(implode "," b)^")")
      | _ -> syn_error loc "sprintf expects a template as first argument")
    | "my_url" -> ("","serializeUri(url)")
    | "redirect" -> (a^pp^"out(httpServerResponse, (url,httpRedirect(parseUri("^b^")),nullCookie(), corr));\n","string_empty()")
    | "session_start" ->
      let al = ["let cookiePair(session_cookie,path_cookie) = cookie_jar in";
        "if protocol(url) = https() && secure(session_cookie) <> nullCookie() then"] in
      (concat (List.map (fun x->pp^x^"\n") al),"bool_true()")
		| "insert_table" -> (match l with
      | (`Argexpr(`Scalar(`String(tbl),_),_))::l -> (a^pp^"insert "^tbl^"("^(implode "," (List.tl bl))^");\n", "bool_true()")
		  | _ -> syn_error loc "insert_table expects the table name as first argument")
    | _ -> (a,f^"("^b^")"))
  | `Arrprop((`Variable(n),_),Some(`Scalar(`String(p),_),_)) when n="_GET" or n="_POST" -> ("", http_param n p)
  | `Arrprop((`Variable(n),_),Some(`Scalar(`String(p),_),_)) when n="_SESSION" ->
    let v = valloc() in (pp^"get serverSessions(=host, =session_cookie, sessionPair("^(string_lit p)^","^v^")) in\n",v)
  | _ -> syn_error loc "unsupported memory path"

and pv_scalar k (scalar, loc) =
  match scalar with
  | `Number(n) -> number_lit n
  | `Float(f) -> float_lit f
  | `String(s) -> string_lit s
	| `Const([t]) -> (match String.lowercase t with
		| "true" -> "bool_true()"
		| "false" -> "bool_false()"
		| _ -> syn_error loc ("unsupported const: "^t)) 
  | _ -> syn_error loc "unsupported scalar"

and parseurl s r =
  let u = parse_url s in
  (*let pp = String.lowercase (pstyle (reparam p up)) in*)
  sprintf "(%s,%s,%s,%s)"
    (match u.protocol with "http" -> "http()" | "https" -> "https()" | _ -> "protocol(u)")
    (match u.domain_name with "" -> "host(u)" | a -> string_lit a)
    (match u.query_path with "" -> "path(u)" | a -> string_lit a)
    (match u.query_string with [] -> "nullParams()" | l -> let (l,v) = List.split l in sprintf "%s(%s)" (mkpconst l) (implode "," (List.map string_lit v)))

and pvhtml const params html =
  let inbuf = Ulexing.from_utf8_string html in
  let parsed = menhir_with_ulex Htmllexer.main Htmlparser.start inbuf in
	let explicitparam u lit =
		if (String.length u) > 2 && u.[0] = '@' && u.[1] = '$' then
			List.nth params (int_of_string (String.sub u 2 ((String.length u)-2)))
		else (if lit then string_lit u else u) in
  let reparam s l = let rex = Pcre.regexp "%[sdx]" in
    let i = ref params in
    let sub m = (try let x = List.hd !i in i := List.tl !i;
      l := x::!l; m with Not_found -> failwith "Invalid % in HTML output!") in
    Pcre.substitute ~rex:rex ~subst:sub s in
	let rec inputs = function [] -> []
  | (`Text(_),_)::r -> inputs r
  | (`Tag(t, a, l),_)::r -> (
		let a = List.map (fun (x,y)->x) a in
		match t with
    | "input" ->
			 let n = try List.assoc "name" a with Not_found -> "" in
			 let v = try Some (List.assoc "value" a) with Not_found -> None in
			 if n = "" then inputs r else (n,v)::(inputs r) 
    | _ -> (inputs l)@(inputs r)) 
  in let rec aux = function [] -> ()
  | (`Text(_),_)::r -> aux r
  | (`Tag(t, a, l),_)::r -> (
		let a = List.map (fun (x,y)->x) a in
    match t with
		| "form" -> let fields = List.sort (fun (a,b) (c,d)->compare b d) (inputs l) in
		  let met = try String.lowercase (List.assoc "method" a) with Not_found -> "get" in
			let t = try parseurl (explicitparam (List.assoc "target" a) true) reparam with Not_found -> "u" in
			let inp = List.map (fun (x,v)->match v with
				| None->"get userInput(="^(string_lit x)^", in_"^(string_lit x)^") in\n"
				| Some(_) -> "") fields in
		  let dconst = (mkpconst (List.map (fun (a,b)->a) fields))
        ^"("^(implode "," (List.map (fun (x,v)-> match v with Some(w) -> explicitparam w false
				| None -> "in_"^(string_lit x)) fields))^")" in
			let tu = (match met with
				| "get" -> "let (proto,dom,path,query_string) = "^t^" in\n"
				  ^"let target = (proto,dom,path,"^(dconst)^") in\n"
			  | _ -> "let target = "^t^" in\n") in
			ua := (tu^"let "^const^"("^(implode "," params)^") = d in\n"
       ^(concat inp)^"out(pageClick(b),(p,target,"^
			 (match met with "get" -> "httpGet()" | _ -> "httpPost("^dconst^")")
			 ^"))\n") :: (!ua)
    | "a" -> (try
      let p = List.assoc "href" a in 
      ua := ("let "^const^"("^(implode "," params)^") = d in\n"
       ^"out(pageClick(b),(p,"^(parseurl p reparam)^",httpGet()))\n") :: (!ua)
     with Not_found -> fprintf stderr "Warning: link without target!\n") 
    | _ -> ()
   ); aux l; aux r
  in aux parsed

let pv p =
  pn := (try pstyle !ifile with Invalid_argument(_) -> "Stdin");
  let cst l = concat (List.map (fun (v,(x,c))->if c then "" else "free "^x^":bitstring.\n") (List.rev l)) in
  (cst !number_c)^(cst !string_c)^(cst !float_c)^(!spec_c)
  ^(concat (List.map (fun (n,l)->"fun "^n^"("^(implode "," (List.map (fun _->"bitstring") l))^"):bitstring [data].\n") !rconst))
  ^"fun "^(String.lowercase !pn)^"Path():Path.\nfree host1:Host.\n"
	^(match !ua with [] -> "" | l ->
   "\nlet "^(!pn)^"UserAgent(b:Browser) =\n(\n"
   ^(implode "|\n" (List.map (fun t->"(in (newPage(b),(p:Page, u:Uri, d:bitstring));\n"
	   ^"if host(u) = host1 && path(u) = "^(String.lowercase !pn)^"Path() then\n"^t^")\n") !ua))^").\n\n")
  ^"let "^(!pn)^"ServerApp(host:Host, app:Path) =\n(\n"
  ^"  in(httpServerRequest, (url:Uri, headers:Headers, method:HttpRequest, corr:bitstring));\n"
  ^"  let uri(proto, =host, ="^(String.lowercase !pn)^"Path(), query_string) = url in\n"
  ^"  let cookie_jar = getCookie(headers) in\n"
  ^(pv_prog (uk ~pad:(Some(1)) init_k) p)^").\n"

