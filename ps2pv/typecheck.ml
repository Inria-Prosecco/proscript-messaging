open Globals
open Ast
open Printf
open Error

module Smap = Map.Make(String)

(* Types *)
type type_t = 
  | Number
  | Byte
  | Boolean
  | String
  | Undefined
  | Arrow of type_t list * type_t option * type_t * type_t option
  | Array of type_t * int ref * bool ref
  | Object of type_t Smap.t ref * bool ref
  | Variable of type_v

(* Type variable *)
and type_v = {id : int; mutable def : type_t option; mutable ng : bool}

(* Set of type variables *)
module V = struct
 type t = type_v
 let compare v1 v2 = compare v1.id v2.id
 let create = let r = ref 1 in fun ()->(incr r; {id = !r; def = None; ng = false})
end

module Vset = Set.Make(V)

(* Type table *)
let type_table = Hashtbl.create 8092

(* Fresh variable *)
let fresh () = Variable(V.create ())

(* Type schema *)
type schema_t = {vars : Vset.t ; t : type_t}

(* Flags for environment variables *)
type env_flag =
	| Normal
	| With_binding
	| Notry of bool ref
	| Return of bool ref
	| Bound_accessor of string

(* Typing environment *)
type env = {bv : (identifier_t * (schema_t * env_flag)) list ; fv : Vset.t ; with_obj: (type_t Smap.t ref * bool) option}
let se s = failwith (sprintf "Typecheck error: %s at <loc>" s)
let env_init = {bv = []; fv = Vset.empty; with_obj=None}

(* Convert resolved type variables to their corresponding type *)
let rec head = function
 | Variable({id = k; def = Some t; ng = _}) -> head t
 | Arrow(tl,this,t,et) -> Arrow(List.map head tl,
   (match this with Some t->Some(head t) | None->None), head t,
     (match et with None->None | Some t->Some(head t)))
 | Array(t,n,f) -> Array(head t,n,f)
 | Object(t,f) -> t:=Smap.map head !t; Object(t,f) 
 | t -> t

(* Pretty print types *)
let print_type t = let c = ref 97 and ids = ref [] in
  let par s = function Arrow(_)->"("^s^")" | _->s in
  let rec aux = function
  | Number -> "number"
  | Byte   -> "byte"
  | Boolean -> "boolean"
  | String -> "string"
| Undefined -> "undefined"
  | Arrow(tl, this, t, et) -> let u = List.map aux tl and v = aux t in
	   let tr = match this with None->"" | Some(tt)->"["^(aux tt)^"]" in
     (sprintf "(%s)%s -> %s" (List.fold_left (fun x y->(if x="" then x else x^", ")^y) "" u) tr v)^
		  (match et with None->"" | Some(t) -> "[throws "^(aux t)^"]")
  | Array(t,n,f) -> sprintf "%s array[%s%d]" (par (aux t) t) (if !f then "" else ">=") !n
  | Object(tl,f) -> let p = Smap.fold (fun i t x->(if x="" then x else x^", ")^"\""^(recode_utf8 i)^"\":"^(aux t)) !tl ""
	  in "{"^p^"}"^(if !f then " final" else "")
  | Variable(t) -> let tid = try snd (List.find (fun (u,v)->u=t.id) !ids)
    with Not_found -> let a=String.make(1) (char_of_int !c) in (incr c; ids := (t.id,a)::!ids; a)
    and ng_flag = if t.ng then "_" else "" in sprintf "'%s%s" ng_flag tid
  in aux (head t)

(* Checks if a given type variable occurs in a given type *)
let rec occur var = function
 | Variable(i) when i=var -> true
 | Arrow(tl, th, t, et) -> (List.fold_left (fun x t->x||(occur var t)) false tl)
   || (occur var t) || (match et with None->false | Some t->occur var t)
 | Array(t,_,_) -> occur var t
 | Object(t,_) -> Smap.fold (fun i t x -> x || (occur var t)) !t false 
 | _ -> false

exception Arity_mismatch of int * int
exception Type_mismatch of type_t * type_t
exception Dependant_type of type_t * type_t
exception Unification_error of type_t * type_t
exception Exception_mismatch of type_t * type_t

(* Unify type variables *)
let rec unify ?objdep:(dep=false) ?ufin:(uf=false) ?assign:(assign=false) type1 type2 =
 let rec uni t1 t2 = if !verbose then eprintf "UNIFY(%s,%s)\n%!" (print_type t1) (print_type t2);
 match (head t1, head t2) with
 | (u, v) when u=v -> ()
 | (Arrow(u, this1, v, eta), Arrow(a, this2, b, etb)) ->
	 (try (List.iter (fun (x,y)->uni x y) (List.combine u a);
	   (match (eta, etb) with (None,None) -> ()
		  | (Some t1, Some t2) -> unify t1 t2
			| _ -> raise (Exception_mismatch(t1,t2)) );
	   (match (this1,this2) with (Some a, Some b)->unify ~objdep:true a b
		 | (None, Some _) | (Some _, None) when not dep -> raise (Unification_error(t1, t2))
		 | _ -> ()); uni v b)
    with Invalid_argument(_) -> raise (Arity_mismatch(List.length u, List.length a)))
 | (Array(u,n,f), Array(v,m,g)) -> uni u v; let b = max !n !m in
   if (b > !n && !f) || (b > !m && !g) then raise (Unification_error(t1, t2));
   n := b; m := b;
   if assign && (!f || !g) then (f := true; g := true)
 | (Object(u,f), Object(v,g)) -> 
   let w = Smap.merge (fun k t1 t2->match (t1,t2) with (None,None)->None
     | (Some t1, Some t2)->uni t1 t2; Some(head t1) | (Some t,_) | (_,Some t) -> Some(head t)) !u !v in 
	 (match (!f,!g) with
	 | (true,true) -> if not (Smap.equal (fun t1 t2->uni t1 t2; true) !u !v) then raise (Unification_error(t1, t2))
	 | (false, false) -> u:=w; v:=w
	 | (b1, b2) -> if Smap.cardinal w > Smap.cardinal (if b1 then !u else !v) then raise (Unification_error(t1, t2));
	   if not dep then (if not b1 then (*v:=w else*) u:=w)); if uf || assign then (f := !f || !g; g := !f)
 | (Variable(u), Variable(v)) -> u.def <- Some (Variable(v))
 | (Variable(t), v) | (v, Variable(t)) -> if occur t v then raise (Dependant_type(v,Variable(t))); t.def <- Some v
 | _ -> raise (Type_mismatch(t1,t2))
 in uni type1 type2

(* Compute free type variables *)
let rec fvars t = match head t with
 | Variable(t) -> Vset.singleton t
 | Arrow(tl, _, t2, _) -> let afv = List.fold_left (fun e t->Vset.union e (fvars t)) Vset.empty tl in Vset.union afv (fvars t2)
 | Array(t,_,_) -> fvars t
 | Object(p,_) -> Smap.fold (fun i t e->Vset.union e (fvars t)) !p Vset.empty 
 | _ -> Vset.empty

let env_add e t env ?flags:(fl=Normal) gen =
 let update_fv var set = Vset.union (Vset.singleton var) set in
 let fv_env = Vset.fold update_fv env.fv Vset.empty in
 let fv_type = fvars t in if gen=1 then Vset.iter (fun t->t.ng<-true) fv_type ;
 let fv_s = (if gen=2 then (Vset.diff fv_type fv_env) else Vset.empty) in
 Vset.iter (fun t->t.ng<-false) fv_s ; let s = {vars = fv_s; t = t} in
 {bv = (e,(s,fl))::env.bv; fv = if gen<2 then Vset.union fv_env fv_type else fv_env; with_obj=env.with_obj}

let rec replace_var v fresh = function
 | Variable(t) when t=v -> if not t.ng then Variable(fresh) else Variable(t)
 | Arrow(t1, this, t2, et) -> Arrow(List.map (fun t->replace_var v fresh t) t1, this,
   replace_var v fresh t2, match et with None->None | Some t->Some(replace_var v fresh t))
 | Array(t,n,f) -> Array(replace_var v fresh t,n,f)
 | t -> t
 
let free_env = ref []
let this = ref None
let toplevel = ref true

let bound_type schema = {vars = Vset.empty; t = schema}

exception Not_in_try
exception Free_in_with
exception Not_closed of string

let env_find ?global:(glob=false) ?throw:(thr=false) f env =
 if f = "this" then (
	 match !this with None->let v = Object(ref Smap.empty, ref false) in
	 this := Some(v); (v,Normal) | Some(v)->(v,Normal)
 )
 else let (s,flags) =
 try
	List.assoc f ((if glob then [] else env.bv) @ (match env.with_obj with None -> !free_env | _ -> []))
 with
  Not_found ->
   if thr then raise Not_in_try;
   let rec v = V.create () and tv = Variable(v) in
   let fs = {vars = Vset.empty; t = tv} in
   (match env.with_obj with
    | Some(l,final) -> if final then raise Free_in_with; l := Smap.add f tv !l; (fs,Normal)  
    | None -> if !closed then raise (Not_closed f); free_env := (f, (fs,Normal))::!free_env; (fs,Normal))
 in let instance v t = let fresh = V.create() in replace_var v fresh t
 in let t = head s.t
 in (Vset.fold instance s.vars t),flags

let last l = List.hd (List.rev l)
let lasttype l = if List.length l > 0 then last l else Undefined

let is_from_with env =
	let rec aux = function
		| `Identifier(i) when snd (env_find i env)=With_binding -> true
		| `Property((e,_),_) -> aux e
		| _ -> false
	in aux 

let exception_handler e = function
 | Arity_mismatch(i,j) -> type_error e (sprintf "function called expected %d arguments, received %d." i j)
 | Type_mismatch(t1,t2) -> type_error e (sprintf "type <%s> was expected but got <%s>." (print_type t1) (print_type t2))
 | Exception_mismatch(t1,t2) -> type_error e (sprintf "the exception types of %s and %s are incompatible." (print_type t1) (print_type t2))
 | Dependant_type(t1,t2) -> type_error e (sprintf "the recursive type %s = %s is not supported." (print_type t2) (print_type t1)) 
 | Unification_error(t1, t2) -> type_error e (sprintf "cannot unify types <%s>\nand\n<%s>.\nThis can be due to mismatched property of objects, or unsatisfied length conditions on arrays and strings." (print_type t2) (print_type t1))
 | Free_in_with -> type_error e "free variables are not allowed in a scope frame loaded from final <with> object."
 | Not_closed(s) -> type_error e (sprintf "access to global variable %s in closed program." s)
 | x -> raise x 

let rec check_prog env p = List.iter (check_src env) p

and check_src env (prog,loc) = match prog with
  | `FunctionDeclaration(_) -> syn_error loc "Functions can only be declared in expressions"
  | `Statement(s) -> check_statement env s

and check_statement env (statement,loc) =
	let chke e = head (check_expr env e)
	and chks s = check_statement env s 
	in try match statement with
	| `Block(l) -> List.iter chks l
  | `With(e,s) -> let t = chke e and props = ref Smap.empty and fin = ref false in unify ~ufin:true (Object(props, fin)) t;
	  let newe = Smap.fold (fun i v e->if i="this" then e else env_add ~flags:With_binding i v e 0)
		  !props ({bv=List.filter (fun (a,b)->try a.[0]='@'&&a.[1]='_' with _->false) env.bv;fv=Vset.empty;with_obj=Some(props,!fin)})
			in check_statement newe s
	| `Try(s, Some(i,catch), fi) ->
		let et = fresh () in let newe = env_add "@_try" et env 0 in
		check_statement newe s; let newe = env_add i (head et) env 0 in check_statement newe catch;
		(match fi with None -> () | Some(st) -> check_statement env st)
	| `Throw(e) -> let t = chke e in let (et,tf) = try env_find ~throw:true "@_try" env
	  with Not_in_try -> syn_error loc "Cannot use throw outside try block or function" in 
		(match tf with Notry(r)->r:=true | _ -> ()); unify t et  
  | `Empty -> ()
  | `Continue(None) | `Break(None) -> () 
  | `Expression(e) -> let _ = chke e in ()
  | `Return(e) -> if !toplevel then syn_error loc "Cannot use return statement at top level";
	  let t = (match e with Some(expr) -> chke expr | None -> Undefined) in
		let (ret,f) = env_find "@_return" env in (match f with Return(r)->r:=true | _ -> ()); unify ret t
  | `If(e,s,so) -> let t1 = chke e in unify t1 Boolean; chks s;
      (match so with Some(s) -> chks s | None -> ())
  | `Do(s,e) | `While(e,s) -> unify (chke e) Boolean; chks s
  | `For(i,c,o,s) ->
		(match (i,c,o) with
		| (Some (`Expression(
			 (`Assign((`Identifier(i),loc_i),inite),loc_ia)
			 ),_), Some (`Lt( (`Identifier(j),_),
				 (`Dot((`Identifier(k),loc_k),"length"),_)
				),_), Some((`Postincr(`Identifier(n),_),_))) when i=j && i=n ->
			(match inite with (`Number(k),_) when k >= 0. && float_of_int (truncate k)=k -> ()
			 | (`Ash((`Identifier(i),_),(`Number(0.),_)),_) -> ()
			 | _ -> syn_error loc_ia "Invalid initial value for dynamic accessor");  
			unify (chke (`Identifier(i),loc_i)) Number;
			let t=chke (`Identifier(k),loc_k) in
			(match t with String(_)->() | _ -> unify t (Array(fresh(),ref 0, ref false)));
			let ne = env_add ~flags:(Bound_accessor(k)) i Number env 0 in check_statement ne s
		| _ -> (match i with None->() | Some(`Expression(e),_) -> let _ = chke e in ()
			 | Some(_,loc) -> syn_error loc "All local variables must be declared at the top of the function");
      (match c with Some(e)->unify (chke e) Boolean | None->());
      (match o with Some(e)->let _=chke e in () | None->()); chks s)
  | `Switch(e, def, cases) -> let t = chke e in
	    (match def with Some(sl)->chks (`Block(sl),loc) | None->());
      List.iter (fun (e,sl)->unify (chke e) t; chks (`Block(sl),loc)) cases
	| `Declaration(l) | `Const(l) when !toplevel ->
		  eprintf "Warning: declaration accepted outside function (typing as a library) at %s.\n%!" (format_loc loc); 
		  List.iter (fun (i,v) -> match v with Some(e) -> let _ = chke (`Assign((`Identifier(i),loc),e),loc) in () | None->()) l
	| `Declaration(l) | `Const(l) when not !toplevel -> syn_error loc "All local variables must be declared at the top of the function"
  | st -> let s = Pretty.pretty_print_statement 0 (st,loc) in syn_error loc ("The following statement:\n"^s^"is not supported") 
 with a -> exception_handler (ES(statement,loc)) a

and is_fully_reachable prog = match prog with
  | [`Statement(s),_] -> is_fully_reachable_statement s
  | _ -> false

and is_fully_reachable_statement ((s,loc):statement_t) : bool = match s with
  | `Return(_) -> true
  | `If(_, s1, Some(s2)) -> (is_fully_reachable_statement s1) && (is_fully_reachable_statement s2)
  | `Switch(_, Some(def), cases) -> (List.for_all (fun x->is_fully_reachable_statement (`Block(snd x),loc)) cases) && (is_fully_reachable_statement (`Block(def),loc))
  | `Block(sl) ->
     let rec valid_seq = function
     | (`Return(_),_)::_ -> true
     | (`Expression(_),_)::r -> valid_seq r
     | [s] -> is_fully_reachable_statement s
     | _ -> false
     in valid_seq sl
  | `For(_,_,_,s) | `Do(s,_) | `While(_,s) -> is_fully_reachable_statement s
  | _ -> false

and check_function env (iname, args, t) =
	let selfvar = fresh() and old_this = !this and old_toplevel = !toplevel in
	this:=None; toplevel:=false;
	let ne = match iname with None -> env | Some(i) -> env_add i selfvar env 0 in  
	let av = List.map (fun _->fresh()) args in
	let final = fresh () and hasreturn = ref false and hasthrow = ref false in
	let targs = (List.combine args av) and etype = fresh() in
	let ae = List.fold_left (fun e (i,v)->env_add i v e 0) ne targs in
	let ae = env_add "@_return" final ~flags:(Return(hasreturn)) ae 0 in
	let ae = env_add "@_try" etype ae ~flags:(Notry(hasthrow)) 0 in
	let declenv = ref ae in (* Must be imperative here because declarations are added incrementally *)
	let rec start = function
		| (`FunctionDeclaration(_),loc) :: t -> syn_error loc "Functions are only allowed as expressions"
		| (`Statement((`Declaration(decl),loc2)),_) :: t ->
			let declare (i,e) = let t = match e with None -> Undefined | Some(e) -> check_expr !declenv e
			  in declenv := env_add i t !declenv (match t with Arrow(_,_,_,_)->1 | _->1);
		  in List.iter declare decl; start t
		| (`Statement((`Const(decl),loc2)),_) :: t ->
			let declare (i,e) = let t = match e with None -> Undefined | Some(e) -> check_expr !declenv e
			  in declenv := env_add i t !declenv (match t with Arrow(_,_,_,_)->1 | _->1);
		  in List.iter declare decl; start t
		| _ as s -> s
	in let fbody = start t in check_prog !declenv fbody;
	let fthis = !this in this:=old_this; toplevel:=old_toplevel;
	if not !hasreturn or (head final)=Undefined then unify final Undefined else (
     match List.hd (List.rev fbody) with (`Statement(`Return(_),_),_) -> ()
     | (_,loc) -> if is_fully_reachable fbody then () else syn_error loc "Functions with a return type must end with a return statement.");
	let et = if !hasthrow then Some(etype) else None in 
	let ftype = Arrow(List.map head av, fthis, head final, et) in
	unify selfvar ftype; head ftype

and check_expr ?lhs:(lh=false) env =
	let rec chke ?lhs:(lh2=lh) e = check_expr ~lhs:lh2 env e and chkel l = List.map chke l
  in fun ((expr,loc):Ast.expression_t) ->
  if lh then (match expr with `Identifier(_)|`Dot(_)|`Property(_)->()
	 | _ -> type_error (EE(expr,loc)) "invalid expression on the left hand side of an assignment.");
	try let result = match expr with
	| `Identifier(i) -> let (t,f)=env_find i env in
	  (match (f,lh) with (Bound_accessor(_),true)->type_error (EE(expr,loc)) "Variables from bound accessors are read only." | _ -> t)
  | `Regexp(_) -> type_error (EE(expr,loc)) "Regular expression literals are not allowed."
	| `Number(_) -> Number | `Byte(_) -> Byte | `Bool(_) -> Boolean | `String(s) -> String
	| `Plus(e) -> let t = chke e in unify t String; Number
  | `Minus(e) -> let t = chke e in unify t Number; Number
  | `Lnot(e) -> let _ = chke e in Boolean
  | `Bnot(e) -> let t = chke e in unify t Number; Number

	| `Object(p) -> let props = (List.fold_left (fun p->function
		(`Property(i,e),loc) ->
			if (i="length") then (
				eprintf "Warning: object contains a length property at %s. It must not be accessed first during inference.\n%!" (format_loc loc)
			);
			Smap.add i (check_expr env e) p
    	| (_,loc) ->
			type_error (EE(expr,loc)) "Object literals should only contain properties."
	) Smap.empty p) in
	let o = Object(ref props, ref true) in
	let rec thisdep _ t = match t with | Arrow(_,Some this,_,_) -> unify ~objdep:true this o | _->() in
	Smap.iter thisdep props; head o
		
  | `Array(el) -> let v = fresh() in List.iter (fun e->unify (chke e) (head v)) el; Array(head v, ref (List.length el), ref true)

  (** _lib method: no typing **)
  | `Call((`Dot((`Identifier("_lib"),_),f), _), el) ->
    let tl = chkel el and v = fresh() in (match f with 
      | "secret" -> unify (Arrow(tl,None,v,None)) (Arrow([String],None,String,None))
      | "hmac" -> unify (Arrow(tl,None,v,None)) (Arrow([String;String],None,String,None))
      | "publish" | "event" -> unify v Undefined
      | "spec" -> unify (Arrow(tl,None,v,None)) (Arrow([String],None,Undefined,None))
      | "DJSON_parse" -> let ot = (Object(ref (Smap.empty), ref false)) in
        unify (Arrow([String;ot],None,ot,None)) (Arrow(tl,None,v,None))
      | _ -> ()
    ); head v

  | `Call(e,el) -> let t = chke e and tl = chkel el and v = fresh() in
	   let thiscond = match t with
			| Arrow(_,Some(o), _, _) -> (* Method call *)
				(match e with (`Dot(_),_) | (`Property(_, (`String(_),_)),_) -> Some o (* Safe (from object) *)
				 | (lhs,_) when is_from_with env lhs -> Some o (* Method from with *)
				 | _ -> type_error (EE(expr,loc)) "Trying to call a method from outside an object.")
			| _ -> None
	   in let et = try
		  let (tt, tf) = env_find ~throw:true "@_try" env in
			(match tf with Notry(r) -> (match t with Arrow(_,_,_,Some(etx)) -> r:=true; unify tt etx; Some(etx) | _ -> None)
			 | _ -> Some(tt))
		 with _ -> None in unify t (Arrow(tl, thiscond, v, et)); head v

  | `Typeof(e) -> let _ = chke e in String
  | `Void(e) -> let _ = chke e in Undefined
  | `Preincr(e) | `Postincr(e) | `Predecr(e) | `Postdecr(e) -> let t = chke ~lhs:true e in unify t Number; Number
  | `Delete(_) -> type_error (EE(expr,loc)) "Deleting properties is not supported."

  | `Function((None, ["s"], ([(`Statement(`If((`Equal((`Typeof((`Identifier("s"),_)),_),(`String("string"),_)),_),(`Return(Some (`Call((`Identifier("_"),_),[(`Identifier("s"),_)]),_)),_),None),_),_) as st]))) -> let t = Arrow([String], None, String, None) and (tr,_) = env_find "_" env in unify tr t; let _ = check_src (env_add "@_return" String (env_add "s" String env 0) 0) st in t
  | `Function(f) -> head (check_function env f)

  | `Conditional(e,f,g) ->
		(match (fst e,fst f) with
		  | (`Land((`Equal((`Dot((`Identifier(i),loci),"length"),_),(`Number(1.),_)),_),
			  (`Le((`Identifier(j),_),(`String(char),_)),_)),`Property((`Identifier(x),locx),(`Identifier(k),_)))
				when String.length char = 1 && i=j && i=k -> unify (chke (`Identifier(i),loci)) String;
				let max = Char.code (char.[0]) and t = chke (`Identifier(x),locx) and ot = fresh() in
				for i=0 to max do unify t (Object(ref (Smap.singleton (String.make 1 (Char.chr i)) ot), ref false)) done; head ot
			| (`Lt((`Ashassign((`Identifier(i),loci),(`Number(0.),_)),_),(`Dot((`Identifier(x),locx),"length"),_)),
			  `Property((`Identifier(y),locy),(`Identifier(j),_))) when x=y && i=j ->
				let tx = chke (`Identifier(x),locx) and ti = chke (`Identifier(i),loci) and tg = chke g in
				unify tx String; unify ti Number; unify tg String; String
      | (`Lt((`Number(k),_),(`Dot((`Identifier(x),locx),"length"),_)),
        `Property((`Identifier(y),locy),(`Number(l),_))) when x=y && k=l && k >= 0. && float_of_int (truncate k)=k ->
        let tx = chke (`Identifier(x),locx) and tg = chke g in unify tx String; unify tg String; String
		  | _ -> let t1 = chke e and t2 = chke f and t3 = chke g in unify t1 Boolean; unify t2 t3; t2)

  | `Sequence(el) -> let tl = chkel el in lasttype tl
  | `In(e,f) -> let _ = chke e and _ = chke f in Boolean 

	| `Sub(e,f) | `Multiply(e,f) | `Mod(e,f) | `Divide(e,f) | `Lsh(e,f) | `Rsh(e,f) | `Ash(e,f)  | `Bor(e,f) | `Band(e,f) | `Bxor(e,f)
      -> (let t1 = chke e and t2 = chke f in
		  (match print_type t2 with
	  		| _ -> (unify t1 Number; unify t2 Number; let _=(head t1, head t2) in Number) 
			| "byte"   -> (unify t1 Byte; unify t2 Byte; let _=(head t1, head t2) in Byte)))
  | `Lor(e,f) | `Land(e,f) -> let t1 = chke e and t2 = chke f in unify t1 Boolean; unify t2 Boolean; Boolean 
  | `Assign(e,f) -> let t1 = chke ~lhs:true e and t2 = chke f in unify ~assign:true t1 t2; t1
  | `Ashassign(e,f) -> let t1 = chke ~lhs:true e and t2 = chke f in unify t1 t2; unify t1 Number; t1

  | `Equal(e,f) | `Sequal(e,f) -> let t1 = chke e and t2 = chke f in unify t1 t2; Boolean

	| `Lt(e,f) | `Gt(e,f) | `Le(e,f) | `Ge(e,f) ->
		 let t1 = chke e and t2 = chke f in unify t1 t2;
		(match head t1 with Number|Byte|String(_)|Boolean->() | Variable(v) -> unify t1 Number
		 | t -> type_error (EE(expr,loc)) (sprintf "Cannot compare object type %s." (print_type t))); Boolean

  (* We arbitrarily decide that + on type variables is number *)
  | `Add(e,f) -> let t1 = chke e and t2 = chke f in
		(match (t1,t2) with
			| (String, _)      -> String
			| (_, String)      -> String
			| (Number, Number) -> Number
			| (Byte, Byte)     -> Byte
			| (Byte, Number)   -> Byte
			| (Number, Byte)   -> Byte
			| (String, Number) -> String
			| (Number, String) -> String
			| (Variable(v1),Variable(v2)) -> unify (Variable(v1)) Number; unify (Variable(v2)) Number; Number
			| (Variable(v),Number) | (Number,Variable(v)) -> unify (Variable(v)) Number; Number
      		| (Variable(v),String) | (String,Variable(v)) ->
				(match e with 
				| (`String(""),loc) -> 
					eprintf "Warning: interpreting %s as a number to string cast at %s\n%!" (Pretty.pretty_print_exp 0 (expr,loc)) (format_loc loc);
				  unify (Variable(v)) Number; String
				| _ -> unify (Variable(v)) String; String)  
			| (t1,t2) -> type_error (EE(expr,loc)) (sprintf "Invalid use of + on types (%s,%s)" (print_type t1) (print_type t2)))

	| `Dot(e, i) -> let t = (match e with 
		| (`This,_) -> if !toplevel then syn_error loc "Cannot use this outside functions"; fst (env_find "this" env)
		| _ -> chke e) and v = fresh() in
		  if lh && i = "length" then type_error (EE(expr,loc)) "Assigning a value to length is not defensive.";
			if try i.[0]='@'&&i.[1]='_' with _->false then type_error (EE(expr,loc)) "property names that start with @_ are reserved.";
	    (match (t,i) with
		  | (String(_), "length") | (Array(_), "length") -> Number
		  | (Variable(v), "length") -> let t = fresh() in unify (Variable(v)) (Array(t,ref 0,ref false)); Number
		  | _ -> (unify (Object(ref (Smap.singleton i v), ref false)) t); head v)

  (** e[f] **)
	| `Property(e,f) ->
		(match f with
			| (`Number(k),loc) -> let l = truncate k in
			  if not (float_of_int l=k && l>=0) then type_error (EE(expr,loc)) "Constant array index must be a non-negative integer.";
				let v=fresh() and t = chke e in unify t (Array(v, ref (l+1), ref false)); head v
			| (`String(i),_) -> chke (`Dot(e,i),loc)

			| (`Identifier(i),_) -> let (t,f) = env_find i env in
			  (match f with Bound_accessor(k) when fst e=`Identifier(k) ->
					let t = chke e in 
					 (match t with 
						| String -> String
						| Array(t,_,_) as at -> unify at (Array(t, ref 1, ref false)); t
						| _ -> type_error (EE(expr,loc)) (sprintf "Cannot use bound accessor on type %s" (print_type t)))
				 | _ -> type_error (EE(expr,loc)) "unable to infer that this property access is safe.")

      | (`Band(expr, (`Number(max),_)),loc2) when max >= 0. && float_of_int (truncate max)=max ->
				let v = fresh() in unify (chke e) (Array(v, ref ((truncate max)+1), ref false));
				unify (chke ~lhs:false expr) Number; head v
			| (`Mod((`Ash(expr2,(`Number(0.),_)),_), (`Dot((`Identifier(i),_),"length"),_)),_) ->
				(match e with
				| (`Identifier(p),loc2) when p=i -> let t=chke e and v=fresh() in
				  unify (chke ~lhs:false expr2) Number;
					unify t (Array(v, ref 1, ref false)); head v
				| (_, loc2) -> type_error (EE(expr,loc)) "Mismatched variable in dynamic accessor x[(e>>>0)%y.length]")
			| _ -> type_error (EE(expr,loc)) "unable to infer that this property access is safe.")

	| `New(_) | `Instanceof(_) -> type_error (EE(expr,loc)) "constructors and new are not supported in this version."
	| `This -> type_error (EE(expr,loc)) "this can only be used to access properties in methods."
	| `Elision -> type_error (EE(expr,loc)) "cannot use elisions in array (they cause missing properties)."
        | `Undefined -> Undefined
        | `Null -> type_error (EE(expr,loc)) "no type for null (use undefined instead)."
	in let t = head result in
	  if !print_subtypes then eprintf "%s: %s\n%!" (Pretty.pretty_print_exp 0 (expr,loc)) (print_type t);
	  if !pv then Hashtbl.replace type_table loc t; t
  with a -> exception_handler (EE(expr,loc)) a
