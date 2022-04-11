open Ast
open Printf
open Globals
open Error
open Pretty
open Globals
open Typecheck
open Str

let number_c = ref []
let string_c = ref []
let consts_c = ref []
let variab_c = ref []
let tables_c = ref []
let my_types = ref []
let constm_c = ref ""
let tablem_c = ref ""
let types_c  = ref ""
let tyfun_c  = ref ""
let inclu_c  = ref ""
let shape_c  = ref ""
let export_c = ref ""
let calls_c  = ref ""
let decl_i   = ref 0

let strip_first_char str =
	if str = "" then "" else
		String.sub str 1 ((String.length str) - 1)

let rec typeof loc = (match (Hashtbl.find type_table loc) with
	| x -> head x)

and get_type_using_name n =
	let ((na, nb, nc, nd), y) = ((ref "", ref "", ref [], ref false), ref false) in
	(List.iter (fun (ma, mb, mc, md) -> (
		if ((String.compare n mb) = 0) then (
			na := ma;
			nb := mb;
			nc := mc;
			nd := md;
			y  := true;
	))) !my_types);
	((!na, !nb, !nc, !nd), !y)

and get_type_using_shape a s =
	let ((na, nb, nc, nd), y) = ((ref "", ref "", ref [], ref false), ref false) in
	List.iter (fun x -> (
		if (!y = false) then (
			let m = ref true in
			(match x with
				| (w, q, c, z) -> (
					if (
						((String.compare a w) <> 0) ||
						((List.length s) <> (List.length c))
					) then (
						m := false
					)
					else (
						(List.iter2 (fun d b ->
							m := (match (String.compare (fst d) (fst b)) with
								| 0 -> !m
								| _ -> false);
							m := (match (String.compare (snd d) (snd b)) with
								| 0 -> !m
								| _ -> false)
						) s c);
						if (!m = true) then (
							na := w;
							nb := q;
							nc := c;
							nd := z;
							y  := true
						)
					)
				)
				| _ -> (m := false)
			)
		)
	)) !my_types;
	((!na, !nb, !nc, !nd), !y)

and pv_type (v, loc) = match v with
	| `Number(n) -> "number"
	| `Byte(y)   -> "byte"
	| `Bool(b)   -> "bool"
	| `String(s) -> "bitstring"
	| `Object(o) -> (match (get_type_using_shape "object" (pv_type_shape v)) with
		| ((na, nb, nc, nd), true)  -> String.lowercase (object_ref nb)
		| (_, false) -> object_def (pv_type_shape v); (pv_type (v, loc)))
	| `Array(a) ->  (match (get_type_using_shape "array" (pv_type_shape v)) with
		| ((na, nb, nc, nd), true)  -> (match nd with
			| true  -> nb
			| false -> String.lowercase (array_ref nb))
		| (_, false) -> "bitstring")
	| `Function(f) -> "function"
	| `Property(_) -> "prop"
	| `Dot(_)      -> "dot"
	| `Identifier(_)  -> "identifier"
	| _ -> ""

and loc_type v = match v with
	| Number       -> "number"
	| Byte         -> "byte"
	| Boolean      -> "bool"
	| String       -> "bitstring"
	| Object(p, b) -> (match (get_type_using_shape "object" (loc_type_shape v)) with
		| ((na, nb, nc, nd), true)  -> String.lowercase (object_ref nb)
		| (_, false) -> object_def (loc_type_shape v); loc_type v)
	| Array(t, i, b) -> (match (get_type_using_shape "array" (loc_type_shape v)) with
		| ((na, nb, nc, nd), true)  -> (match nd with
			| true  -> nb
			| false -> String.lowercase (array_ref nb))
		| (_, false) -> array_def (loc_type_shape v); loc_type v)
	| Arrow(at, this, out, throw) -> "function"
	| Undefined    -> "undefined"
	| Variable(t)  -> (match t.def with
		| Some def -> loc_type def
		| None     -> "bitstring")
	| _            -> "other" 

and sort_free t =
	let c = ref 97 and ids = ref [] in
	let par s = function Arrow(_)->"("^s^")" | _->s in
	let rec aux = function
		| Number    -> "number"
		| Byte      -> "byte"
		| Boolean   -> "boolean"
		| String    -> "string"
		| Undefined -> "undefined"
		| Arrow(tl, this, t, et) ->
			let u = List.map aux tl in
			let v = aux t in
			let tr = match this with None->"" | Some(tt)->"["^(aux tt)^"]" in
			(
				sprintf "(%s)%s -> %s"
				(List.fold_left (fun x y->(if x="" then x else x^", ")^y) "" u)
				tr
				v
			)^
			(match et with None->"" | Some(t) -> "[throws "^(aux t)^"]")
		| Array(t,n,f) -> sprintf "%s array[%s%d]" (par (aux t) t) (if !f then "" else ">=") !n
		| Object(tl,f) -> let p = Smap.fold(
				fun i t x->(if x="" then x else x^", ")^"\""^(recode_utf8 i)^"\":"^(aux t)
			) !tl "" in
			"{"^p^"}"^(if !f then " final" else "")
		| Variable(t) -> let tid = try snd (List.find (fun (u,v)->u=t.id) !ids)
			with Not_found -> let a=String.make(1) (char_of_int !c) in (incr c; ids := (t.id,a)::!ids; a)
			and ng_flag = if t.ng then "_" else "" in sprintf "'%s%s" ng_flag tid
  	in
	aux (head t)

and number_def_ref n =
	(match (List.assoc n !number_c) with
		| exception Not_found -> (
			incr decl_i;
			let t = sprintf "number_%d" !decl_i in
			number_c := ((n, t) :: !number_c);
			t)
		| x -> x)

and string_def_ref s = 
	(match (List.assoc s !string_c) with
		| exception Not_found -> (
			incr decl_i;
			let t = sprintf "string_%d" !decl_i in
			string_c := ((s, t) :: !string_c);
			t)
		| x -> x)

and table_def_ref s l =
	(match (List.assoc s !tables_c) with
		| exception Not_found -> (
			let a = ref "" in
			(List.iteri (fun i x ->
				(match i with
					| 0 -> ()
					| 1 -> a := (loc_type (typeof (snd x)))
					| _ -> a := sprintf "%s, %s" !a (loc_type (typeof (snd x))))
			) l);
			tables_c := ((s,s) :: !tables_c);
			tablem_c :=  (sprintf "%stable %s(%s).\n" !tablem_c s !a);
			s
		)
		| _ -> s)

and compareVs v1 v2 = (match (v1, v2) with
	| [], []       -> true
	| [], _
	| _, []        -> false
	| x::xs, y::ys -> x = y && compareVs xs ys)

and array_val_is_def v =
	let v = (fst (List.split v)) in
	let f = ref false in
	(List.iter (fun x ->
		(match (compareVs v (fst x)) with
			| true  -> f := true
			| false -> ())
	) !consts_c);
	!f

and array_val_def_ref v y i =
	let v = (fst (List.split v)) in
	let f = ref false in
	let z = ref "" in
	(List.iter (fun x ->
		(match (compareVs v (fst x)) with
			| true  -> f := true; z := (fst (snd x))
			| false -> ())
	) !consts_c);
	(match !f with
		| true -> !z
		| false -> let t = sprintf "%s_%d" y i in
			consts_c := ((v, (t, y)) :: !consts_c); t)

and get_array_shape a =
	let i = ref 0 in decr i;
	(List.map (fun v -> incr i; (sprintf "%d" !i, (match (typeof (snd v)) with
		| exception Not_found -> (pv_type v)
		| x -> loc_type x))) a)

and get_object_shape o =
	let (l, _) = List.split o in
	let l = List.map (function
		`Property(n, v) -> (match (typeof (snd v)) with
			| exception Not_found -> (n, pv_type v)
			| x -> (n, loc_type x))
		| _ -> failwith "UNSUPPORTED151"
	) l in
	let l = List.sort (fun (a, _) (b, _) -> compare a b) l in
	l

and get_loc_object_shape o =
	let l = ref [] in
	(Smap.iter (fun n v ->
		l := !l@[(n, loc_type v)];
	) o); 
	l := List.sort (fun (a, _) (b, _) -> compare a b) !l;
	!l

and get_loc_array_shape t n =
	let l = loc_type t in
	let s = ref [] in
	for i = 0 to (n - 1) do
		s := !s@[(sprintf "%d" i, l)];
	done;
	!s

and array_def shape =
	incr decl_i;
	let prop = ref [] in
	let r = ref "" in
	types_c := !types_c^(sprintf "type array_%d.\n" !decl_i);
	r := !r^(sprintf "fun Array_%d(" !decl_i);
	(List.iter (fun (n, v) ->
		r := !r^v^", ";
	) shape);
	r := String.sub !r 0 ((String.length !r) - 2);
	r := sprintf "%s):array_%d [data].\n" !r !decl_i;
	let dec_reduc = ref "reduc forall " in
	(List.iter (fun (n, v) ->
		dec_reduc := !dec_reduc^(sprintf "e_%s:" n );
		dec_reduc := !dec_reduc^v^", ";
		prop := !prop @ [((sprintf "e_%s" n), v)];
	) shape);
	dec_reduc := String.sub !dec_reduc 0 ((String.length !dec_reduc) - 2);
	(List.iter (fun (s, p) ->
		r := (!r)^(!dec_reduc)^";\n";
		let rb = ref (sprintf "Array_%d_get_%s(" !decl_i s) in
		rb := !rb^(sprintf "Array_%d(" !decl_i);
		(List.iter (fun (ss, pp) ->
			rb := !rb^ss^", ";
		) !prop);
		rb := String.sub !rb 0 ((String.length !rb) - 2);
		rb := (!rb)^")) = "^s^".";
		r := (!r)^(!rb)^"\n";
	) !prop);
	(List.iter (fun (s, p) -> 
		let argnam = ref (List.fold_left (^) "" (List.map (fun (ss, pp) ->
			ss^", "
		) !prop)) in
		argnam := String.sub !argnam 0 ((String.length !argnam) - 2);
		let rb = ref (sprintf "%s, pvnew:%s;\nArray_%d_set_%s(Array_%d(%s), pvnew) = Array_%d(%s"
			!dec_reduc p !decl_i s !decl_i !argnam !decl_i (List.fold_left (^) ""
				(List.map (fun (ss, pp) -> (match (compare s ss) with
					| 0 -> "pvnew, "
					| _ -> ss^", ")) !prop))) in 
		rb := String.sub !rb 0 ((String.length !rb) - 2);
		r  := (!r)^(!rb)^").\n";
	) !prop);
	my_types := !my_types@[("array", (sprintf "%d" !decl_i), shape, false)];
	shape_c := (!shape_c) ^ (!r) ^ "\n"

and object_def ?(t) shape =
	let id = ref "" in
	(match t with
		| Some t -> id := t
		| None   -> incr decl_i; id := (string_of_int !decl_i));
	let prop = ref [] in
	let r = ref "" in
	types_c := !types_c^(sprintf "type object_%s.\n" !id);
	r := !r^(sprintf "fun Object_%s(" !id);
	(List.iter (fun (n, v) ->
		r := !r^v^", ";
	) shape);
	r := String.sub !r 0 ((String.length !r) - 2);
	r := sprintf "%s):object_%s [data].\n" !r !id;
	let dec_reduc = ref "reduc forall " in
	(List.iter (fun (n, v) ->
		dec_reduc := !dec_reduc^(sprintf "%s:" n);
		dec_reduc := !dec_reduc^v^", ";
		prop := !prop @ [((sprintf "%s" n), v)];
	) shape);
	dec_reduc := String.sub !dec_reduc 0 ((String.length !dec_reduc) - 2);
	(List.iter (fun (s, p) ->
		r := (!r)^(!dec_reduc)^";\n";
		let rb = ref (sprintf "Object_%s_get_%s(" !id s) in
		rb := !rb^(sprintf "Object_%s(" !id);
		(List.iter (fun (ss, pp) ->
			rb := !rb^ss^", ";
		) !prop);
		rb := String.sub !rb 0 ((String.length !rb) - 2);
		rb := (!rb)^")) = "^s^".";
		r := (!r)^(!rb)^"\n";
	) !prop);
	(List.iter (fun (s, p) -> 
		let argnam = ref (List.fold_left (^) "" (List.map (fun (ss, pp) ->
			ss^", "
		) !prop)) in
		argnam := String.sub !argnam 0 ((String.length !argnam) - 2);
		let rb = ref (sprintf "%s, pvnew:%s;\nObject_%s_set_%s(Object_%s(%s), pvnew) = Object_%s(%s"
			!dec_reduc p !id s !id !argnam !id (List.fold_left (^) ""
				(List.map (fun (ss, pp) -> (match (compare s ss) with
					| 0 -> "pvnew, "
					| _ -> ss^", ")) !prop))) in 
		rb := String.sub !rb 0 ((String.length !rb) - 2);
		r  := (!r)^(!rb)^").\n";
	) !prop);
	my_types := !my_types@[("object", (!id), shape, false)];
	shape_c := (!shape_c) ^ (!r) ^ "\n"

and object_ref i =
	sprintf "Object_%s" i

and array_ref i =
	sprintf "Array_%s" i

and num_op o = (match o with 
	| `Mod(x, y)      -> sprintf "modulo(%s, %s)" (pv_expression x) (pv_expression y)
	| `Add(x, y)      -> (match ((pv_type x), (pv_type y)) with
		| ("bitstring", _) | (_, "bitstring") -> sprintf "str_add(%s, %s)" (pv_expression x) (pv_expression y)
		| ("number", _)    | (_, "number")    -> sprintf "add(%s, %s)" (pv_expression x) (pv_expression y)
		| (_, _)           | (_, _)           -> sprintf "str_add(%s, %s)" (pv_expression x) (pv_expression y))
	| `Divide(x, y)   -> sprintf "divide(%s, %s)" (pv_expression x) (pv_expression y)
	| `Multiply(x, y) -> sprintf "multiply(%s, %s)" (pv_expression x) (pv_expression y)
	| `Sub(x, y)      -> sprintf "subtract(%s, %s)" (pv_expression x) (pv_expression y)
	| `Lsh(x, y)      -> sprintf "bitShiftLeft(%s, %s)" (pv_expression x) (pv_expression y)
	| `Rsh(x, y)      -> sprintf "bitShiftRight(%s, %s)" (pv_expression x) (pv_expression y)
	| `Ash(x, y)      -> sprintf "bitZeroFill(%s, %s)" (pv_expression x) (pv_expression y)
	| `Lt(x, y)       -> sprintf "lessThan(%s, %s)" (pv_expression x) (pv_expression y)
	| `Gt(x, y)       -> sprintf "greaterThan(%s, %s)" (pv_expression x) (pv_expression y)
	| `Le(x, y)       -> sprintf "lessThanOrEqual(%s, %s)" (pv_expression x) (pv_expression y)
	| `Ge(x, y)       -> sprintf "greaterThanOrEqual(%s, %s)" (pv_expression x) (pv_expression y)
	| `Band(x, y)     -> sprintf "bitAND(%s, %s)" (pv_expression x) (pv_expression y)
	| `Bxor(x, y)     -> sprintf "bitXOR(%s, %s)" (pv_expression x) (pv_expression y)
	| `Bor(x, y)      -> sprintf "bitOR(%s, %s)" (pv_expression x) (pv_expression y)
	| `Bnot(x)        -> sprintf "not(%s)" (pv_expression x)
	| `Minus(x)       -> sprintf "minus(%s)" (pv_expression x)
	| `Plus(x)        -> sprintf "plus(%s)" (pv_expression x)
	| _            -> failwith "UNSUPPORTED366")

and declare_type h t =
	(match h with
		| `Object(o) -> (
			let (l, _) = List.split o in
			let con = ref [] in
			let q = ref "" in
			(List.iter (function
				| `Property(n, v) -> (match n with
					| "construct" -> (match (fst v) with
						| `Function(f) -> (match f with
							| (m,args,b) -> (List.iter (function
								| `Statement(s),loc -> (match (fst s) with
									| `Return(r) -> (match r with
										| Some r -> (match (fst r) with
											| `Object(_) | `Array(_) -> (match (fst r) with
													| `Object(_) -> (object_def ~t:(t) (pv_type_shape (fst r)))
													| `Array(l)  -> q := "array";
														array_val_def_ref l t 0;
														con := pv_type_shape (fst r);
														(my_types := (match (List.length !con) with
															| 0 -> !my_types
															| _ -> !my_types@[(!q, t, !con, true)]))
													| _ -> ());
												let tt = (match (fst r) with
													| `Object(_) -> "object_"^t
													| _          -> t) in
												types_c := !types_c^
													(match (fst r) with
														| `Array(_) -> (sprintf "type %s.\n" tt)
														| _      -> "");
												tyfun_c := !tyfun_c^
													(sprintf "fun Type_%s_construct():%s.\n" t tt)^
													(sprintf "fun Type_%s_toBitstring(%s):bitstring [data, typeConverter].\n" t tt)^
													(sprintf "fun Type_%s_fromBitstring(bitstring):%s [data, typeConverter].\n" t tt)^
													(sprintf "fun Type_%s_clone(%s):%s [data].\n" t tt tt)^
													(sprintf "reduc forall a:%s; Type_%s_assert(a) = a.\n" tt t)
											| _ -> ())
										| None   -> ())
									| _          -> ())
								| _          -> ()) b)
							| _          -> ())
						| _            -> ())
					| _           -> ())
				| _               -> ())
			l))
		| _ -> (failwith "UNSUPORTED356"))
		
and pv_type_shape p = match p with
	| `Object(o) -> get_object_shape o
	| `Array(a)  -> get_array_shape a
	| _          -> failwith "UNSUPPORTED362"

and loc_type_shape p = match p with
	| Object(p, b) -> get_loc_object_shape !p
	| Array(t, i, b) -> get_loc_array_shape t !i
	| _          -> failwith "UNSUPPORTED367"

and assign_number e f =
	"assign_number"

and assign_bool e f =
	"assign_bool"

and assign_string e f =
	"assign_string"

and assign_object ?(name) o =
	let nl = (match name with
		| Some name -> fst name
		| None      -> "") in
	(match (string_match (Str.regexp "^Type_.+") nl 0) with
		| true  -> declare_type o (String.sub nl 5 (String.length nl - 5)); ""
		| false -> sprintf "%s%s"
			(match name with
				| None -> ""
				| Some name  -> "let "^
					(get_local_identifier (fst name) (snd name))^" = ")
			(match (get_type_using_shape "object" (pv_type_shape o)) with
				| ((na, nb, nc, nd), true)  -> return_object o
				| (_, false) -> (match o with
					| `Object(c) -> object_def (pv_type_shape o); assign_object o
					| _          -> failwith "UNSUPPORTED375"))^
			(match name with
				| Some name -> " in"
				| None      -> ""))

and assign_export ?(name) o =
	(match o with
		| `Object(c) -> (
			let (l, _) = List.split c in
			(List.map (function
				`Property(n, v) -> (match (fst v) with
					| `Function(f) -> (
						(declare_function f (snd v) false true));
					| `Identifier(i) -> export_c := !export_c^(sprintf "meow%s" i)
					| _ -> failwith "UNSUPPORTED460")
				| _ -> failwith "UNSUPPORTED462") l))
		| _ -> failwith "UNSUPPORTED 464"); ""

and assign_array ?(name) a =
	sprintf "%s%s"
	(match name with
		| None -> ""
		| Some name  -> "let "^
			(get_local_identifier (fst name) (snd name))^" = ")
	(match (get_type_using_shape "array" (pv_type_shape a)) with
		| ((na, nb, nc, nd), true)  -> return_array a
		| (_, false) -> (match a with
			| `Array(c) -> array_def (pv_type_shape a); assign_array a
			| _          -> failwith "UNSUPPORTED375"))^
	(match name with
		| Some name -> " in"
		| None      -> "")

and return_object o =
	let r = ref "Object_" in
	(match (get_type_using_shape "object" (pv_type_shape o)) with
		| ((na, nb, nc, nd), true) -> (
				r := !r^nb^"(";
				(match o with 
					| `Object(p) -> 
						(let (l, _) = List.split p in
						let l = List.map (function
							| `Property(x, y) -> (x, y)
							| _ -> failwith "UNSUPPORTED399") l in
						let l = List.sort (fun (a, _) (b, _) -> compare a b) l in
						(List.map (function
							| (n, v) -> r:=!r^(match (fst v) with
								| `Object(_)     -> return_object (fst v)
								| `Number(x)     -> number_def_ref x
								| `String(x)     -> string_def_ref x
								| `Call(x, y)    -> assign_call x y
								| `Array(x)      -> return_array (fst v)
								| `Dot(e, i)     -> get_val e i
								| `Bool(x)       -> (match x with
									| true -> "true"
									| false -> "false")
								| `Function(x)   -> (declare_function ~name:(n, (snd v)) x (snd v) true false);
									get_local_identifier n (snd v)
								| `Identifier(x) -> get_local_identifier x (snd v)
								| `Property(pl, pr) -> get_val pl (match (fst pr) with
									| `Number(n) -> (sprintf "e_%n" (int_of_float n))
									| _          -> failwith "UNSUPPORTED494")
								| _ -> pv_expression v)^", ";
							| _ -> failwith "UNSUPPORTED432"
						) l);
						r := (String.sub !r 0 ((String.length !r) - 2));
						r := !r^")")
					| _ -> failwith "UNSUPPORTED436"
				)
			); !r^""
		| (_, false) -> object_def (pv_type_shape o); return_object o
	)

and return_array o =
	let r = ref "Array_" in
	(match (get_type_using_shape "array" (pv_type_shape o)) with
		| ((na, nb, nc, true), true)  -> (match o with
			| `Array(l) -> incr decl_i; array_val_def_ref l nb !decl_i
			| _ -> failwith "UNSUPPORTED482")
		| ((na, nb, nc, false), true) -> (
				r := !r^nb^"(";
				(match o with 
					| `Array(l) -> 
						(List.map (function
							| (v, loc) -> r:=!r^(match v with
								| `Object(_)     -> return_object v
								| `Number(x)     -> number_def_ref x
								| `String(x)     -> string_def_ref x
								| `Call(x, y)    -> assign_call x y
								| `Array(x)      -> return_array v
								| `Dot(e, i)     -> get_val e i 
								| `Identifier(x) -> get_local_identifier x loc
								| _              -> pv_expression (v, loc))^", ";
							| _ -> failwith "UNSUPPORTED499"
						) l);
						r := (String.sub !r 0 ((String.length !r) - 2));
						r := !r^")"
					| _ -> failwith "UNSUPPORTED503"
				)
			); !r^""
		| (_, false) -> array_def (pv_type_shape o); return_array o
	)


and declare_function ?(name) ((n,args,b):function_t) loc asprop export =
	let located = typeof loc in
	let r = ref "" in
	let argnam = ref "" in
	let argtyp = ref "" in
	let argstr = ref "" in
	let f = (sprintf "%s" (match n with
		| Some n -> (match n with
			| "" -> failwith "UNSUPPORTED546"
			| _  -> n)
		| None   -> (match name with
			| Some (z, x) -> z
			| None -> "AnonymousFunction"))) in
	let fn = (get_local_identifier f loc) in
	(match (string_match (Str.regexp "^ProScript_.+") fn 0) with
		| true  -> ()
		| false -> (match located with
			| Arrow(at, this, out, throw) ->
				(List.iter2 (fun a t ->
					let sgra = List.tl (List.rev args) in
					let g = get_local_identifier a loc in 
					argnam := !argnam^g^(match (List.rev args) with
						| (x::y) when ((x = a) && (y = sgra)) -> ""
						| _ -> ", ");
					argtyp := !argtyp^(loc_type t)^(match (List.rev args) with
						| (x::y) when ((x = a) && (y = sgra)) -> ""
						| _ -> ", ");
					argstr := !argstr^g^":"^(loc_type t)^(match (List.rev args) with
						| (x::y) when ((x = a) && (y = sgra)) -> ""
						| _ -> ", ");
				) args at);
				(match (asprop, export, (List.length args)) with
					| (true, false, 0) -> constm_c := !constm_c^(sprintf "const %s:function [data].\n" fn);
						r := (sprintf "letfun fun_%s(%s) =\n" fn !argstr)
					| (true, false, _) -> constm_c := !constm_c^(sprintf "const %s:function [data].\n" fn);
						r := (sprintf "letfun fun_%s(%s) =\n" fn !argstr)
					| (false, false, _) -> r := (sprintf "letfun fun_%s(%s) =\n" fn !argstr)
					| (_, true, _)      -> (r := (sprintf "!(\nin(io, (%s));\n" !argstr)));
				(List.iter (fun bi ->
					r := !r^(match bi with
						| (`Statement(s),loc) -> (pv_statement ~fn:(fn, asprop) s export)
						| (`FunctionDeclaration(f),loc) -> failwith "UNSUPPORTED530")
				) b);
				(match export with
					| false ->  (calls_c := !calls_c^(!r)^".\n\n")
					| true  -> (match (String.length !export_c) with
						| _  -> (export_c := !export_c^"|\n"^(!r)^"\n)\n")
						| 0  -> (export_c := !export_c^(!r)^"\n)\n")));
			| _ -> failwith "UNSUPPORTED582")
		| _ -> failwith "UNSUPPORTED583")

and assign_identifier e i =
	sprintf "let %s = %s in\n" e i

and assign_add e al ar =
	sprintf "let %s = add(%s, %s) in\n" e (pv_expression al) (pv_expression ar)

and assign_sub e al ar =
	sprintf "let %s = subtract(%s, %s) in\n" e (pv_expression al) (pv_expression ar)

and make_byte_array_shape b = 
	let s = ref [] in
	for i = 0 to (b - 1) do
		s := !s@[(sprintf "%d" i, "byte")];
	done;
	!s

and get_full_name e =
	(match e with
		| `Dot(dl, dr) -> (sprintf "%s_%s" (get_full_name (fst dl)) dr)
		| `Identifier(x) -> x
		| _ -> failwith "UNSUPPORTED610")

and assign_call ?(name) e l = 
	incr decl_i;
	let reglist = [
		(Str.regexp "^ProScript_");
		(Str.regexp "^Type_")
	] in
	let silent = ref false in
	let table = ref false in
	let tableget = ref false in
	let fname = ref (match (fst e) with
		| `Dot(dl, dr) -> (get_local_identifier dr (snd e))
		| _            -> (get_full_name (fst e))) in
	(List.iter (fun r ->
		(match (string_match r (get_full_name (fst e)) 0) with
			| true -> (
				silent := true;
				fname := (get_full_name (fst e));
				(match (string_match (Str.regexp "^ProScript_state_insert") !fname 0) with
					| true -> (fname := sprintf "insert %s" (match (fst (List.nth l 0)) with
						| `String(s) -> (
							table := true;
							table_def_ref s l
						)
						| _ -> failwith "ProScript table names must be string literals."))
					| false -> ());
				(match (string_match (Str.regexp "^ProScript_state_get") !fname 0) with
					| true -> (fname := sprintf "get %s" (match (fst (List.nth l 0)) with
						| `String(s) -> table := true; tableget := true; s
						| _ -> failwith "ProScript table names must be string literals."))
					| false -> ()))
			| false -> ())
	) reglist);
	let ll = (match !table with
		| true -> List.tl l
		| false -> l) in
	(match !silent with
		| _ -> (sprintf "%s%s%s"
			(match name with 
				| None -> ""
				| Some name -> (match !table with
					| false -> (sprintf "let %s = "
						(get_local_identifier (fst name) (snd name)))
					| true -> ""))
			(sprintf "%s(%s"
				(match !silent with
					| true -> !fname
					| false -> "fun_"^(!fname))
				(match (List.length ll) with
					| 0 -> ")"
					| _ -> (List.fold_left (^) "" (List.mapi (fun i x ->
						(match (fst x) with
							| `Identifier(y) -> (sprintf "%s%s"
								(if (i = 0 && !tableget) then ("=") else (""))
								(get_local_identifier y (snd e)))
							| _              -> (pv_expression x))^
						(if ((i + 1) = (List.length ll)) then (
							if (!tableget) then (
								(sprintf ", %s)" (match name with
									| None -> ""
									| Some name -> (get_local_identifier (fst
									name) (snd name))))
							) else (")")
						) else ", ")) ll))))
			(match !table with
				| true -> (match !tableget with
					| true -> " in\n"
					| false -> ";\n")
				| false -> (match name with
					| None         -> ""
					| Some (x, y)  -> " in\n"))))

and get_local_identifier n loc = 
	let c = fst (List.split !variab_c) in
	(match (List.assoc n c) with
		| exception Not_found -> (
			incr decl_i;
			variab_c := !variab_c@[(n, !decl_i), loc];
			sprintf "%s" n)
		| x -> sprintf "%s" n
	)

and pv_expression ?(name) expr =
	let rec pve (v, loc) = match v with
		| `Number(n) -> number_def_ref n
		| `String(s) -> string_def_ref s
		| `Null      -> "0"
		| `Bool(b)   -> if b then "true" else "false"
		| `Object(o) -> (match (get_type_using_shape "object" (pv_type_shape v)) with
			| ((na, nb, nc, nd), true)  -> (assign_object v)
			| (_, false) -> (match name with
				| Some (x, y) -> assign_object ~name:(x, y) v
				| None -> assign_object v))
		| `Array(l) -> (match (get_type_using_shape "array" (pv_type_shape v)) with
			| ((na, nb, nc, nd), true) -> (match nb with
				| "" -> array_ref nb
				| _  -> incr decl_i; (array_val_def_ref l nb !decl_i))
			| (_, false) -> (match name with
				| Some (x, y) -> assign_array ~name:(x, y) v
				| None -> assign_array v))
		| `Identifier(i)    -> get_local_identifier i loc
		| `Dot(e, i)        -> get_val e i
		| `Property(pl, pr) -> get_val pl (match (fst pr) with
			| `Number(n) -> (sprintf "e_%n" (int_of_float n))
			| `String(s) -> (sprintf "e_%s" s)
			| _          -> ("UNSUPPORTED733"))
		| `Call(e, l)       -> (match name with
			| Some (x, y) -> assign_call ~name:(x, y) e l
			| None -> assign_call e l)
		| `Plus(e)  -> num_op v
		| `Minus(e) -> num_op v
		| `Lnot(e)  -> "not("^(pv_expression e)^")"
		| `Bnot(e)  -> num_op v
		| `Add(e,f) -> num_op v
		| `Sub(e,f) | `Multiply(e,f) | `Mod(e,f) | `Divide(e,f)
		| `Lsh(e,f) | `Rsh(e,f) | `Ash(e,f)  | `Bor(e,f) | `Band(e,f) | `Bxor(e,f)
		| `Lt(e, f) | `Gt(e, f) | `Le(e, f) | `Ge(e, f) -> num_op v
		| `Land(e, f) -> (pv_expression e)^" && "^(pv_expression f)
		| `Lor(e, f)  -> (pv_expression e)^" || "^(pv_expression f)
		| `Sequal(e, f) | `Equal(e,f) -> (pv_expression e)^" = "^(pv_expression f)
		| `Assign(e, x) -> (match (fst e) with
			| `Dot(vl, il) -> (match (fst x) with
				| `Number(n)        -> (set_val (vl, il) x)
				| `Bool(b)          -> (set_val (vl, il) x)
				| `String(s)        -> assign_string (pve e) s
				| `Array(a)         -> assign_array ~name:(il, (snd e)) (fst x)
				| `Object(o)        -> (match ((pv_expression vl), il) with
					| ("module", "exports") -> (assign_export ~name:(il, (snd e)) (fst x))
					| _ -> (assign_object ~name:(il, (snd e)) (fst x)))
				| `Identifier(i)    -> assign_identifier (pve e) i
				| `Dot(vr, ir)      -> (get_val vr ir)
				| `Property(vr, ir) -> (set_val (vl, il) x)
				| `Call(v, l)       -> (match name with
					| Some name -> (set_val ~name:("randomBytes", (snd e)) (vl, il) x)
					| None      -> (set_val (vl, il) x))
				| `Minus(x)         -> (set_val (vl, il) x)
				| `Plus(x)     -> "UNSUPPORTED764"
				| _                 -> (set_val (vl, il) x))
				(*(failwith (sprintf "ProScript: (Dot -> %s) assignment is not supported." (pv_expression x))))*)
			| `Property(vl, il) -> (match (fst x) with
				| _                 -> "UNSUPPORTED768")
				(*(failwith (sprintf "ProScript: (Property %s -> _) assignment is not supported." (pv_expression vl))))*)
			| `Identifier(il) -> (match (fst x) with
				| `Number(n)        -> assign_number (pve e) n
				| `Bool(b)          -> assign_bool (pve e) b
				| `String(s)        -> assign_string (pve e) s
				| `Array(a)         -> assign_array ~name:(il, (snd e)) (fst x)
				| `Object(o)        -> assign_object ~name:(il, (snd e)) (fst x)
				| `Identifier(i)    -> assign_identifier (pve e) i
				| `Call(vr, ir)     -> assign_call ~name:(il, (snd x)) vr ir
				| `Add(al, ar)      -> assign_add (pve e) al ar
				| `Sub(al, ar)      -> assign_sub (pve e) al ar
				| `Mod(_, _) -> "UNSUPPORTED780"
				| `Dot(_) -> "UNSUPPORTED781"
				| _                 -> (failwith (sprintf "%s: %s" "ProScript: This identifier assignment is not supported" (pv_expression x)))
			| _ -> (failwith "ProScript: This type of assignment is not supported.")))
		| `Function(f) -> (match name with
			| Some (x, y) -> (declare_function ~name:(x, y) f loc false false);""
			| None -> (declare_function f loc false false);"")
		| _ -> "UNSUPPORTED722"
	in
	pve expr

and set_val ?(name) (vl, il) (vr, ir) =
	(match (fst vl) with
		| `Identifier(x) -> (match typeof (snd vl) with
			| Object(_) -> (match (get_type_using_shape "object" (loc_type_shape (typeof (snd vl)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "let %s = Object_%s_set_%s(%s, %s) in\n" (get_local_identifier x (snd vl)) nb il (get_local_identifier x (snd vl)) (match name with
						| Some name -> (pv_expression ~name:(name) (vr, ir))
						| None      -> (pv_expression (vr, ir)))
				| (_, false) -> sprintf "%s_%s" x il)
			| Array(_) -> (match (get_type_using_shape "array" (loc_type_shape (typeof (snd vl)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "Array_%s_set_%s(%s, %s)" nb il (get_local_identifier x (snd vl)) (match name with
						| Some name -> (pv_expression ~name:(name) (vr, ir))
						| None      -> (pv_expression (vr, ir)))
				| (_, false) -> sprintf "%s_%s" x il)
			| _ -> loc_type (typeof (snd vl)))
		| _              -> sprintf "%s_%s" (match name with
			| Some name -> (pv_expression ~name:(name) vl)
			| None      -> (pv_expression vl)) il)

and get_val e i =
	(match (fst e) with
		| `Identifier(x) -> (match typeof (snd e) with
			| Object(_) -> (match (get_type_using_shape "object" (loc_type_shape (typeof (snd e)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "Object_%s_get_%s(%s)" nb i (get_local_identifier x (snd e))
				| (_, false) -> sprintf "%s_%s" x i)
			| Array(_) -> (match (get_type_using_shape "array" (loc_type_shape (typeof (snd e)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "Array_%s_get_%s(%s)" nb i (get_local_identifier x (snd e))
				| (_, false) -> sprintf "%s_%s" x i)
			| _ -> loc_type (typeof (snd e)))
		| `Dot(dl, dr) -> (match  typeof (snd e) with
			| Object(_) -> (match (get_type_using_shape "object" (loc_type_shape (typeof (snd e)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "Object_%s_get_%s(%s)" nb i (get_val dl dr)
				| (_, false) -> sprintf "%s_%s" dr i)
			| Array(_) -> (match (get_type_using_shape "array" (loc_type_shape (typeof (snd e)))) with
				| ((na, nb, nc, nd),  true) -> sprintf "Array_%s_get_%s(%s)" nb i (get_val dl dr)
				| (_, false) -> sprintf "%s_%s" dr i)
			| _ -> loc_type (typeof (snd e)))
		| _  -> sprintf "%s_%s" (pv_expression e) i)


and pv_statement ?(fn) ((p,loc):statement_t) export =
	let rec pvs (p, loc) = match p with
		| `Empty -> "0"
		| `Expression(e) -> (pv_expression e)
		| `Block(l) -> List.fold_left (^) "" (List.map (fun (i) ->
			"\n"^(match fn with
				| Some fn -> pv_statement ~fn:(fn) i export
				| None   -> pv_statement i export)
		) l)
		| `Switch(se, stl, sesll) -> (List.fold_left (^) "" (List.map (fun (tet, tstl) ->
				(sprintf "if (%s = %s) then (\n\t%s) else ("
					(pv_expression se)
					(pv_expression tet)
					(List.fold_left (^) "" (List.map (fun (tst) ->
						(pv_statement tst false))
					tstl))
				)
			) sesll))^(match stl with
				| None -> ""
				| Some stle -> (List.fold_left (^) "" (
					List.map (fun (st) -> (pv_statement st export))
				stle)))^(List.fold_left (^) "" (List.map (fun (tet, tstl) ->
					(sprintf ")")) sesll))^(match stl with
						| None -> ""
						| Some stle -> "")
		| `If(c, t, f) -> (match fn with
			| Some fn -> "if ("^(pv_expression c)^") then ("^(pv_statement ~fn:(fn) t export)^")\n"^(match f with
				| Some f -> "else ("^(
					let pf = (pv_statement ~fn:(fn) f export) in
					(match (String.length pf) with
						| 0 -> "0"
						| _ -> pf)
				)^")"
				| None   -> "")
			| None    -> "if ("^(pv_expression c)^") then ("^(pv_statement t export)^")\n"^(match f with
				| Some f -> " else ("^(
					let pf = (pv_statement f export) in
					(match (String.length pf) with
						| 0 -> "0"
						| _ -> pf)
				)^")"
				| None   -> ""))
		| `Return(e) -> (sprintf "%s%s%s"
			(match export with
				| true -> "\nout(io, "
				| false -> "") (match fn with
			| Some fn -> (match e with
				| Some e -> (match (fst e) with
					| `Object(o)     -> return_object (fst e)
					| `Array(a)      -> return_array (fst e)
					| `Call(x, y)    -> assign_call x y
					| `Number(x)     -> number_def_ref x
					| `String(x)     -> string_def_ref x
					| `Identifier(x) -> get_local_identifier x (snd e)
					| `Dot(v, l)     -> get_val v l
					| `Bool(x)       -> (match x with
						| true -> "true"
						| false -> "false")
					| _           -> failwith "UNSUPPORTED770")
				| None   -> "0")
			| None    -> (match e with
				| Some e -> (match (fst e) with
					| `Object(o)  -> return_object (fst e)
					| `Array(a)   -> return_array (fst e)
					| `Call(x, y) -> assign_call x y
					| `Number(x)  -> number_def_ref x
					| `String(x)   -> string_def_ref x
					| `Identifier(x) -> get_local_identifier x (snd e)
					| `Dot(v, l)     -> get_val v l
					| `Bool(x)       -> (match x with
						| true -> "true"
						| false -> "false")
					| _           -> failwith "UNSUPPORTED784")^"\n"
				| None   -> "0")) (match export with
					| true -> ")"
					| false -> ""))
		| `Declaration(l) | `Const(l) -> List.fold_left (^) "" (List.map (fun (i, e) ->
			(match i with
				| "ProScript" -> ""
				| _ -> (match e with
					| None -> ""
					| Some x -> (match (fst x) with
						| `Object(o)   -> (assign_object   ~name:(i, (snd x)) (fst x))^"\n"
						| `Array(a)    -> (assign_array    ~name:(i, (snd x)) (fst x))^"\n"
						| `Call(v, l)  -> (assign_call     ~name:(i, (snd x)) v l)
						| `Function(f) -> (declare_function ~name:(i, (snd x)) f (snd x) false false);""
						| _            -> (sprintf "let %s = %s in\n" (get_local_identifier i (snd x)) (pv_expression x)))
			))) l)
		| _ -> "UNSUPPORTED501"
	in
	pvs (p, loc)

and prepare_includes x =
	inclu_c := !inclu_c^"fun add(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun str_add(bitstring, bitstring):bitstring [data].\n";
	inclu_c := !inclu_c^"fun modulo(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun divide(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun multiply(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun subtract(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitShiftLeft(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitShiftRight(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitFillZero(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun lessThan(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun greaterThan(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun lessThanOrEqual(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun greaterThanOrEqual(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitAND(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitXOR(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitOR(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun bitNOT(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun minus(number, number):number [data].\n";
	inclu_c := !inclu_c^"fun plus(number, number):number [data].\n";
	(match (get_type_using_name "key", (get_type_using_name "iv")) with
		| ((_, true), (_, true)) ->
			let g = [
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(0), (Lexing.dummy_pos, Lexing.dummy_pos));
				(`Byte(9), (Lexing.dummy_pos, Lexing.dummy_pos));
			] in
			let gr = (Lexing.dummy_pos, Lexing.dummy_pos) in
			(match (array_val_is_def g) with
				| true  -> ()
				| false -> (pv_expression (`Array(g), gr)); ());
			incr decl_i;
			let gref = (array_val_def_ref g "key" !decl_i) in
			inclu_c:=!inclu_c^
"fun one_way(key, bitstring):key.
fun ProScript_crypto_DH25519(key, key):key.
equation forall a:key, b:key;
	ProScript_crypto_DH25519(b, ProScript_crypto_DH25519(a, "^gref^")) =
	ProScript_crypto_DH25519(a, ProScript_crypto_DH25519(b, "^gref^")).
(*
fun ProScript_crypto_DHP256(key, key):key.
fun ProScript_crypto_DHP256Public(key):key.
equation forall a:key, b:key;
	ProScript_crypto_DHP256(a, ProScript_crypto_DHP256Public(b)) =
	ProScript_crypto_DHP256(b, ProScript_crypto_DHP256Public(b)).
*)
fun AESGCM_encrypt(key, iv, bitstring, bitstring):bitstring.
fun AESGCM_tag(key, iv, bitstring, bitstring):bitstring.
reduc forall k:key, i:iv, m:bitstring, a:bitstring; ProScript_crypto_AESGCMEncrypt(k, i, m, a) =
	Object_aesgcmencryptresult(
		AESGCM_encrypt(k, i, m, a),
		AESGCM_tag(k, i, m, a)
	).

fun ProScript_crypto_AESGCMDecrypt(key, iv, object_aesgcmencryptresult, bitstring):object_aesgcmdecryptresult reduc
	forall k:key, i:iv, m:bitstring, a:bitstring;
		ProScript_crypto_AESGCMDecrypt(k, i, Object_aesgcmencryptresult(
			AESGCM_encrypt(k, i, m, a), AESGCM_tag(k, i, m, a)), a
		) = Object_aesgcmdecryptresult(m, true)
	otherwise forall k:key, i:iv, c:object_aesgcmencryptresult, a:bitstring;
		ProScript_crypto_AESGCMDecrypt(k, i, c, a) = Object_aesgcmdecryptresult(string_1, false).
fun ProScript_crypto_AESCTREncrypt(key, iv, bitstring):bitstring.
reduc forall k:key, i:iv, m:bitstring; ProScript_crypto_AESCTRDecrypt(
	k, i, ProScript_crypto_AESCTREncrypt(k, i, m)
) = m.
(* reduc forall k:key, i:iv, m1:bitstring, m2:bitstring;
	AESCTRReuse(ProScript_crypto_AESCTREncrypt(k, i, m1), ProScript_crypto_AESCTREncrypt(k, i, m2)) = (m1, m2). *)
fun ProScript_crypto_SHA256(bitstring):bitstring.
fun ProScript_crypto_ED25519_publicKey(key):key.
fun ProScript_crypto_ED25519_signature(bitstring, key, key):bitstring.
fun ProScript_crypto_ED25519_checkValid(bitstring, bitstring, key):bool reduc
	forall m:bitstring, sk:key;
		ProScript_crypto_ED25519_checkValid(
			ProScript_crypto_ED25519_signature(m, sk, ProScript_crypto_ED25519_publicKey(sk)),
			m,
			ProScript_crypto_ED25519_publicKey(sk)
		) = true
	otherwise forall s:bitstring, m:bitstring, pk:key;
		ProScript_crypto_ED25519_checkValid(s, m, pk) = false.
fun ProScript_crypto_HMACSHA256(key, bitstring):key.
reduc forall k:key, m:bitstring; ProScript_crypto_checkHMACSHA256(k, m, ProScript_crypto_HMACSHA256(k, m)) = true.
letfun ProScript_crypto_random32Bytes(x:bitstring) = new secrkey[]:key; secrkey.
letfun ProScript_crypto_random12Bytes(x:bitstring) = new secriv[]:iv; secriv.
fun ProScript_crypto_HKDF_expand(bitstring, bitstring, bitstring, number):bitstring.
fun ProScript_crypto_tls12_prf_label(bitstring, bitstring, number, number):bitstring.\n\n"
		| _ -> ())

and pv_program (program:Ast.t) =
	let print acc = function
		| (`Statement(s),loc) -> "\t"^acc^(pv_statement s false)
		| (`FunctionDeclaration(f),loc) -> "\t"^acc^"(* Named function *)\n"
	in
	(List.fold_left print "" program);
	(prepare_includes 0); ""

let pv p =
	let free = List.fold_left (^) "" (List.map (
		fun (i,(t,_)) -> sprintf "%s: %s\n\n%!" i (sort_free (head t.t))
	) (List.rev !free_env)) in

	let csts t l = List.fold_left (^) "" (List.map (
		fun (v,x) -> (sprintf "const %s:%s [data]. (* %s *)\n" x t v)
	) (List.rev l)) in

	let cstf t l = List.fold_left (^) "" (List.map (
		fun (v,x) -> (sprintf "const %s:%s [data]. (* %f *)\n" x t v)
	) (List.rev l)) in

	let cst2 l = List.fold_left (^) "" (List.map (
		fun (m, (t, y)) -> ("const "^t^":"^y^" [data].\n")
	) (List.rev l)) in

	(string_def_ref "");

	(object_def ~t:("aesgcmencryptresult") (pv_type_shape (`Object([
		(`Property("ciphertext", (`String(""), (Lexing.dummy_pos, Lexing.dummy_pos))), (Lexing.dummy_pos, Lexing.dummy_pos));
		(`Property("tag", (`String(""), (Lexing.dummy_pos, Lexing.dummy_pos))), (Lexing.dummy_pos, Lexing.dummy_pos))
	]))));

	(object_def ~t:("aesgcmdecryptresult") (pv_type_shape (`Object([
		(`Property("plaintext", (`String(""), (Lexing.dummy_pos, Lexing.dummy_pos))), (Lexing.dummy_pos, Lexing.dummy_pos));
		(`Property("valid", (`Bool(false), (Lexing.dummy_pos, Lexing.dummy_pos))), (Lexing.dummy_pos, Lexing.dummy_pos))
	]))));

	"free io:channel.\n\n"^
	"type number.\n"^
	"type function.\n"^
	"type args.\n"^
	"type return.\n"^
	(!types_c)^"\n"^
	(!tyfun_c)^"\n"^
	(!shape_c)^"\n"^
	(cstf "number" !number_c)^
	(csts "bitstring" !string_c)^
	(cst2 !consts_c)^"\n"^
	"const nullArgs:args [data].\n"^
	(!constm_c)^"\n"^
	(!tablem_c)^"\n"^
	(!inclu_c)^"\n"^
	(!calls_c)^"\n\n"^
	((strip_first_char !export_c))^
	(pv_program p)
