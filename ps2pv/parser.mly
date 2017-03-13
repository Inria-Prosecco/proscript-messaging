%{

let check_array l = match List.rev l with
	| (`Elision,_) :: t -> List.rev t
	| _ -> l

let clean_slist = 
	let rec delempty = function (`Empty,_)::t -> delempty t | _ as l -> l in
	fun l->List.rev (delempty (List.rev (delempty l)))

let clean_fbody = 
  let rec delempty = function (`Statement((`Empty,_)),_)::t -> delempty t | _ as l -> l in
  fun l->List.rev (delempty (List.rev (delempty l)))

%}

%token NULL
%token DO WHILE FOR BREAK CONTINUE
%token INSTANCEOF TYPEOF
%token NEW VAR CONST VOID DELETE
%token TRY CATCH FINALLY THROW
%token SWITCH CASE DEFAULT
%token DEBUGGER
%token FUNCTION THIS RETURN
%token LP LB LC RC RB RP
%token DOT SEMI COMMA QM COLON LNOT BNOT
%token LT LE GT GE EQ NEQ SEQ SNEQ
%token PLUS MINUS TIMES MOD BAND BOR XOR
%token INCR DECR RSH LSH ARSH LAND LOR
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN OR_ASSIGN
%token AND_ASSIGN XOR_ASSIGN RSH_ASSIGN LSH_ASSIGN ARSH_ASSIGN
%token MOD_ASSIGN DIV DIV_ASSIGN
%token WITH IN
%token IF ELSE
%token EOL EOF

%token <bool> BOOL
%token <string> KEYWORD
%token <string> RESERVED
%token <string> IDENTIFIER
%token <string> STRING
%token <string*string> REGEXP
%token <float> NUMBER
%token <int> BYTE
%start <Ast.t> main

%nonassoc NOELSE
%nonassoc ELSE

%left NOLP
%left LP
%left LB
%right DOT

%nonassoc NOSEMI
%nonassoc SEMI
%nonassoc EOL

%%

main:
| a = list(source_element) EOF {(clean_fbody a)}

source_element:
| s=terminated_statement {(`Statement(s),($startpos,$endpos))}
| f=optional_semi(function_decl) {(`FunctionDeclaration(f),($startpos,$endpos))} 

optional_semi(X):
| s=X terminator {s}
| s=X %prec NOSEMI {s}

terminator:
| SEMI {}
| line_breaks {}

line_breaks:
| EOL line_breaks {}
| EOL {}

re_semi:
| SEMI {Globals.re:=true}

terminated_statement_list:
| l = list(terminated_statement) {(clean_slist l)}

terminated_statement:
| terminator {(`Empty,($startpos,$endpos))}
| s=ne_statement {s}

st_statement:
| SEMI {(`Empty,($startpos,$endpos))}
| ne_statement {$1}

ne_statement:
| s=statement terminator {s}
| b=optional_semi(block) {b}
| IF LP c=par_expression RP EOL* t=st_statement %prec NOELSE {(`If(c, t, None),($startpos,$endpos))}
| IF LP c=par_expression RP EOL* t=st_statement ELSE f=st_statement {(`If(c, t, Some f),($startpos,$endpos))}
| FOR LP i=for_init? SEMI c=expression? re_semi l=par_expression? RP EOL* s=st_statement {(`For(i,c,l,s),($startpos,$endpos))}
| FOR LP v=forin IN e=par_expression RP EOL* s=st_statement {(`Forin(v,e,s),($startpos,$endpos))}
| WHILE LP e=par_expression RP EOL* s=st_statement {(`While(e,s),($startpos,$endpos))}
| WITH LP e=par_expression RP EOL* s=st_statement {(`With(e,s),($startpos,$endpos))}
| SWITCH LP e=par_expression RP EOL* LC EOL* c=cases RC optional_semi(EOL) {(`Switch(e, fst c, snd c),($startpos,$endpos))}
| DO s=st_statement WHILE LP e=par_expression optional_semi(RP) {(`Do(s,e),($startpos,$endpos))}
| TRY b=block CATCH LP i=IDENTIFIER RP EOL* c=optional_semi(block) {(`Try(b,Some(i,c),None),($startpos,$endpos))}
| TRY b=block CATCH LP i=IDENTIFIER RP EOL* c=block FINALLY f=optional_semi(block) {(`Try(b,Some(i,c),Some f),($startpos,$endpos))}
| TRY b=block FINALLY f=optional_semi(block) {(`Try(b,None,Some f),($startpos,$endpos))}
| i=IDENTIFIER COLON s=st_statement {(`Label(i, s),($startpos,$endpos))} 

function_decl:
| FUNCTION n = IDENTIFIER LP args = separated_list(COMMA, IDENTIFIER) RP EOL* LC body=list(source_element) RC EOL 
  {(Some n,args,clean_fbody body)}

statement:
| e = statement_expression {(`Expression(e),($startpos,$endpos))}
| v = var_decl(full_commaless_expression)   {(`Declaration(v),($startpos,$endpos))}
| c = const_decl(full_commaless_expression) {(`Const(c),($startpos,$endpos))}
| RETURN e=expression? {(`Return(e),($startpos,$endpos))}
| THROW e=expression {(`Throw(e),($startpos,$endpos))}
| CONTINUE i=IDENTIFIER? {(`Continue(i),($startpos,$endpos))}
| BREAK i=IDENTIFIER? {(`Break(i),($startpos,$endpos))}
| DEBUGGER {(`Debugger,($startpos,$endpos))}

var_decl(X):
| VAR v = separated_nonempty_list(COMMA, var_declaration(X)) {v}

const_decl(X):
| CONST c = separated_nonempty_list(COMMA, const_declaration(X)) {c}

for_init:
| e = inless_sequence {(`Expression(e),($startpos,$endpos))}
| v = var_decl(inless_expression) {(`Declaration(v),($startpos,$endpos))}

forin:
| e     = lhs_expression(base_expression) {(`Expression(e),($startpos,$endpos))}
| VAR v = var_declaration(inless_expression) {(`Declaration([v]),($startpos,$endpos))}

cases:
| c=list(case) {(None, c)}
| c1=list(case) d=default_case c2=list(case) {(Some d, c1@c2)}

case:
| CASE e=expression COLON l=terminated_statement_list {(e,l)}

default_case:
| DEFAULT COLON l=terminated_statement_list {l}

block:
| LC b = terminated_statement_list RC EOL {(`Block(b),($startpos,$endpos))}

var_declaration(X):
| IDENTIFIER {($1, None)}
| IDENTIFIER ASSIGN X(base_expression) {($1, Some $3)}

const_declaration(X):
| IDENTIFIER {($1, None)}
| IDENTIFIER ASSIGN X(base_expression) {($1, Some $3)}

par_expression:
| e=expression {Globals.re:=true; e}

expression:
| full_commaless_expression(base_expression) {$1}
| e = full_commaless_expression(base_expression) COMMA
  s = separated_nonempty_list(COMMA, full_commaless_expression(base_expression)) {(`Sequence(e::s),($startpos,$endpos))}

inless_sequence:
| inless_expression(base_expression) {$1}
| e = inless_expression(base_expression) COMMA
  s = separated_nonempty_list(COMMA, inless_expression(base_expression)) {(`Sequence(e::s),($startpos,$endpos))}

(* Statement expressions can't start with { or function *)
statement_expression:
| full_commaless_expression(objless_funcless_expression) {$1}
| e = full_commaless_expression(objless_funcless_expression) COMMA
  s = separated_nonempty_list(COMMA, full_commaless_expression(base_expression)) {(`Sequence(e::s),($startpos,$endpos))}

full_commaless_expression(X):
| commaless_expression(X, in_expression) {$1}

base_full_commaless_expression:
| full_commaless_expression(base_expression) {$1}

inless_expression(X):
| commaless_expression(X, cmp_expression) {$1}

commaless_expression(X,Y):	
| e=lhs_expression(X) o=assignop f=base_commaless_expression(Y) {o(e,f)}
| cond_expression(X,Y) {$1}

base_commaless_expression(Y):  
| e=lhs_expression(base_expression) o=assignop f=base_commaless_expression(Y) {o(e,f)}
| base_cond_expression(Y) {$1}

cond_expression(X,Y):
| c=lor_expression(X,Y) QM t=base_full_commaless_expression COLON f = base_commaless_expression(Y) 
  {(`Conditional(c, t, f),($startpos,$endpos))}
| lor_expression(X,Y) {$1}

base_cond_expression(Y):
| c=base_lor_expression(Y) QM t=base_full_commaless_expression COLON f = base_commaless_expression(Y)
  {(`Conditional(c, t, f),($startpos,$endpos))}
| base_lor_expression(Y) {$1}

lor_expression(X,Y):
| e=lor_expression(X,Y) LOR f=base_land_expression(Y) {(`Lor(e, f),($startpos,$endpos))}
| land_expression(X,Y) {$1}

base_lor_expression(Y):
| e=base_lor_expression(Y) LOR f=base_land_expression(Y) {(`Lor(e, f),($startpos,$endpos))}
| base_land_expression(Y) {$1}

land_expression(X,Y):
| e=land_expression(X,Y) LAND f=base_bor_expression(Y) {(`Land(e, f),($startpos,$endpos))}
| bor_expression(X,Y) {$1}

base_land_expression(Y):
| e=base_land_expression(Y) LAND f=base_bor_expression(Y) {(`Land(e, f),($startpos,$endpos))}
| base_bor_expression(Y) {$1}

bor_expression(X,Y):
| e=bor_expression(X,Y) BOR f=base_xor_expression(Y) {(`Bor(e, f),($startpos,$endpos))}
| xor_expression(X,Y) {$1}

base_bor_expression(Y):
| e=base_bor_expression(Y) BOR f=base_xor_expression(Y) {(`Bor(e, f),($startpos,$endpos))}
| base_xor_expression(Y) {$1}

xor_expression(X,Y):
| e=xor_expression(X,Y) XOR f=base_band_expression(Y) {(`Bxor(e, f),($startpos,$endpos))}
| band_expression(X,Y) {$1}

base_xor_expression(Y):
| e=base_xor_expression(Y) XOR f=base_band_expression(Y) {(`Bxor(e, f),($startpos,$endpos))}
| base_band_expression(Y) {$1}

band_expression(X,Y):
| e=band_expression(X,Y) BAND f=base_eq_expression(Y) {(`Band(e, f),($startpos,$endpos))}
| eq_expression(X,Y) {$1}

base_band_expression(Y):
| e=base_band_expression(Y) BAND f=base_eq_expression(Y) {(`Band(e, f),($startpos,$endpos))}
| base_eq_expression(Y) {$1}

eq_expression(X,Y):
| e=eq_expression(X,Y) o=eqop f=Y(base_expression) {o (e, f)}
| Y(X) {$1}

base_eq_expression(Y):
| e=base_eq_expression(Y) o=eqop f=Y(base_expression) {o (e, f)}
| Y(base_expression) {$1}

in_expression(X):
| e=in_expression(X) IN f=base_shift_expression {(`In(e, f),($startpos,$endpos))}
| cmp_expression(X) {$1}

cmp_expression(X):
| e=cmp_expression(X) o=cmpop f=base_shift_expression {o (e, f)}
| shift_expression(X) {$1}

shift_expression(X):
| e=shift_expression(X) o=shiftop f=base_add_expression {o (e, f)}
| add_expression(X) {$1}

%inline base_shift_expression:
| e=shift_expression(base_expression) {e}

add_expression(X):
| e=add_expression(X) o=addop f=base_mul_expression {o (e, f)}
| mul_expression(X) {$1}

%inline base_add_expression:
| e=add_expression(base_expression) {e}

mul_expression(X):
| e=mul_expression(X) o=mulop f=base_un_expression {o (e, f)}
| un_expression(X) {$1}

%inline base_mul_expression:
| e=mul_expression(base_expression) {e}

un_expression(X):
| o = unop e = base_un_expression {o(e)}
| INCR EOL* e = base_un_expression {(`Preincr(e),($startpos,$endpos))}
| DECR EOL* e = base_un_expression {(`Predecr(e),($startpos,$endpos))}
| postfix_expression(X) {$1}

%inline base_un_expression:
| e=un_expression(base_expression) {e}

postfix_expression(X):
| e=lhs_expression(X) INCR {(`Postincr(e),($startpos,$endpos))}
| e=lhs_expression(X) DECR {(`Postdecr(e),($startpos,$endpos))}
| e=lhs_expression(X) {e}

%inline assignop:
| ASSIGN {fun a->(`Assign(a),($startpos,$endpos))}
| PLUS_ASSIGN {fun (a,b)->(`Assign(a, (`Add(a,b),($startpos,$endpos))),($startpos,$endpos))}
| MINUS_ASSIGN {fun (a,b)->(`Assign(a, (`Sub(a,b),($startpos,$endpos))),($startpos,$endpos))}
| TIMES_ASSIGN {fun (a,b)->(`Assign(a, (`Multiply(a,b),($startpos,$endpos))),($startpos,$endpos))}
| DIV_ASSIGN {fun (a,b)->(`Assign(a, (`Divide(a,b),($startpos,$endpos))),($startpos,$endpos))}
| MOD_ASSIGN {fun (a,b)->(`Assign(a, (`Mod(a,b),($startpos,$endpos))),($startpos,$endpos))}
| LSH_ASSIGN {fun (a,b)->(`Assign(a, (`Lsh(a,b),($startpos,$endpos))),($startpos,$endpos))}
| RSH_ASSIGN {fun (a,b)->(`Assign(a, (`Rsh(a,b),($startpos,$endpos))),($startpos,$endpos))}
| ARSH_ASSIGN {fun a->(`Ashassign(a),($startpos,$endpos))}
| XOR_ASSIGN {fun (a,b)->(`Assign(a, (`Bxor(a,b),($startpos,$endpos))),($startpos,$endpos))}
| AND_ASSIGN {fun (a,b)->(`Assign(a, (`Band(a,b),($startpos,$endpos))),($startpos,$endpos))}
| OR_ASSIGN {fun (a,b)->(`Assign(a, (`Bor(a,b),($startpos,$endpos))),($startpos,$endpos))}

%inline unop:
| DELETE {fun a->(`Delete(a),($startpos,$endpos))}
| VOID {fun a->(`Void(a),($startpos,$endpos))}
| TYPEOF {fun a->(`Typeof(a),($startpos,$endpos))}
| PLUS {fun a->(`Plus(a),($startpos,$endpos))}
| MINUS {fun a->(`Minus(a),($startpos,$endpos))}
| LNOT {fun a->(`Lnot(a),($startpos,$endpos))}
| BNOT {fun a->(`Bnot(a),($startpos,$endpos))}

%inline eqop:
| EQ {fun a->(`Equal(a),($startpos,$endpos))}
| NEQ {fun a->(`Lnot((`Equal(a),($startpos,$endpos))),($startpos,$endpos))}
| SEQ {fun a->(`Sequal(a),($startpos,$endpos))}
| SNEQ {fun a->(`Lnot((`Sequal(a),($startpos,$endpos))),($startpos,$endpos))}

%inline cmpop:
| INSTANCEOF {fun a->(`Instanceof(a),($startpos,$endpos))}
| GT {fun a->(`Gt(a),($startpos,$endpos))}
| LT {fun a->(`Lt(a),($startpos,$endpos))}
| GE {fun a->(`Ge(a),($startpos,$endpos))}
| LE {fun a->(`Le(a),($startpos,$endpos))}

%inline shiftop:
| LSH {fun a->(`Lsh(a),($startpos,$endpos))}
| RSH {fun a->(`Rsh(a),($startpos,$endpos))}
| ARSH {fun a->(`Ash(a),($startpos,$endpos))}

%inline addop:
| PLUS {fun a->(`Add(a),($startpos,$endpos))}
| MINUS {fun a->(`Sub(a),($startpos,$endpos))}

%inline mulop:
| TIMES {fun a->(`Multiply(a),($startpos,$endpos))}
| DIV {fun a->(`Divide(a),($startpos,$endpos))}
| MOD {fun a->(`Mod(a),($startpos,$endpos))}

lhs_expression(X):
| NEW e = new_expression {(`New(e),($startpos,$endpos))}
| e = newless_expression(X) {e}

newless_expression(X):
| f = lhs_expression(X) args = arguments {(`Call(f, args),($startpos,$endpos))}
| o = lhs_expression(X) LB p = expression RB {(`Property(o,p),($startpos,$endpos))}
| a = lhs_expression(X) DOT i = property_name {(`Dot(a, i),($startpos,$endpos))}
| X {$1}

new_expression:
| e = final_new_expression %prec NOLP {(e,None)}
| e = final_new_expression args = arguments {(e, Some args)}

final_new_expression:
| NEW e = new_expression {(`New(e),($startpos,$endpos))}
| o = final_new_expression LB p = expression RB {(`Property(o,p),($startpos,$endpos))}
| a = final_new_expression DOT i = property_name {(`Dot(a, i),($startpos,$endpos))}
| base_expression {$1}

arguments:
| LP args = separated_list(COMMA, full_commaless_expression(base_expression)) RP {args}

(* Allows object literals and functions *)
base_expression:
| LC l = properties EOL RC EOL {(`Object(l),($startpos,$endpos))}
| FUNCTION n = IDENTIFIER? LP args = separated_list(COMMA, IDENTIFIER) RP EOL* LC b=function_body RC EOL
  {(`Function(n,args,b),($startpos,$endpos))}
| objless_funcless_expression {$1}

function_body:
| body=list(source_element) {Globals.re:=false; clean_fbody body}

objless_funcless_expression:
| LP e = expression RP {e}
| LB l = list_elements RB {(`Array(check_array l),($startpos,$endpos))}
| THIS {(`This,($startpos,$endpos))}
| NULL {(`Null,($startpos,$endpos))}
| b = BOOL {(`Bool(b),($startpos,$endpos))}
| n = NUMBER {(`Number(n),($startpos,$endpos))}
| y = BYTE {(`Byte(y),($startpos,$endpos))}
| s = STRING {(`String(s),($startpos,$endpos))}
| r = REGEXP {(`Regexp(r),($startpos,$endpos))}
| i = IDENTIFIER {(`Identifier(i),($startpos,$endpos))}

list_elements:
| l=list_element {[l]}
| e=list_element COMMA l=list_elements {e::l}

list_element:
| {(`Elision,($startpos,$endpos))}
| full_commaless_expression(base_expression) {$1}

properties:
| {Globals.re:=false; []}
| p=property {Globals.re:=false; [p]}
| p=property COMMA l=properties {p::l}

(* Reserved words are valid property names... *)
property_name:
| IDENTIFIER {$1}	
| NUMBER {Globals.cutdot (string_of_float $1)}
| BOOL {string_of_bool $1}
| STRING {$1} | KEYWORD {$1} | RESERVED {$1}
| NULL {"null"} | INSTANCEOF {"instanceof"} | TYPEOF {"typeof"}
| DO {"do"} | FOR {"for"} | WHILE {"while"} | BREAK {"break"} | CONTINUE {"continue"} | FINALLY {"finally"}
| NEW {"new"} | VAR {"var"} | | CONST {"const"} | DELETE {"delete"} | SWITCH {"switch"} | CASE {"case"} | DEFAULT {"default"}
| FUNCTION {"function"} | THIS {"this"} | RETURN {"return"} | VOID {"void"} | WITH {"with"} | IN {"in"}
| IF {"if"} | ELSE {"else"} | DEBUGGER {"debugger"} | TRY {"try"} | THROW {"throw"} | CATCH {"catch"}

(* Getters and setters are poorly hacked into the standard *)
property:
| property_name COLON full_commaless_expression(base_expression) {(`Property($1,$3),($startpos,$endpos))}
| t=IDENTIFIER p=property_name LP args = separated_list(COMMA, IDENTIFIER) RP EOL* LC body=list(source_element) RC EOL 
  {match t with "get"->(`Getter(p,(None,args,clean_fbody body)),($startpos,$endpos))
	 | "set" -> (`Getter(p,(None,args,clean_fbody body)),($startpos,$endpos))
	 | _ -> Globals.lerr "Expected get or set"}
