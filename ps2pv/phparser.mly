%{

(** Converted from zend_language_parser.y for PHP 5.4 **)

%}

%token EOF
%token INCLUDE INCLUDE_ONCE EVAL REQUIRE REQUIRE_ONCE
%token COMMA SEMI DOLLAR
%token PRINT YIELD ECHO

%token CONSTANT_ENCAPSED_STRING
%token EXIT

%token IF ELSE ELSEIF
%token INT_CAST BOOL_CAST FLOAT_CAST STRING_CAST ARRAY_CAST OBJECT_CAST UNSET_CAST
%token FINAL
%token NEW CLONE INSTANCEOF

%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN DOT_ASSIGN
%token MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSH_ASSIGN RSH_ASSIGN
%token QM COLON
%token BOR BAND BXOR BNOT
%token LOR LAND LXOR LNOT
%token INCR DECR AT

%token EQUAL NEQUAL SEQUAL NSEQUAL
%token LT LE GT GE
%token LSH RSH PLUS MINUS DOT MUL DIV MOD
%token LP LB LC RC RB RP

%token DO WHILE FOR FOREACH DECLARE AS SWITCH CASE DEFAULT BREAK
%token CONTINUE GOTO FUNCTION CONST RETURN TRY CATCH FINALLY
%token THROW USE INSTEADOF GLOBAL STATIC ABSTRACT PRIVATE
%token PROTECTED PUBLIC VAR UNSET ISSET EMPTY HALT CLASS
%token TRAIT INTERFACE EXTENDS IMPLEMENTS ARROW DBLARROW
%token LIST ARRAY CALLABLE SCLASS STRAIT SMETHOD SFUNCTION
%token SLINE SFILE SDIR SNAMESPACE BACKSL 
%token NAMESPACE DBLCOLON DOLLAR_CURLY

%nonassoc LOW
%left INCLUDE INCLUDE_ONCE REQUIRE REQUIRE_ONCE
%left LOR LAND LXOR
%right PRINT
%left ASSIGN PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN DOT_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSH_ASSIGN RSH_ASSIGN
%left QM COLON
%left BOR BAND BXOR
%nonassoc EQUAL NEQUAL SEQUAL NSEQUAL LT LE GT GE
%left LSH RSH PLUS MINUS DOT MUL DIV MOD
%right LNOT
%nonassoc INSTANCEOF
%right INCR INT_CAST FLOAT_CAST STRING_CAST ARRAY_CAST OBJECT_CAST BOOL_CAST UNSET_CAST 
%right BNOT AT
%nonassoc CLONE
%left ELSEIF
%left ELSE
%nonassoc YIELD

%token <string> INLINE_HTML
%token <float> DNUMBER
%token <int> LNUMBER
%token <string> STRING
%token <string> VARIABLE
%token <string> LABEL
%start <Phpast.t> start

%%

start:
| l=list(top_statement) EOF {l}

namespace_name:
| l=separated_nonempty_list(BACKSL, LABEL) {l}

top_statement:
| s=inner_statement {(`Topstatement(s), ($startpos,$endpos))}
| NAMESPACE n=namespace_name SEMI {(`Namespace(n),($startpos,$endpos))}
| NAMESPACE n=namespace_name? LC s=start RC {(`Namespaceblock(n,s),($startpos,$endpos))}
| USE l=separated_nonempty_list(COMMA, use_decl) SEMI {(`Use(l),($startpos,$endpos))}
| CONST l=separated_nonempty_list(COMMA, constant_declaration) SEMI {(`Constant(l),($startpos,$endpos))}

opt_label:
| AS l=LABEL {l}

use_decl:
| BACKSL? n=namespace_name l=opt_label? {(n,l)}

constant_declaration:
| l=LABEL ASSIGN s=static_scalar {(l,s)}

inner_statement_list:
| l=list(inner_statement) {l}

inner_statement:
| s=statement {(`Statement(s),($startpos,$endpos))}
| FUNCTION r=is_reference n=LABEL LP p=parameter_list RP LC l=list(inner_statement) RC {(`Fundecl(n,r,p,l),($startpos,$endpos))}
| c=class_declaration_statement {(`Classdecl(c),($startpos,$endpos))}
| HALT LP RP SEMI {(`Halt,($startpos,$endpos))}

statement:
| l=LABEL COLON {(`Label(l),($startpos,$endpos))}
| LC l=inner_statement_list RC {(`Block(l),($startpos,$endpos))}
| IF e=parenthesis_expr s=statement el=elseif_list fe=felse {(`If(e, s, el, fe),($startpos,$endpos))}
| WHILE e=parenthesis_expr s=statement {(`While(e,s),($startpos,$endpos))}
| DO s=statement WHILE e=parenthesis_expr SEMI {(`Do(s,e),($startpos,$endpos))}
| FOR LP e=for_expr SEMI f=for_expr SEMI g=for_expr RP s=statement {(`For(e,f,g,s),($startpos,$endpos))}
| SWITCH e=parenthesis_expr LC SEMI? l=list(switch_case) RC {(`Switch(e,l),($startpos,$endpos))}
| BREAK n=LNUMBER? SEMI {(`Break(n),($startpos,$endpos))}
| CONTINUE n=LNUMBER? SEMI {(`Continue(n),($startpos,$endpos))}
| RETURN e=expr? SEMI {(`Return(e),($startpos,$endpos))}
| y=yield_expr SEMI {(`Yield(y),($startpos,$endpos))}
| GLOBAL l=separated_nonempty_list(COMMA, global_var) SEMI {(`Global(l),($startpos,$endpos))}
| STATIC l=separated_nonempty_list(COMMA, static_var) SEMI {(`Static(l),($startpos,$endpos))}
| ECHO l=separated_nonempty_list(COMMA, expr) SEMI {(`Echo(l),($startpos,$endpos))}
| h=INLINE_HTML {(`Inlinehtml(h),($startpos,$endpos))}
| e=expr SEMI {(`Expression(e),($startpos,$endpos))}
| UNSET LP l=separated_nonempty_list(COMMA, variable) RP SEMI {(`Unset(l),($startpos,$endpos))}
| FOREACH LP e=expr AS f=foreach_variable g=foreach_optional_arg? RP s=statement {(`Foreach(e,f,g,s),($startpos,$endpos))}
| DECLARE LP l=separated_nonempty_list(COMMA, declare) RP s=statement {(`Declare(l,s),($startpos,$endpos))}
| SEMI {(`Empty,($startpos,$endpos))}
| TRY LC l=inner_statement_list RC c=list(catch_statement) f=finally_statement? {(`Try(l,c,f),($startpos,$endpos))}
| THROW e=expr SEMI {(`Throw(e),($startpos,$endpos))}
| GOTO l=LABEL SEMI {(`Goto(l),($startpos,$endpos))}

catch_statement:
| CATCH LP n=fully_qualified_class_name v=VARIABLE RP LC l=inner_statement_list RC {((n,v),l)}

finally_statement:
| FINALLY LC l=inner_statement_list RC {l}

class_entry_type:
| CLASS {`Class}
| ABSTRACT CLASS {`Abstract}
| TRAIT {`Trait}
| FINAL CLASS {`Final}

class_declaration_statement:
| t=class_entry_type n=LABEL e=extends_from? i=implements_list LC l=list(class_statement) RC {`Class(n,t,e,i,l)}
| INTERFACE n=LABEL e=interface_extends LC l=list(class_statement) RC {`Interface(n,e,l)}

%inline is_reference:
| {true}
| BAND {false}

extends_from:
| EXTENDS n=fully_qualified_class_name {n}

interface_extends:
| {[]}
| EXTENDS l=separated_nonempty_list(COMMA, fully_qualified_class_name) {l}

implements_list:
| {[]}
| IMPLEMENTS l=separated_nonempty_list(COMMA, fully_qualified_class_name) {l}

foreach_optional_arg:
| DBLARROW foreach_variable {}

foreach_variable:
| variable {}
| BAND variable {}
| LIST LP assignment_list RP {}

declare:
| l=LABEL ASSIGN s=static_scalar {(l,s)}

switch_case:
| CASE e=expr case_separator l=inner_statement_list {`Case(e,l)}
| DEFAULT case_separator l=inner_statement_list {`Default(l)}

case_separator:
| COLON {}
| SEMI {}

elseif_list:
| e=elseif l=elseif_list {e::l}
| %prec LOW {[]}

elseif:
| ELSEIF e=parenthesis_expr s=statement {(e,s)}

felse:
| ELSE s=statement {Some(s)}
| %prec LOW {None}

parameter_list:
| l=separated_list(COMMA, parameter) {l}

parameter:
| c=optional_class_type? r=is_reference n=VARIABLE d=default_value? {(n,r,c,d)}

default_value:
| ASSIGN s=static_scalar {s}

optional_class_type:
| ARRAY {`Array}
| CALLABLE {`Callable}
| n=fully_qualified_class_name {`Class(n)}

function_call_parameter_list:
| LP l=separated_list(COMMA, call_parameter) RP {`Arglist(l)}
| LP y=yield_expr RP {`Argyield(y)}

call_parameter:
| e=expr {`Argexpr(e)}
| BAND v=variable {`Argref(v)}

global_var:
| v=VARIABLE {`Variable(v)}
| DOLLAR v=variable {`Variablevar(v)}
| DOLLAR LC e=expr RC {`Variableexpr(e)}

static_var:
| v=VARIABLE d=default_value? {(v,d)}

class_statement:
| variable_modifiers separated_nonempty_list(COMMA, class_variable) SEMI {}
| CONST separated_nonempty_list(COMMA, class_constant_def) SEMI {}
| trait_use_statement {}
| nonempty_list(member_modifier) FUNCTION is_reference LABEL LP parameter_list RP method_body {}

trait_use_statement:
| USE separated_nonempty_list(COMMA, fully_qualified_class_name) trait_adaptations {}

trait_adaptations:
| SEMI {}
| LC list(trait_adaptation_statement) RC {}

trait_adaptation_statement:
| trait_precedence SEMI {}
| trait_alias SEMI {}

trait_precedence:
| trait_method_reference_fq INSTEADOF separated_nonempty_list(COMMA, fully_qualified_class_name) {}

trait_method_reference:
| LABEL {}
| trait_method_reference_fq {}

trait_method_reference_fq:
| fully_qualified_class_name DBLCOLON LABEL {}

trait_alias:
| trait_method_reference AS trait_modifiers? {}

trait_modifiers:
| member_modifier LABEL {}

method_body:
| SEMI {}
| LC inner_statement_list RC {}

variable_modifiers:
| nonempty_list(member_modifier) {}
| VAR {}

member_modifier:
| PUBLIC {}
| PROTECTED {}
| PRIVATE {}
| STATIC {}
| ABSTRACT {}
| FINAL {}

class_variable:
| VARIABLE default_value? {}

class_constant_def:
| LABEL ASSIGN static_scalar {}

for_expr:
| l=separated_list(COMMA, expr) {l}

new_expr:
| NEW n=class_name_reference function_call_parameter_list? {`New(n)}

%inline assignop:
| ASSIGN {fun (a,b)->(`Assign(a,b),($startpos,$endpos))}
| PLUS_ASSIGN {fun (a,b)->(`Plus_assign(a,b),($startpos,$endpos))}
| MINUS_ASSIGN {fun (a,b)->(`Minus_assign(a,b),($startpos,$endpos))}
| MUL_ASSIGN {fun (a,b)->(`Mul_assign(a,b),($startpos,$endpos))}
| DIV_ASSIGN {fun (a,b)->(`Div_assign(a,b),($startpos,$endpos))}
| MOD_ASSIGN {fun (a,b)->(`Mod_assign(a,b),($startpos,$endpos))}
| LSH_ASSIGN {fun (a,b)->(`Lsh_assign(a,b),($startpos,$endpos))}
| RSH_ASSIGN {fun (a,b)->(`Rsh_assign(a,b),($startpos,$endpos))}
| DOT_ASSIGN {fun (a,b)->(`Dot_assign(a,b),($startpos,$endpos))}
| XOR_ASSIGN {fun (a,b)->(`Xor_assign(a,b),($startpos,$endpos))}
| AND_ASSIGN {fun (a,b)->(`And_assign(a,b),($startpos,$endpos))}
| OR_ASSIGN {fun (a,b)->(`Or_assign(a,b),($startpos,$endpos))}

%inline unop:
| LNOT {fun a->(`Lnot(a),($startpos,$endpos))}
| BNOT {fun a->(`Bnot(a),($startpos,$endpos))}
| AT {fun a->(`At(a),($startpos,$endpos))}

%inline lop:
| LAND {fun a->(`Land(a),($startpos,$endpos))}
| LOR {fun a->(`Lor(a),($startpos,$endpos))}
| LXOR {fun a->(`Lxor(a),($startpos,$endpos))}

%inline binop:
| PLUS {fun a->(`Add(a),($startpos,$endpos))}
| DOT {fun a->(`Dot(a),($startpos,$endpos))}
| MINUS {fun a->(`Sub(a),($startpos,$endpos))}
| MUL {fun a->(`Multiply(a),($startpos,$endpos))}
| DIV {fun a->(`Divide(a),($startpos,$endpos))}
| MOD {fun a->(`Mod(a),($startpos,$endpos))}
| BAND {fun a->(`Band(a),($startpos,$endpos))}
| BOR {fun a->(`Bor(a),($startpos,$endpos))}
| BXOR {fun a->(`Bxor(a),($startpos,$endpos))}
| LSH {fun a->(`Lsh(a),($startpos,$endpos))}
| RSH {fun a->(`Rsh(a),($startpos,$endpos))}

%inline cmpop:
| EQUAL {fun a->(`Equal(a),($startpos,$endpos))}
| NEQUAL {fun a->(`Lnot((`Equal(a),($startpos,$endpos))),($startpos,$endpos))}
| SEQUAL {fun a->(`Sequal(a),($startpos,$endpos))}
| NSEQUAL {fun a->(`Lnot((`Sequal(a),($startpos,$endpos))),($startpos,$endpos))}
| GT {fun a->(`Gt(a),($startpos,$endpos))}
| LT {fun a->(`Lt(a),($startpos,$endpos))}
| GE {fun a->(`Ge(a),($startpos,$endpos))}
| LE {fun a->(`Le(a),($startpos,$endpos))}

%inline castop:
| INT_CAST {fun a->(`Intcast(a),($startpos,$endpos))}
| BOOL_CAST {fun a->(`Boolcast(a),($startpos,$endpos))}
| STRING_CAST {fun a->(`Stringcast(a),($startpos,$endpos))}
| FLOAT_CAST {fun a->(`Floatcast(a),($startpos,$endpos))}
| ARRAY_CAST {fun a->(`Arraycast(a),($startpos,$endpos))}
| OBJECT_CAST {fun a->(`Objectcast(a),($startpos,$endpos))}
| UNSET_CAST {fun a->(`Unsetcast(a),($startpos,$endpos))}

expr_without_variable:
| LIST LP assignment_list RP ASSIGN expr {(`Listassign,($startpos,$endpos))}
| v=variable ASSIGN BAND w=variable {(`Assignref(v,w),($startpos,$endpos))}
| v=variable o=assignop e=expr {o(v,e)}
| CLONE e=expr {(`Clone(e),($startpos,$endpos))}
| v=variable INCR {(`Postincr(v),($startpos,$endpos))}
| INCR v=variable {(`Preincr(v),($startpos,$endpos))}
| v=variable DECR {(`Postdecr(v),($startpos,$endpos))}
| DECR v=variable {(`Predecr(v),($startpos,$endpos))}
| e=expr o=lop f=expr {o(e,f)}
| e=expr o=binop f=expr {o(e,f)}
| PLUS e=expr %prec INCR {(`Plus(e),($startpos,$endpos))}
| MINUS e=expr %prec INCR {(`Minus(e),($startpos,$endpos))}
| o=unop e=expr {o(e)}
| e=expr o=cmpop f=expr {o(e,f)}
| e=expr INSTANCEOF v=class_name_reference {(`Instanceof(e,v),($startpos,$endpos))}
| p=parenthesis_expr {p}
| n=new_expr {(n,($startpos,$endpos))}
(**| LP n=new_expr RP instance_call {(n,($startpos,$endpos))} WTF **)
| e=expr QM f=expr? COLON g=expr {(`Ternary(e,f,g),($startpos,$endpos))}
| f=internal_function {(f,($startpos,$endpos))}
| o=castop e=expr {o(e)}
| EXIT e=parenthesis_expr? {(`Exit(e),($startpos,$endpos))} 
| s=scalar {(`Scalar(s),($startpos,$endpos))}
| s=combined_scalar {(`Scalar(s),($startpos,$endpos))}
(* backticks *)
| PRINT e=expr {(`Print(e),($startpos,$endpos))}
| YIELD {(`Yieldexpr(None),($startpos,$endpos))}
| s=is_static FUNCTION r=is_reference LP l=parameter_list RP v=lexical_vars LC b=inner_statement_list RC {(`Closure(l,s,r,v,b),($startpos,$endpos))}

%inline is_static:
| {false}
| STATIC {true}

yield_value:
| DBLARROW e=expr {e}

yield_expr:
| YIELD e=expr v=yield_value? {(e,v)}

combined_scalar:
| ARRAY LP l=array_pair_list RP {(`Array(l),($startpos,$endpos))}
| LB l=array_pair_list RB {(`Array(l),($startpos,$endpos))}

lexical_vars:
| {[]}
| USE LP l=separated_nonempty_list(COMMA, lexical_var) RP {l}

lexical_var:
| r=is_reference v=VARIABLE {(v,r)}

function_call:
| f=call_prefix l=function_call_parameter_list {(`Call(f,l),($startpos,$endpos))}

call_prefix:
| n=fully_qualified_class_name {(`Classname(n),($startpos,$endpos))}
| e=class_name DBLCOLON f=variable_name {(`Staticmet(e,f),($startpos,$endpos))}
| e=class_name DBLCOLON f=variable_without_objects {(`Staticmet(e,f),($startpos,$endpos))}
| e=variable_class_name DBLCOLON f=variable_name {(`Staticmet(e,f),($startpos,$endpos))}
| e=variable_class_name DBLCOLON f=variable_without_objects {(`Staticmet(e,f),($startpos,$endpos))}
| v=variable_without_objects {v}

class_name:
| STATIC {(`Static,($startpos,$endpos))}
| n=fully_qualified_class_name {(`Classname(n),($startpos,$endpos))}

fully_qualified_class_name:
| nsprefix n=namespace_name {n}

%inline nsprefix:
| NAMESPACE BACKSL {}
| BACKSL {}
| {}

class_name_reference:
| n=class_name {n}
| n=dynamic_class_name_reference {n}

dynamic_class_name_reference:
| n=base_variable l=list(dynamic_class_name_variable_property) {List.fold_left (fun x f->f x) n l}

dynamic_class_name_variable_property:
| ARROW p=object_property {fun x->(`Objprop(x,p),($startpos,$endpos))}

common_scalar:
| n=LNUMBER {(`Number(n),($startpos,$endpos))}
| f=DNUMBER {(`Float(f),($startpos,$endpos))}
| s=STRING {(`String(s),($startpos,$endpos))}
| SLINE {(`Macro(`Line),($startpos,$endpos))}
| SFILE {(`Macro(`File),($startpos,$endpos))}
| SDIR {(`Macro(`Dir),($startpos,$endpos))}
| STRAIT {(`Macro(`Trait),($startpos,$endpos))}
| SMETHOD {(`Macro(`Method),($startpos,$endpos))}
| SFUNCTION {(`Macro(`Function),($startpos,$endpos))}
| SNAMESPACE {(`Macro(`Namespace),($startpos,$endpos))}
| SCLASS {(`Macro(`Class),($startpos,$endpos))}

static_scalar:
| s=common_scalar {s}
| n=fully_qualified_class_name {(`Const(n),($startpos,$endpos))}
| ARRAY LP l=static_array_pair_list RP {(`Array(l),($startpos,$endpos))}
| LB l=static_array_pair_list RB {(`Array(l),($startpos,$endpos))}
| n=class_name DBLCOLON l=LABEL {(`Staticval(n,l),($startpos,$endpos))}

scalar:
| c=class_constant {(`Classconst(c),($startpos,$endpos))}
| n=fully_qualified_class_name {(`Const(n),($startpos,$endpos))}
| s=common_scalar {s}
(* | DBQUOTE encaps_list DBQUOTE {} *)
(* | START_HEREDOC encaps_list END_HEREDOC {} *)

class_constant:
| n=class_name DBLCOLON l=LABEL {(n,l)}
| n=variable_class_name DBLCOLON l=LABEL {(n,l)}

static_array_pair_list:
| x=static_array_pair COMMA l=static_array_pair_list {x::l}
| x=static_array_pair {[x]}
| {[]}

array_key:
| s=static_scalar DBLARROW {(`Scalar(s),($startpos,$endpos))}

static_array_pair:
| k=array_key? v=static_scalar {(k,(`Scalar(v),($startpos,$endpos)))}

expr:
| v=variable {(`Variable(v),($startpos,$endpos))}
| e=expr_without_variable {e}

parenthesis_expr:
| LP e=expr RP {e}
| LP y=yield_expr RP {(`Yieldexpr(Some y),($startpos,$endpos))}

variable:
| b=base_variable_with_function_calls l=list(variable_property) {List.fold_left (fun x f->f x) b (List.concat l)}

variable_property:
| ARROW p=object_property l=method_or_not {(fun x->(`Objprop(x,p),($startpos,$endpos)))::l}

method_or_not:
| u=function_call_parameter_list l=list(array_method_dereference) {(fun x->(`Metcall(x,u),($startpos,$endpos)))::l}
| {[]}

array_method_dereference:
| LB o=dim_offset RB {fun x->(`Arrprop(x,o),($startpos,$endpos))}

variable_without_objects:
| v=reference_variable {v}
| l=simple_indirect_reference v=reference_variable {List.fold_left (fun x f->f x) v l}

static_member:
| n=class_name DBLCOLON p=variable_without_objects {(`Staticmet(n,p),($startpos,$endpos))}
| n=variable_class_name DBLCOLON p=variable_without_objects {(`Staticmet(n,p),($startpos,$endpos))}

variable_class_name:
| n=reference_variable {n}

base_variable_with_function_calls:
| b=base_variable {b}
| c=function_call l=list(array_method_dereference) {List.fold_left (fun x f->f x) c l}

base_variable:
(*| v=reference_variable {v}
| l=nonempty_list(simple_indirect_reference) v=reference_variable {List.fold_left (fun x f->f x) v l}*)
| v=variable_without_objects {v}
| n=static_member {n}

reference_variable:
| v=reference_variable LB o=dim_offset RB {(`Arrprop(v,o),($startpos,$endpos))}
| v=reference_variable LC e=expr RC {(`Arrprop(v,Some e),($startpos,$endpos))}
| c=compound_variable {c}

compound_variable:
| v=VARIABLE {(`Variable(v),($startpos,$endpos))}
| DOLLAR LC e=expr RC {(`Compound(e),($startpos,$endpos))}

dim_offset:
| e=expr? {e}

object_property:
| v=variable_name l=list(object_dim) {List.fold_left (fun x f->f x) v l}
| v=variable_without_objects {v}

object_dim:
| LB o=dim_offset RB {fun x->(`Arrprop(x,o),($startpos,$endpos))}
| LC e=expr RC {fun x->(`Arrprop(x, Some e),($startpos,$endpos))}

variable_name:
| p=LABEL {(`Property(p),($startpos,$endpos))}
| LC e=expr RC {(`Varproperty(e),($startpos,$endpos))}

simple_indirect_reference:
| DOLLAR {[fun x->(`Variablevar(x),($startpos,$endpos))]}
| l=simple_indirect_reference DOLLAR {(fun x->(`Variablevar(x),($startpos,$endpos)))::l}

assignment_list:
| separated_nonempty_list(COMMA, assignment_list_element) {}

assignment_list_element:
| variable {}
| LIST LP assignment_list RP {}
| {}

array_pair_list:
| x=array_pair COMMA l=array_pair_list {x::l}
| x=array_pair {[x]}
| {[]}

opt_refexpr:
| BAND v=variable {(`Reference(v),($startpos,$endpos))}
| e=expr {e}

array_pair:
| f=expr DBLARROW e=opt_refexpr {(Some f, e)}
| e=opt_refexpr {(None, e)}

internal_function:
| ISSET LP l=separated_nonempty_list(COMMA, variable) RP {`Isset(l)}
| EMPTY LP v=variable RP {`Isempty(v)}
| EMPTY LP expr_without_variable RP {failwith "Cannot use empty(expr), use expr===NULL"}
| INCLUDE e=expr {`Include(e)}
| INCLUDE_ONCE e=expr {`Includeo(e)}
| EVAL LP e=expr RP {`Eval(e)}
| REQUIRE e=expr {`Require(e)}
| REQUIRE_ONCE e=expr {`Requireo(e)}
