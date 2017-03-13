%{

(** Converted from zend_language_parser.y for PHP 5.4 **)

%}

%token EOF
%token DOCTYPE
%token TAG_OPEN
%token TAG_SLASH_OPEN
%token TAG_CLOSE
%token EQUAL
%token SLASH

%token <string> STRING
%start <Htmlast.t> start

%%

start:
| doctype? l=list(tag) EOF {l}

tag:
| TAG_OPEN t=STRING a=list(attribute) SLASH TAG_CLOSE {(`Tag(t, a, []),($startpos,$endpos))}
| TAG_OPEN t=STRING a=list(attribute) TAG_CLOSE l=list(tag) TAG_SLASH_OPEN q=STRING TAG_CLOSE
   {(`Tag((if t=q then String.lowercase t else failwith "Invalid HTML"), a, l),($startpos,$endpos))}
| t=STRING {(`Text(t),($startpos,$endpos))}

doctype:
| DOCTYPE l=list(attribute) TAG_CLOSE {}

attribute:
| n=STRING v=attval {((n,v),($startpos,$endpos))}

attval:
| EQUAL s=STRING {s}
| {""}