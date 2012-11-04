%{
  open Printf
  open Types

  let getloc n1 n2 = (Parsing.rhs_start_pos n1, Parsing.rhs_end_pos n2)
%}

%token EOF LET REC AND REX EQ FUNCTION MATCH WITH
%token BAR ARROW WHEN PERCENT AS COLON
%token COLONEQ STAR PLUS QUESTION TILDE LBR RBR DASH LAZY POSSESSIVE
%token UNDERSCORE LT GT NOT
%token <string> LIDENT STRING OCAML
%token <int> INT
%token <char> CHAR

%left AS
%left BAR

%start main
%type <Types.ast> main
%%

main:
| item main { $1 :: $2 }
| EOF { [] }
;

item:
| OCAML { Ocaml_item (getloc 1 1, $1) }

| LET REX LIDENT EQ regexp
    { Rex_def (getloc 1 5, ((getloc 3 3, $3), $5)) }

| LET LIDENT args EQ body and_defs
    { let def = ((getloc 2 2, $2), $3, $5) in
      Let_defs (getloc 1 6, false, def :: $6) }

| LET REC LIDENT args EQ body and_defs
    { let def = ((getloc 3 3, $3), $4, $6) in
      Let_defs (getloc 1 7, true, def :: $7) }

| MATCH LIDENT WITH cases
    { Match_item (getloc 1 4, $2, $4) }

| FUNCTION cases
    { Function_item (getloc 1 2, $2) }
;

args:
| LIDENT args { $1 :: $2 }
|             { [] }
/* TODO labeled arguments */
;

body:
| MATCH LIDENT WITH cases
    { Match (getloc 1 4, $2, $4) }

| FUNCTION cases
    { Function (getloc 1 2, $2) }
;

cases:
| regexp guard ARROW OCAML cases
    { ($1, $2, (getloc 4 4, $4)) :: $5 }
| BAR regexp guard ARROW OCAML cases
    { ($2, $3, (getloc 5 5, $5)) :: $6 }
|
    { [] }
;

guard:
| WHEN OCAML
    { Some (getloc 2 2, $2) }
|
    { None }
;

and_defs:
| AND LIDENT args EQ body and_defs
    { ((getloc 2 2, $2), $3, $5) :: $6 }
|   { [] }
;

regexp:
| regexp AS LIDENT opt_converter
| regexp BAR regexp
| regexp regexp
| regexp STAR
| regexp PLUS
| regexp QUESTION
| regexp TILDE
| regexp LBR range RBR
| regexp HASH regexp
| EXCL LIDENT
| AT OCAML
| LBR charset RBR
| STRING { Regexp.of_string (getloc 1 1) $1 }
| CHAR { Characters (getloc 1 1, Charset.singleton $1) }
| LIDENT
| PERCENT LIDENT
| LPAR regexp RPAR
;

opt_converter:
| COLON LIDENT
| COLONEQ OCAML
| EQ OCAML
|
;

charset:
| HAT charset { Charset.complement $2 }
| CHAR MINUS CHAR { Charset.range $1 $2 }
| CHAR { Charset.singleton $1 }
| STRING { Charset.of_string $1 }
| LIDENT { let loc = getloc 1 1 in
           Regexp_ast.as_charset loc "not a set of characters"
	     (find_named_regexp loc name)
         }
| charset charset { Charset.union $1 $2 }
;

range:
| INT           { let mini = int_of_string $1 in (mini, None) }
| INT PLUS      { (int_of_string $1, Some None), getloc 1 2 }
| INT MINUS INT { (int_of_string $1, Some (int_of_string $3)), getloc 1 3 }
;
