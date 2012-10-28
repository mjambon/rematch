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

and_defs:
| AND LIDENT args EQ body and_defs
    { ((getloc 2 2, $2), $3, $5) :: $6 }
|   { [] }

regexp:
| STRING { Regexp.of_string (getloc 1 1) $1 }
/* TODO more cases */
;
