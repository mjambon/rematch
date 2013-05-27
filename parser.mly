%{
  open Printf
  open Regexp (* Regexp.ast type *)
  open Types (* other types *)

  let getloc n1 n2 = (Parsing.rhs_start_pos n1, Parsing.rhs_end_pos n2)
%}

%token EOF LET REC AND REX EQ FUNCTION MATCH WITH
%token BAR ARROW WHEN PERCENT AS COLON
%token COLONEQ STAR PLUS QUESTION TILDE LCBR RCBR LSBR RSBR
%token DASH LAZY POSSESSIVE HASH EXCL AT LPAR RPAR CARET
%token UNDERSCORE LT GT NOT
%token <string> LIDENT STRING OCAML
%token <int> INT
%token <char> CHAR

%left AS
%left BAR
%nonassoc CONCAT
%nonassoc STAR PLUS QUESTION TILDE
%left LCBR
%left HASH
%nonassoc EXCL AT LSBR STRING CHAR LIDENT PERCENT LPAR

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
| regexp AS LIDENT opt_converter { Bind (getloc 1 4, $1, $3, $4) }
| regexp BAR regexp              { alternative (getloc 1 3) $1 $3 }
| regexp regexp  %prec CONCAT    { Sequence (getloc 1 2, $1, $2) }
| regexp STAR          { Repetition (getloc 1 2, (Star, true), Closed $1) }
| regexp PLUS          { Repetition (getloc 1 2, (Plus, true), Closed $1) }
| regexp QUESTION      { Repetition (getloc 1 2, (Option, true), Closed $1) }
| regexp TILDE         { nocase $1 }
| regexp LCBR range RCBR
    { let r = $1 in
      let loc = getloc 1 4 in
      let rng, rng_loc = $3 in
      Repetition (loc, (Range rng, true), Closed r)
    }
| regexp HASH regexp
    { let r1 = $1 and r2 = $3 in
      let loc = getloc 1 3 in
      let msg = " term is not a set of characters" in
      let set1 = Regexp.as_charset loc ("left" ^ msg) r1 in
      let set2 = Regexp.as_charset loc ("right" ^ msg) r2 in
      Characters (loc, Charset.diff set1 set2)
    }
| EXCL LIDENT         { Backref (getloc 1 2, $2) }
| AT OCAML            { Variable (getloc 1 2, $2) }
| LSBR charset RSBR   { Characters (getloc 1 3, $2) }
| STRING              { Regexp.of_string (getloc 1 1) $1 }
| CHAR                { Characters (getloc 1 1, Charset.singleton $1) }
| LIDENT              { let loc = getloc 1 1 in
                        Regexp.as_charset loc "not a set of characters"
	                  (Emit.find_named_regexp loc name)
                      }
| PERCENT LIDENT      { Bind_pos (getloc 1 2, name) }
| LPAR regexp RPAR    { $2 }
;

opt_converter:
| COLON LIDENT
    { let conv =
        match s with
	    "int" -> `Int
          | "float" -> `Float
          | "option" -> `Option
          | s -> Messages.invalid_converter loc s
      in
      Some conv
    }
| COLONEQ OCAML { Some (`Custom $2) }
| EQ OCAML      { Some (`Value $2) }
|               { None }
;

charset:
| CARET charset1       { Charset.complement $2 }
| charset1             { $1 }
;

charset1:
| CHAR DASH CHAR    { Charset.range $1 $3 }
| CHAR              { Charset.singleton $1 }
| STRING            { Charset.of_string $1 }
| LIDENT            { let loc = getloc 1 1 in
                      Regexp_ast.as_charset loc "not a set of characters"
	                (Match.find_named_regexp loc name)
                    }
| charset1 charset1  %prec CONCAT
                    { Charset.union $1 $2 }
;

range:
| INT           { let mini = int_of_string $1 in (mini, None) }
| INT PLUS      { (int_of_string $1, Some None), getloc 1 2 }
| INT DASH INT  { (int_of_string $1, Some (int_of_string $3)), getloc 1 3 }
;
