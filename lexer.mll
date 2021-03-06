{
open Printf
open Lexing

open Parser

type env = {
  mutable line_start : bool;
  buf : Buffer.t;
  lexbuf : Lexing.lexbuf;
}

let pos1 lexbuf = lexbuf.lex_start_p
let pos2 lexbuf = lexbuf.lex_curr_p
let loc lexbuf = (pos1 lexbuf, pos2 lexbuf)

let lexer_error lexbuf descr =
  Loc.error (loc lexbuf) descr

let new_file lb name =
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = name }

let count_new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <-
    { p with
	pos_lnum = p.pos_lnum + n;
	pos_bol = p.pos_cnum
    }

let set_lnum lb opt_file lnum =
  let p = lb.lex_curr_p in
  let cnum = p.pos_cnum in
  let fname =
    match opt_file with
	None -> p.pos_fname
      | Some file -> file
  in
  lb.lex_curr_p <-
    { pos_fname = fname;
      pos_bol = cnum;
      pos_cnum = cnum;
      pos_lnum = lnum }

let directive e n opt_filename =
  set_lnum e.lexbuf opt_filename n

let read_hexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'A'..'F' -> Char.code c - 55
    | 'a'..'z' -> Char.code c - 87
    | _ -> invalid_arg "read_hexdigit"

let read_hex2 c1 c2 =
  Char.chr (read_hexdigit c1 * 16 + read_hexdigit c2)

let new_line env =
  env.line_start <- true;
  count_new_lines env.lexbuf 1

let clear env = Buffer.clear env.buf

let add env =
  env.line_start <- false;
  Buffer.add_string env.buf (Lexing.lexeme env.lexbuf)

let add_string env s =
  env.line_start <- false;
  Buffer.add_string env.buf s

let add_char env c =
  env.line_start <- false;
  Buffer.add_char env.buf c

let get env = Buffer.contents env.buf
}

(* standard character classes *)
let upper = ['A'-'Z']
let lower = ['a'-'z']
let digit = ['0'-'9']

(* iso-8859-1 upper and lower characters used for ocaml identifiers *)
let o_upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let o_lower = ['a'-'z' '\223'-'\246' '\248'-'\255']
let o_identchar = o_upper | o_lower | digit | ['_' '\'']

(*
  Identifiers: lident is used for identifiers outside of raw OCaml code
  and is a subset of o_ident
*)
let identchar = upper | lower | digit | [ '_' '\'' ]
let lident = (lower | '_' identchar) identchar*
let o_ident = (o_lower | '_' o_identchar | o_upper) o_identchar*


let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let oct = ['0'-'7']
let bin = ['0'-'1']

let operator_char =
  [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let infix_symbol =
  ['=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' '$' '%'] operator_char*
let prefix_symbol =
    '!' operator_char*
  | ['?' '~'] operator_char+

let operator = infix_symbol | prefix_symbol

let punct_keyword =
  "!=" | "#" | "&" | "&&" | (*"\'" |*) "(" | ")" | "*" | "+" | "," | "-"
  | "-." | "->" | "." | ".." | ":" | "::" | ":=" | ":>" | ";" | ";;" | "<"
  | "<-" | "=" | ">" | ">]" | ">}" | "?" | "??" | "[" | "[<" | "[>" | "[|"
  | "]" | "_" | "`" | (*"{" |*) "{<" | "|" | "|]" | (*"}" |*) "~"

let blank = [ ' ' '\t' ]

rule token e = parse
  | blank* "#" blank* (digit+ as num)
        {
	  if e.line_start then (
	    add e;
            let opt_filename = finish_directive e lexbuf in
	    directive e (int_of_string num) opt_filename;
            token e lexbuf;
	  )
	  else
	    lexer_error lexbuf "Syntax error: unexpected '#'"
	}

  | blank+             { token e lexbuf }
  | '\r'? '\n'         { new_line e; token e lexbuf }
  | eof                { EOF }

  | "let"              { e.line_start <- false; LET }
  | "rec"              { e.line_start <- false; REC }
  | "and"              { e.line_start <- false; AND }
  | "rex"              { e.line_start <- false; REX }
  | "="                { e.line_start <- false; EQ }
  | "function"         { e.line_start <- false; FUNCTION }
  | "match"            { e.line_start <- false; MATCH }
  | "with"             { e.line_start <- false; WITH }
  | "|"                { e.line_start <- false; BAR }
  | "->"               { e.line_start <- false; ARROW }
  | "when"             { e.line_start <- false; WHEN }

  | lident as s        { e.line_start <- false; LIDENT s }

  (* Keywords specific to regexps *)
  | "%"                { e.line_start <- false; PERCENT }
  | "as"               { e.line_start <- false; AS }
  | ":"                { e.line_start <- false; COLON }
  | ":="               { e.line_start <- false; COLONEQ }
  | "*"                { e.line_start <- false; STAR }
  | "+"                { e.line_start <- false; PLUS }
  | "?"                { e.line_start <- false; QUESTION }
  | "~"                { e.line_start <- false; TILDE }
  | "["                { e.line_start <- false; LBR }
  | "]"                { e.line_start <- false; RBR }
  | "-"                { e.line_start <- false; DASH }
  | "^"                { e.line_start <- false; CARET }
  | digit+ as s        { INT (int_of_string s) }
  | '\"'               { eval_string e lexbuf; STRING (get e) }
  | '\''               { CHAR (eval_char e lexbuf) }
  | "Lazy"             { e.line_start <- false; LAZY }
  | "Possessive"       { e.line_start <- false; POSSESSIVE }
  | "_"                { e.line_start <- false; UNDERSCORE }
  | "<"                { e.line_start <- false; LT }
  | ">"                { e.line_start <- false; GT }
  | "Not"              { e.line_start <- false; NOT }

  (* Block of OCaml code *)
  | "{"                { e.line_start <- false;
                         clear e;
                         ocaml e 1 lexbuf;
                         OCAML (get e) }

and finish_directive e = parse
  | blank* '"'         { eval_string e lexbuf;
                         let s = get e in
                         finish_line e lexbuf;
                         Some s }
  | ""                 { finish_line e lexbuf;
                         None }

and finish_line e = parse
  | blank* '\r'? '\n'  { new_line e }
  | blank* eof         { () }
  | ""                 { lexer_error lexbuf "Invalid location directive" }

and ocaml e n = parse
  | "}"                { if n = 1 then
                           ()
                         else (
                           add e;
                           ocaml e (n-1) lexbuf
                         )
                       }

  | "{"                { add e;
                         ocaml e (n+1) lexbuf
                       }

  | blank* "#" blank* (digit+ as num)
        { add e;
	  if e.line_start then (
	    let opt_filename = finish_directive e lexbuf in
	    directive e (int_of_string num) opt_filename;
            (match opt_filename with
               | None -> add_string e "\n"
               | Some s -> add_string e (sprintf " %S\n" s));
            ocaml e n lexbuf;
	  )
	}

  | '\r'? '\n'
  | '\\' ('\r'? '\n')  { add e;
	                 new_line e }
  | "'\n'"
  | "'\r\n'"           { add e;
	                 new_line e;
                         e.line_start <- false }

  | blank+
  | o_ident

  | operator
  | punct_keyword

  | "'" ([^ '\'' '\\' '\n']
         | '\\' (_ | digit digit digit | 'x' hex hex)) "'"

  | '-'? ( digit (digit | '_')*
         | ("0x"| "0X") hex (hex | '_')*
	 | ("0o"| "0O") oct (oct | '_')*
	 | ("0b"| "0B") bin (bin | '_')* )

  | '-'? digit (digit | '_')* ('.' (digit | '_')* )?
      (['e' 'E'] ['+' '-']? digit (digit | '_')* )?

                       { add e }

  | "(*"               { add e;
	                 comment e 1 lexbuf }

  | '"'                { add e;
	                 string e lexbuf }

  | _                  { add e }

and comment e n = parse
  | "*)"
                       { add e;
	                 if n > 1 then
	                   comment e (n-1) lexbuf
                       }

  | "(*"               { add e;
                         comment e (n+1) lexbuf }

  | '"'
                       { add e;
	                 string e lexbuf;
	                 comment e n lexbuf }

  | "'\n'"
  | "'\r\n'"           { add e;
	                 new_line e;
                         e.line_start <- false;
	                 comment e n lexbuf
                       }

  | "'" ([^ '\'' '\\']
         | '\\' (_ | digit digit digit | 'x' hex hex)) "'"
                       { add e;
                         comment e n lexbuf
                       }

  | '\r'? '\n'         { new_line e;
                         add e;
	                 comment e n lexbuf
                       }

  | [^'(' '*' '"' '\r' '\n']+
                       {
	                 (* tolerates unmatched single quotes in comments,
                            unlike the standard ocaml lexer *)
                         add e;
	                 comment e n lexbuf
                       }

  | _
                       { add e;
	                 comment e n lexbuf }

  | eof
      { lexer_error lexbuf "Unterminated comment reaching the end of file" }

and string e = parse
    '"'
      { add e }

  | "\\\\"
  | '\\' '"'
      { add e;
	string e lexbuf }

  | '\\' '\r'? '\n'
      {
	add e;
	new_line e;
	string e lexbuf
      }

  | '\r'? '\n'
      {
	add e;
        new_line e;
	string e lexbuf
      }

  | _ as c
      { add_char e c;
	string e lexbuf }

  | eof
      { }


and eval_string e = parse
    '"'
      {  }

  | '\\' (['\'' '\"' '\\'] as c)
      { add_char e c;
	eval_string e lexbuf }

  | '\\' '\r'? '\n' blank*
      { new_line e;
        eval_string e lexbuf }

  | '\r'? '\n'
      { new_line e;
        add e }

  | '\\' (digit digit digit as s)
      { add_char e (Char.chr (int_of_string s));
	eval_string e lexbuf }

  | '\\' 'x' (hex as c1) (hex as c2)
      { add_char e (read_hex2 c1 c2);
	eval_string e lexbuf }

  | '\\' 'b'
      { add_char e '\b';
	eval_string e lexbuf }

  | '\\' 'n'
      { add_char e '\n';
	eval_string e lexbuf }

  | '\\' 'r'
      { add_char e '\r';
	eval_string e lexbuf }

  | '\\' 't'
      { add_char e '\t';
	eval_string e lexbuf }

  | [^ '\"' '\\']+
      { add e;
	eval_string e lexbuf }

  | eof
      { lexer_error lexbuf "Unterminated string literal" }

and eval_char e = parse

  | '\\' (['\'' '\"' '\\'] as c) '\''
      { c }

  | '\r'? '\n' '\''
      { new_line e;
        e.line_start <- false;
        '\n' }

  | '\\' (digit digit digit as s) '\''
      { Char.chr (int_of_string s) }

  | '\\' 'x' (hex as c1) (hex as c2) '\''
      { read_hex2 c1 c2 }

  | '\\' 'b' '\''
      { '\b' }

  | '\\' 'n' '\''
      { '\n' }

  | '\\' 'r' '\''
      { '\r' }

  | '\\' 't' '\''
      { '\t' }

  | _ as c '\''
      { c }

  | eof
      { lexer_error lexbuf "Unterminated character literal" }

{
  let init file lexbuf =
    new_file lexbuf file;
    {
       line_start = true;
       buf = Buffer.create 200;
       lexbuf = lexbuf;
    }
}
