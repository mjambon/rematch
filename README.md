Rematch
=======

Mikmatch is an extension of OCaml's pattern matching that integrates
regular expressions and makes it easy to get them right.

The Rematch project as currently envisioned will be in a Camlp4-free
reimplementation of the most useful features of Mikmatch.

Goals
-----

* Faster build times: Camlp4 preprocessing typically doubles build
  times of OCaml projects, which is not acceptable.

* Not depending on Camlp4 or Camlp5: they are complicated pieces of
  software but lack strong and unified developer support.

* Replacing Mikmatch at a reasonable cost: that means keeping only the
  most commonly used features.

Design ideas
------------

* Fast, standalone program that converts source code to OCaml code.
  Generated code shall be reviewable by humans.

* Embedding within OCaml programs will be possible with cppo and the
  `#ext` ... `#endext` directives.

* Keeping the same syntax for regexps.

* Support for only PCRE, dropping support for Str.

* Support only string matching, no more matching of other concrete
  types containing strings. For example, writing
  `match l with RE "Hello" :: _ -> ...` for testing the first element
  of a list of strings will no longer be possible.

* Unfortunately no more toplevel support. Not sure what to do in order
  to facilitate testing.

Example:

```ocaml
(* Some OCaml code *)

#ext rematch

(* Some Rematch code *)

(* "let rex" is the syntax for introducing a regexp definition.
   It is "RE" in Mikmatch and just "let" in Ocamllex.
   It plays better with automatic OCaml indentation than "RE". *)
let rex date =
  (digit{4} as year : int)
  "-" (digit{2} as month : int)
  "-" (digit{2} as day : int)

(* Function definition.
   The "[/" and "/]" around the regexp are not necessary for now,
   but would be necessary if in the future we can match using
   normal OCaml patterns as supported by Mikmatch.
   The vertical bars are also not needed but make automatic
   indentation easier. (What about arrows?)
   The curly braces around the right-hand OCaml expression make
   it possible to not implement and maintain a strict OCaml parser.
*)
let f x = function
    [/ "[" date "]" blank* (_* as msg) eos /]  { (year, month, day, msg) }
  | [/ "" /]  { x }

(* Same as: let g = function ... *)
let g s =
  match s with
    ...
#endext

(* Simple match expression *)
let h x =
  ...
  #ext rematch
    match s with
       ...
  #endext
```
