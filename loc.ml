open Lexing

type t = Lexing.position * Lexing.position

exception Error of t * string

let error loc s =
  raise (Error (loc, s))

let to_string (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)
