type loc = Loc.t

type rex_def = (loc * string) * Regexp.ast

type case = Regexp.ast * (loc * string) option * (loc * string)

type body =
    Match of loc * string * case list
  | Function of loc * case list

type let_def = (loc * string) * string list * body

type item =
    Ocaml_item of loc * string
  | Rex_def of loc * rex_def
  | Let_defs of loc * bool * let_def list
  | Match_item of loc * string * case list
  | Function_item of loc * case list

type ast = item list
