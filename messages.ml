open Printf

let warning loc s =
  let label = if !Sys.interactive then "" else "Warning: " in
  eprintf "%s:%s\n%s\n" (Loc.to_string loc) label s

let failure loc s = Loc.error loc s

let list = function
    [] -> ""
  | [s] -> s
  | l ->
      let l' = List.rev l in
      String.concat ", " (List.rev (List.tl l')) ^ " and " ^ List.hd l'

let invalid_backref loc name =
  failure loc
    (sprintf "Invalid backreference %s" name)

let unbalanced_bindings loc l =
  failure loc
    (sprintf "Variable%s %s must occur on both sides of this | pattern"
       (if List.length l > 1 then "s" else "")
       (list l))

let multiple_binding loc l =
  let s, are =
    if List.length l > 1 then "s", "are"
    else "", "is" in
  failure loc
    (sprintf "Variable%s %s %s bound several times in this matching"
       s (list l) are)

let invalid_range loc =
  failure loc "Invalid range"

let invalid_pattern loc =
  failure loc "Invalid pattern"

let invalid_lookbehind loc kind adjective =
  failure loc
    (sprintf "%s are disabled in %slookbehind assertions" kind adjective)

let not_visible loc who where =
  let s, are =
    if List.length who > 1 then "s", "are"
    else "", "is" in
  warning loc
    (sprintf "identifier%s %s %s not visible \
              out of this %s"
       s (list who) are where)

let invalid_converter loc name =
  failure loc
    (sprintf "%s is not a valid converter" name)

let reserved_identifier loc prefix name =
  failure loc
    (sprintf "%s is a reserved identifier: use another prefix than %s"
       name prefix)
