type t =
    Line of string
  | Block of t list
  | Inline of t list
  | Pos of Lexing.position * t list

let line_directive prev_file pos =
  let file = pos.Lexing.pos_fname in
  let directive =
    match prev_file with
        Some s when s = file ->
          Printf.sprintf "# %i\n"
            pos.Lexing.pos_lnum
      | _ ->
          Printf.sprintf "# %i %S\n"
            pos.Lexing.pos_lnum
            pos.Lexing.pos_fname
  in
  let indentation = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  directive, indentation

let to_buffer ?(offset = 0) ?(indent = 2) buf l =
  let prev_file = ref None in
  let rec print n = function
      Block l -> List.iter (print (n + indent)) l
    | Inline l -> List.iter (print n) l
    | Line s ->
	for i = 1 to n do
	  Buffer.add_char buf ' '
	done;
	Buffer.add_string buf s;
	Buffer.add_char buf '\n'
    | Pos (pos, l) ->
        let s, n = line_directive !prev_file pos in
        Buffer.add_string buf s;
        prev_file := Some pos.Lexing.pos_fname;
        List.iter (print n) l
  in
  List.iter (print offset) l

let to_string ?offset ?indent l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.contents buf

let to_channel ?offset ?indent oc l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.output_buffer oc buf

let to_stdout ?offset ?indent l =
  to_channel ?offset ?indent stdout l
