open Printf

open Regexp
open Constants
open Types
open Indent

(* Regular expression to match against *)
type regexp_info = {
  re_loc : Loc.t;
  re_name : string; (* identifier of the compiled regexp *)
  re_num : int;
  re_args : regexp_args;
  re_source : regexp_source; (* string representation of the regexp *)
  re_groups :
    named_groups (* names of substrings *)
  * named_groups (* names of positions *);

  re_anchored : bool
}


(* Sets of strings with location *)

let compare_loc_string (_, s1) (_, s2) = String.compare s1 s2

module Names = Set.Make (
  struct
    type t = Loc.t * string
    let compare = compare_loc_string
  end
)

let add_new loc set x =
  if Names.mem x set then
    Messages.multiple_binding loc [(snd x)];
  Names.add x set

let add_if_needed loc set x = Names.add x set

let unloc l = List.map snd l

let check_same_ids loc e1 e2 =
  if not (Names.equal e1 e2) then
    let diff_elements =
      unloc
	(Names.elements
	   (Names.diff (Names.union e1 e2) (Names.inter e1 e2))) in
    Messages.unbalanced_bindings loc diff_elements

let check_different_ids loc e1 e2 =
  let inter = Names.inter e1 e2 in
  if not (Names.equal inter Names.empty) then
    Messages.multiple_binding loc (unloc (Names.elements inter))

let get_all_names re =
  let loc = re.re_loc in
  let get_names x =
    Named_groups.fold
      (fun group_name _ accu ->
	 add_new loc accu (loc, group_name))
      x
      Names.empty in

  let (groups, positions) = re.re_groups in
  let group_names = get_names groups
  and position_names = get_names positions in
  check_different_ids loc group_names position_names;
  Names.union group_names position_names

let list_all_names re =
  Names.elements (get_all_names re)

let has_prefix ~prefix s =
  String.length s >= String.length prefix &&
  String.sub s 0 (String.length prefix) = prefix

let match_suffix ~suffix s =
  let len = String.length s in
  let slen = String.length suffix in
  if len >= slen &&
    String.sub s (len - slen) slen = suffix then
      String.sub s 0 (len - slen)
  else
    invalid_arg "match_suffix"

let posix_regexps =
  List.map
    (fun (name, set) -> (name, Characters (dummy_loc, set)))
    Charset.Posix.all


module Predefined_regexps =
struct
  let loc = dummy_loc
  let set x = Characters (loc, x)
  let cset s = set (Charset.of_string s)
  let opt x = Repetition (loc, (Option, true), x)
  let plus x = Repetition (loc, (Plus, true), x)
  let star x = Repetition (loc, (Star, true), x)
  let lr f l =
    match List.rev l with
	[] -> Epsilon loc
      | last :: rl ->
	  List.fold_left
	    (fun accu x -> f x accu)
	    last
	    rl
  let seq l = lr (fun x y -> Sequence (loc, x, y)) l
  let alt l = lr (fun x y -> Alternative (loc, x, y, S.empty, S.empty)) l
  let explode s =
    let l = ref [] in
    for i = String.length s - 1 downto 0 do
      l := set (Charset.singleton s.[i]) :: !l
    done;
    !l
  let string s = seq (explode s)

  open Charset.Posix

  let opt_sign = opt (cset "-+")
  let d = set digit

(*
RE int = ["-+"]? ( "0" ( ["xX"] xdigit+
		       | ["oO"] ['0'-'7']+
		       | ["bB"] ["01"]+ )
                 | digit+ )
*)

  let int =
    seq [
      opt_sign;
      (alt [
	 seq [
	   (cset "0");
	   (alt [
	      seq [ (cset "xX"); (plus (set xdigit)) ];
	      seq [ (cset "oO"); (plus (set (Charset.range '0' '7'))) ];
	      seq [ (cset "bB"); (plus (cset "01")) ];
	    ]);
	 ];
	 plus d;
       ])
    ]

(*
RE float =
  ["-+"]?
     ( ( digit+ ("." digit* )? | "." digit+ ) (["eE"] ["+-"]? digit+ )?
       | "nan"~
       | "inf"~ )
*)

  let float =
    seq [
      opt_sign;
      alt [
	seq [
	  alt [
	    seq [
              plus d;
	      opt (seq [ cset "."; star d ])
	    ];
	    seq [ cset "."; plus d ];
	  ];
	  opt
	    (seq [
	       cset "eE";
	       opt (cset "+-");
	       plus d;
	     ])
	];
	nocase (string "nan");
	nocase (string "inf");
      ]
    ]

  let all =
    [ "int", int;
      "float", float ]
end

module Env = Map.Make (
  struct
    type t = string
    let compare = String.compare
  end
)

let add_list env l =
  List.fold_left
    (fun env (key, data) -> Env.add env key data)
    env l

let init_env () =
  let env = Env.empty in
  let env = add_list env posix_regexps in
  let env = add_list env Predefined_regexps.all in
  env

let add_compiled_regexp
    env anchored loc name num re_args re_source named_groups =
  Env.add env
    name
    { re_loc = loc;
      re_num = num;
      re_name = name;
      re_source = re_source;
      re_args = re_args;
      re_groups = named_groups;
      re_anchored = anchored }

let find_named_regexp env loc name =
  try Env.find named_regexps name
  with Not_found ->
    Messages.failure loc
      ("Unbound regular expression " ^ name)


let output_item env = function
  | Ocaml_item (loc, s) -> env, [ Pos (fst loc, Line s) ]
  | Rex_def (loc, rex_def) ->
      let env, code = output_rex_def rex_def in
      env, [ Pos (fst loc, code) ]
  | Let_defs (loc, is_rec, l) ->
      let code = output_let_defs is_rec l in
      env, [ Pos (fst loc, code) ]
  | Match_item (loc, name, cases) ->
      let code = output_match loc name cases in
      env, [ Pos (fst loc, code) ]
  | Function_item (loc, cases) ->
      let code = output_function loc name cases in
      env, [ Pos (fst loc, code) ]

let output_ast l =
  let env = init_env () in
  let env, code_l =
    List.fold_left (
      fun (env, code_l) x ->
        let env, code = output_item env x in
        env, (Inline code :: code_l)
    ) (Env.empty, []) l
  in
  List.rev code_l
