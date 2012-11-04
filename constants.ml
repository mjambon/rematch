let debug_mode = ref false
let reserved_prefix = if !debug_mode then "_" else "__rematch_"
let uppercase_prefix = "C" ^ reserved_prefix
let typevar_prefix = "a" ^ reserved_prefix

let mod_runtime = "Rematch"
let exn_exit = "Rematch_exit"

let any_exn = reserved_prefix ^ "any_exn"
let any_target = reserved_prefix ^ "any_target"
let any_result = reserved_prefix ^ "any_result"

let expr_exit =
  <:expr< $uid: !mod_runtime$.$uid:exn_exit$ >>

let raise_exit loc =
  <:expr< raise $expr_exit loc$ >>

let patt_exit loc =
  <:patt< $uid: !mod_runtime$.$uid:exn_exit$ >>

let shared re_name = re_name ^ "shared"
let subgroups2 re_name = re_name ^ "subgroups2"
let shared_ovector re_name = re_name ^ "shared_ovector"

let regexp_prefix = reserved_prefix ^ "regexp_"
let view_prefix = reserved_prefix ^ "view_"

let new_regexp =
  let r = ref 0 in
  fun () -> incr r; (!r, regexp_prefix ^ string_of_int !r)

let new_view =
  let r = ref 0 in
  fun () -> incr r; (!r, view_prefix ^ string_of_int !r)

let new_target =
  let r = ref 0 in
  fun () -> incr r; reserved_prefix ^ "match_target_" ^ string_of_int !r

let new_subpatt =
  let r = ref 0 in
  fun () -> incr r; reserved_prefix ^ "subpatt_" ^ string_of_int !r

let new_var =
  let r = ref 0 in
  fun () -> incr r; reserved_prefix ^ "var_" ^ string_of_int !r

let new_type_var =
  let r = ref 0 in
  fun () -> incr r; typevar_prefix ^ string_of_int !r
