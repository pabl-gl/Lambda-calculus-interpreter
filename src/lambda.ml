include Types
include Terms
include Context
include Typing
include Eval

type command =
  | Eval of term
  | Bind of string * term
  | TypeBind of string * ty
  | Quit
  | Clear


let execute ctx = function
  | Eval tm ->
      let tyTm = resolve_type ctx (typeof ctx tm) in
      let tm' = eval ctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      ctx

  | Bind (s, tm) ->
      let tyTm = resolve_type ctx (typeof ctx tm) in
      let tm' = eval ctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      addTermBinding ctx s tyTm tm'

  | TypeBind (s, ty) ->
      let ty' = resolve_type ctx ty in
      print_endline ("type " ^ s ^ " = " ^ string_of_ty ty');
      addTypeBinding ctx s ty'

  | Clear ->
    print_string "\027[2J\027[H";
    flush stdout;
    ctx

  | Quit ->
      raise End_of_file
