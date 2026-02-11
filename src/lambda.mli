type ty = Types.ty
type term = Terms.term

type binding = Context.binding
type context = Context.context

type command =
  | Eval of term
  | Bind of string * term
  | TypeBind of string * ty
  | Quit
  | Clear

val emptyctx : context

val addTypeBinding : context -> string -> ty -> context
val addTermBinding : context -> string -> ty -> term -> context
val getTypeBinding : context -> string -> ty
val getTermBinding : context -> string -> term

val string_of_ty : ty -> string
exception Type_error of string
val typeof : context -> term -> ty

val string_of_term : term -> string
val eval : context -> term -> term
exception NoRuleApplies

val execute : context -> command -> context
