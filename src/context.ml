open Types
open Terms

type binding =
  | TypeDef of ty
  | TermBinding of (ty * term)

type context = (string * binding) list

let emptyctx : context = []

let addTypeBinding ctx x bind =
  (x, TypeDef bind) :: ctx

let addTermBinding ctx x ty tm =
  (x, TermBinding (ty, tm)) :: ctx

let getTypeBinding ctx x =
  match List.assoc x ctx with
    | TypeDef ty -> ty
    | TermBinding (ty, _) -> ty

let getTermBinding ctx x =
  match List.assoc x ctx with
    | TermBinding (_, tm) -> tm
    | _ -> raise Not_found
