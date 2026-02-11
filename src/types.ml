type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTupl of ty list
  | TyName of string
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyVariant of (string * ty) list



let string_of_ty ty =
  let rec rec_sot prec t =
    match t with
    | TyBool ->
        "Bool"

    | TyNat ->
        "Nat"

    | TyArr (ty1, ty2) ->
        let str_ty = (rec_sot 1 ty1) ^ " -> " ^ (rec_sot 0 ty2) in
        if prec > 0 then "(" ^ str_ty ^ ")" else str_ty

    | TyString ->
        "String"

    | TyTupl tylist ->
        let rec print_tys = function
          | [] -> ""
          | [t] -> rec_sot 0 t
          | t :: rest -> rec_sot 0 t ^ " * " ^ print_tys rest
        in
        "{" ^ print_tys tylist ^ "}"

    | TyName s ->
        s

    | TyRecord fields ->
        let rec print_fields = function
          | [] -> ""
          | [ (l, ty) ] -> l ^ " : " ^ rec_sot 0 ty
          | (l, ty) :: rest ->
              l ^ " : " ^ rec_sot 0 ty ^ ", " ^ print_fields rest
        in
        "{" ^ print_fields fields ^ "}"

    | TyList ty ->
        "List[" ^ rec_sot 0 ty ^ "]"

    | TyVariant tagList ->
        let rec print_tags = function
          | [] -> ""
          | [ (l, ty) ] -> l ^ " : " ^ rec_sot 0 ty
          | (l, ty) :: rest ->
              l ^ " : " ^ rec_sot 0 ty ^ ", " ^ print_tags rest
        in
        "<" ^ print_tags tagList ^ ">"


  in
  rec_sot 0 ty



exception Type_error of string
