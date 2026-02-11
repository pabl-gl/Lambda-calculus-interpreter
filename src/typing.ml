open Types
open Terms
open Context

(* Resolve alias types deeply *)
let rec resolve_type ctx = function
  | TyName s ->
      resolve_type ctx (getTypeBinding ctx s)

  | TyArr (t1, t2) ->
      TyArr (resolve_type ctx t1, resolve_type ctx t2)

  | TyTupl tys ->
      TyTupl (List.map (resolve_type ctx) tys)

  | TyRecord fields ->
      TyRecord (List.map (fun (l,t) -> (l, resolve_type ctx t)) fields)

  | TyList ty ->
      TyList (resolve_type ctx ty)

  | t -> t


let rec typeof ctx tm =
  match tm with

  | TmTrue -> TyBool

  | TmFalse -> TyBool

  | TmIf (t1, t2, t3) ->
      let tyCond = resolve_type ctx (typeof ctx t1) in
      if tyCond <> TyBool then
        raise (Type_error "guard of conditional not a boolean");

      let tyT2 = resolve_type ctx (typeof ctx t2) in
      let tyT3 = resolve_type ctx (typeof ctx t3) in

      if tyT2 = tyT3 then tyT2
      else raise (Type_error "arms of conditional have different types")

  | TmZero -> TyNat

  | TmSucc t1 ->
      if resolve_type ctx (typeof ctx t1) = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

  | TmPred t1 ->
      if resolve_type ctx (typeof ctx t1) = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

  | TmIsZero t1 ->
      if resolve_type ctx (typeof ctx t1) = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

  | TmVar x ->
      (try getTypeBinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

  | TmAbs (x, tyT1, t2) ->
      let tyT1' = resolve_type ctx tyT1 in
      let ctx' = addTypeBinding ctx x tyT1' in
      let tyT2 = resolve_type ctx (typeof ctx' t2) in
      TyArr (tyT1', tyT2)

  | TmApp (t1, t2) ->
      let tyT1 = resolve_type ctx (typeof ctx t1) in
      let tyT2 = resolve_type ctx (typeof ctx t2) in
      (match tyT1 with
       | TyArr (tyT11, tyT12) ->
           if tyT2 = tyT11 then tyT12
           else raise (Type_error "parameter type mismatch")
       | _ -> raise (Type_error "arrow type expected"))

  | TmLetIn (x, t1, t2) ->
      let tyT1 = resolve_type ctx (typeof ctx t1) in
      let ctx' = addTypeBinding ctx x tyT1 in
      typeof ctx' t2

  | TmFix t1 ->
      let tyT1 = resolve_type ctx (typeof ctx t1) in
      (match tyT1 with
       | TyArr (tyT11, tyT12) ->
           if tyT11 = tyT12 then tyT12
           else raise (Type_error "result of body not compatible with domain")
       | _ -> raise (Type_error "arrow type expected"))

  | TmString _ -> TyString

  | TmConcat (t1, t2) ->
      let ty1 = resolve_type ctx (typeof ctx t1) in
      let ty2 = resolve_type ctx (typeof ctx t2) in
      if ty1 = TyString && ty2 = TyString then TyString
      else raise (Type_error "String concatenation expects two strings")

  | TmTupl list ->
      TyTupl (List.map (fun t -> resolve_type ctx (typeof ctx t)) list)

  | TmProj (t1, i) ->
      (match resolve_type ctx (typeof ctx t1) with
       | TyTupl tylist ->
           if i < 1 || i > List.length tylist then
             raise (Type_error ("There is no element " ^ string_of_int i ^ " in the tuple"))
           else List.nth tylist (i - 1)
       | _ -> raise (Type_error "argument of projection is not a tuple"))

  | TmRecord fields ->
      TyRecord (List.map (fun (l,t) -> (l, resolve_type ctx (typeof ctx t))) fields)

  | TmProjRecord (t, label) ->
      (match resolve_type ctx (typeof ctx t) with
       | TyRecord fields ->
           (match List.assoc_opt label fields with
            | Some ty -> ty
            | None -> raise (Type_error ("label "^label^" not in record")))
       | _ -> raise (Type_error "projection on non-record"))

  | TmNil ty ->
      TyList (resolve_type ctx ty)

  | TmCons (tyElem, h, t) ->
      let tyElem' = resolve_type ctx tyElem in
      let th = resolve_type ctx (typeof ctx h) in
      let tt = resolve_type ctx (typeof ctx t) in
      if th <> tyElem' then
        raise (Type_error "cons: head type mismatch");
      (match tt with
       | TyList tyT when tyT = tyElem' -> TyList tyElem'
       | TyList _ -> raise (Type_error "cons: tail list element type mismatch")
       | _ -> raise (Type_error "cons: tail must be a list"))

  | TmIsNil (tyElem, t) ->
      (match resolve_type ctx (typeof ctx t) with
       | TyList ty ->
           let ty' = resolve_type ctx ty in
           let tyElem' = resolve_type ctx tyElem in
           if ty' = tyElem' then TyBool
           else raise (Type_error
             ("isnil[" ^ string_of_ty tyElem' ^
              "] expects List[" ^ string_of_ty tyElem' ^
              "] but argument is List[" ^ string_of_ty ty' ^ "]"))
       | nonlist ->
           raise (Type_error
             ("isnil[" ^ string_of_ty (resolve_type ctx tyElem) ^
              "] expects a List type but got " ^ string_of_ty nonlist)))

  | TmHead (tyElem, t) ->
      (match resolve_type ctx (typeof ctx t) with
       | TyList ty ->
           let ty' = resolve_type ctx ty in
           let tyElem' = resolve_type ctx tyElem in
           if ty' = tyElem' then tyElem'
           else raise (Type_error
             ("head[" ^ string_of_ty tyElem' ^
              "] expects List[" ^ string_of_ty tyElem' ^
              "] but argument is List[" ^ string_of_ty ty' ^ "]"))
       | nonlist ->
           raise (Type_error
             ("head[" ^ string_of_ty (resolve_type ctx tyElem) ^
              "] expects a List type but got " ^ string_of_ty nonlist)))

  | TmTail (tyElem, t) ->
      (match resolve_type ctx (typeof ctx t) with
       | TyList ty ->
           let ty' = resolve_type ctx ty in
           let tyElem' = resolve_type ctx tyElem in
           if ty' = tyElem' then TyList tyElem'
           else raise (Type_error
             ("tail[" ^ string_of_ty tyElem' ^
              "] expects List[" ^ string_of_ty tyElem' ^
              "] but argument is List[" ^ string_of_ty ty' ^ "]"))
       | nonlist ->
           raise (Type_error
             ("tail[" ^ string_of_ty (resolve_type ctx tyElem) ^
              "] expects a List type but got " ^ string_of_ty nonlist)))

 | TmTag (label, t, tyVariant) ->
    let tyVariant' = resolve_type ctx tyVariant in
    (match tyVariant' with
     | TyVariant branches ->
         (match List.assoc_opt label branches with
          | None ->
              raise (Type_error ("Tag '" ^ label ^ "' not present in variant type"))
          | Some tyExpected ->
              let tyActual = resolve_type ctx (typeof ctx t) in
              if tyActual = tyExpected then
                tyVariant'
              else
                raise (Type_error
                  ("Type mismatch in tag '" ^ label ^
                   "': expected " ^ string_of_ty tyExpected ^
                   " but got " ^ string_of_ty tyActual)))
     | _ ->
         raise (Type_error "tag annotation must be a variant type"))

  | TmCase (t, branches) ->
    let tyT = resolve_type ctx (typeof ctx t) in
    (match tyT with
     | TyVariant branchTypes ->
         let check_branch (label, x, body) =
           match List.assoc_opt label branchTypes with
           | None ->
               raise (Type_error ("case: label '" ^ label ^ "' not in variant type"))
           | Some tyField ->
               let ctx' = addTypeBinding ctx x tyField in
               typeof ctx' body
         in

         let resultTypes = List.map check_branch branches in
         let first = List.hd resultTypes in
         List.iter
           (fun ty -> if ty <> first then
              raise (Type_error "case: result types differ"))
           (List.tl resultTypes);
         first

   | _ -> raise (Type_error "case applied to non-variant type"))
