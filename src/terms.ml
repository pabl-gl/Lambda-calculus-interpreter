open Types

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term

  | TmLetIn of string * term * term
  | TmFix of term

  | TmString of string
  | TmConcat of term * term

  | TmTupl of term list
  | TmProj of term * int

  | TmRecord of (string * term) list
  | TmProjRecord of term * string

  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term

  | TmTag of string * term * ty
  | TmCase of term * (string * string * term) list


open Format

let rec pp_term fmt tm = match tm with
  | TmIf (t1, t2, t3) ->
      (*
        if t1
          then t2
        else  
          t3
      *)
      fprintf fmt "@[<v>if %a@;<0 2>@[<2>then %a@]@;<0 0>@[<v>else@;<0 2>@[<2>%a@]@]@]"
        pp_term t1 
        pp_term t2 
        pp_term t3
  
  | TmAbs (x, ty, t) ->
      (* @[<2> indenta 2 espacios si hay salto de línea *)
      fprintf fmt "@[<2>lambda %s:%s.@ %a@]" 
        x (string_of_ty ty) pp_term t
  
  | TmLetIn (x, t1, t2) ->
      fprintf fmt "@[<v>let %s = %a in@ %a@]" 
        x pp_term t1 pp_term t2

  | TmFix t ->
      fprintf fmt "fix %a" pp_app t

  | TmCase (t, branches) ->
      let pp_branch fmt (label, x, body) =
        fprintf fmt "@[<2><%s=%s> =>@ %a@]" label x pp_term body
      in
      (* pp_print_list imprime lista con separadores *)
      fprintf fmt "@[<v>case %a of@ | %a@]" 
        pp_term t 
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ | ") pp_branch) branches

  (* Si no es una termino, pasamos al siguiente nivel de precedencia *)
  | _ -> pp_app fmt tm

and pp_app fmt tm = match tm with
  | TmApp (t1, t2) ->
      fprintf fmt "@[<2>%a@ %a@]" pp_app t1 pp_atom t2
  
  | TmSucc t ->
      (* Función auxiliar para contar sucesores recursivamente *)
      let rec count_succs n t' = match t' with
        | TmZero -> Some n           
        | TmSucc s -> count_succs (n + 1) s 
        | _ -> None                
      in
      (match count_succs 1 t with
       | Some n -> fprintf fmt "%d" n      
       | None -> fprintf fmt "succ %a" pp_atom t) 

  | TmPred t -> fprintf fmt "pred %a" pp_atom t
  | TmIsZero t -> fprintf fmt "iszero %a" pp_atom t
  
  | TmConcat (t1, t2) ->
      fprintf fmt "%a ^ %a" pp_atom t1 pp_atom t2

  | TmCons (ty, h, t) ->
      fprintf fmt "@[cons[%s]@ %a@ %a@]" (string_of_ty ty) pp_atom h pp_atom t
  
  | TmIsNil (ty, t) -> fprintf fmt "isnil[%s] %a" (string_of_ty ty) pp_atom t
  | TmHead (ty, t) -> fprintf fmt "head[%s] %a" (string_of_ty ty) pp_atom t
  | TmTail (ty, t) -> fprintf fmt "tail[%s] %a" (string_of_ty ty) pp_atom t

  (* Si no es aplicación, pasamos a nivel átomo *)
  | _ -> pp_atom fmt tm
and pp_atom fmt tm = match tm with
  | TmTrue -> fprintf fmt "true"
  | TmFalse -> fprintf fmt "false"
  | TmZero -> fprintf fmt "0"
  | TmString s -> fprintf fmt "\"%s\"" s
  | TmVar x -> fprintf fmt "%s" x
  | TmNil ty -> fprintf fmt "nil[%s]" (string_of_ty ty)

  | TmTupl terms ->
      let pp_sep fmt () = fprintf fmt ",@ " in
      fprintf fmt "{@[<hov>%a@]}" (pp_print_list ~pp_sep pp_term) terms

  | TmRecord fields ->
      let pp_field fmt (l, t) = fprintf fmt "%s = %a" l pp_term t in
      let pp_sep fmt () = fprintf fmt ",@ " in
      fprintf fmt "{@[<hov>%a@]}" (pp_print_list ~pp_sep pp_field) fields

  | TmProj (t, i) ->
      fprintf fmt "%a.%d" pp_atom t i

  | TmProjRecord (t, l) ->
      fprintf fmt "%a.%s" pp_atom t l

  | TmTag (l, t, ty) ->
      fprintf fmt "<%s=%a> as %s" l pp_term t (string_of_ty ty)

  (* Si tenemos que imprimir algo complejo añadimos paréntesis. *)
  | _ -> fprintf fmt "(%a)" pp_term tm

let string_of_term t =
  let buff = Buffer.create 100 in
  let fmt = formatter_of_buffer buff in
  (* Ajustamos margen para forzar saltos de línea si es muy largo *)
  pp_set_margin fmt 80; 
  pp_term fmt t;
  pp_print_flush fmt ();
  Buffer.contents buff
;;


let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmTupl list ->
      List.fold_left (fun acc t -> lunion acc (free_vars t)) [] list
  | TmProj (t, _) ->
      free_vars t
  | TmRecord fields ->
    List.fold_left (fun acc (_,t) -> lunion acc (free_vars t)) [] fields

  | TmProjRecord (t, _) ->
    free_vars t

  | TmNil _ ->[]

  | TmCons (_,h,t) -> lunion (free_vars h) (free_vars t)

  | TmIsNil (ty,t) -> free_vars t

  | TmHead (ty,t) -> free_vars t

  | TmTail (ty,t) -> free_vars t

  | TmTag (_, t, _) ->
    free_vars t

  | TmCase (t, branches) ->
    let fv_t = free_vars t in
    let fv_branches =
      List.fold_left
        (fun acc (_, x, body) ->
           lunion acc (ldif (free_vars body) [x]))
        [] branches
    in
    lunion fv_t fv_branches




let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue

  | TmFalse ->
      TmFalse

  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)

  | TmZero ->
      TmZero

  | TmSucc t ->
      TmSucc (subst x s t)

  | TmPred t ->
      TmPred (subst x s t)

  | TmIsZero t ->
      TmIsZero (subst x s t)

  | TmVar y ->
      if y = x then s else tm

  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else
        let fvs = free_vars s in
        if not (List.mem y fvs)
        then TmAbs (y, tyY, subst x s t)
        else
          let z = fresh_name y (free_vars t @ fvs) in
          TmAbs (z, tyY, subst x s (subst y (TmVar z) t))

  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)

  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else
        let fvs = free_vars s in
        if not (List.mem y fvs)
        then TmLetIn (y, subst x s t1, subst x s t2)
        else
          let z = fresh_name y (free_vars t2 @ fvs) in
          TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))

  | TmFix t ->
      TmFix (subst x s t)

  | TmString _ ->
      tm

  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)

  | TmTupl list ->
      TmTupl (List.map (subst x s) list)

  | TmProj (t, i) ->
      TmProj (subst x s t, i)

  | TmRecord fields ->
    TmRecord (List.map (fun (l,t) -> (l, subst x s t)) fields)

  | TmProjRecord (t, l) ->
    TmProjRecord (subst x s t, l)


  | TmNil ty ->
      TmNil ty

  | TmCons (ty, h, t) ->
      TmCons (ty, subst x s h, subst x s t)

  | TmIsNil (ty, t) -> TmIsNil (ty, subst x s t)

  |  TmHead (ty, t) -> TmHead (ty, subst x s t)

  | TmTail (ty, t) -> TmTail (ty, subst x s t)

  | TmTag (label, t, ty) ->
    TmTag(label, subst x s t, ty)

  | TmCase (t, branches) ->
    let t' = subst x s t in
    let branches' =
      List.map
        (fun (label, y, body) ->
           if y = x then
             (label, y, body)
           else
             let fvs = free_vars s in
             if not (List.mem y fvs) then
               (label, y, subst x s body)
             else
               (* alpha-renaming seguro *)
               let z = fresh_name y (free_vars body @ fvs) in
               (label, z,
                subst x s (subst y (TmVar z) body)))
        branches
    in
    TmCase(t', branches')




let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | TmTupl list -> List.for_all isval list
  | TmRecord fields ->
      List.for_all (fun (_, v) -> isval v) fields

  | TmNil _ -> true
  | TmCons (_, h, t) -> isval h && isval t
  | TmTag (_, v, _) ->
      isval v
  | TmCase _ ->
      false

  | _ -> false
