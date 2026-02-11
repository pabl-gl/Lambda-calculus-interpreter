open Types
open Terms
open Context

exception NoRuleApplies

let rec eval1 ctx tm = match tm with
    TmIf (TmTrue, t2, _) ->
      t2

  | TmIf (TmFalse, _, t3) ->
      t3

  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

  | TmPred TmZero ->
      TmZero

  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

  | TmIsZero TmZero ->
      TmTrue

  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

  | TmLetIn (x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

  | TmVar s ->
      getTermBinding ctx s

  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmConcat (v1, t2')

  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat (t1', t2)

  | TmProj (TmTupl list, i) when isval (TmTupl list) ->
      if i < 1 || i > List.length list then
        raise NoRuleApplies
      else
        List.nth list (i - 1)

  | TmProj (t1, i) ->
      let t1' = eval1 ctx t1 in
      TmProj (t1', i)

  | TmTupl list ->
      let rec eval_fields = function
          [] -> raise NoRuleApplies
        | t::rest when isval t ->
            let rest' = eval_fields rest in
            t :: rest'
        | t::rest ->
            let t' = eval1 ctx t in
            t' :: rest
      in
      TmTupl (eval_fields list)


  | TmRecord fields when List.for_all (fun (_,v) -> isval v) fields ->
      raise NoRuleApplies


  | TmRecord fields ->
      let rec eval_fields = function
        | [] -> raise NoRuleApplies
        | (l, v) :: rest when isval v ->
            let rest' =
              try eval_fields rest with NoRuleApplies -> rest
            in
            (l, v) :: rest'
        | (l, t) :: rest ->
            let t' = eval1 ctx t in
            (l, t') :: rest
      in
      TmRecord (eval_fields fields)


  | TmProjRecord (TmRecord fields, label) ->
      (match List.assoc_opt label fields with
       | Some v when isval v -> v
       | Some v -> v   (* si quieres permitir que el campo no esté completamente evaluado *)
       | None -> raise NoRuleApplies)

  (* si el término no es aún un record, lo evaluamos *)
  | TmProjRecord (t, label) ->
      let t' = eval1 ctx t in
      TmProjRecord (t', label)


  | TmIsNil (_, TmNil _) ->
        TmTrue

  | TmIsNil (_, TmCons (_, _, _)) ->
        TmFalse

  | TmIsNil (ty, t) ->
        let t' = eval1 ctx t in
        TmIsNil (ty, t')

  | TmHead (_, TmNil _) ->
        raise (Failure "runtime error: head on empty list")

  | TmHead (_, TmCons (_, h, _)) ->
        h

  | TmHead (ty, t) ->
        let t' = eval1 ctx t in
        TmHead (ty, t')

  | TmTail (_, TmNil _) ->
        raise (Failure "runtime error: tail on empty list")

  | TmTail (_, TmCons (_, _, t)) ->
        t

  | TmTail (ty, t) ->
        let t' = eval1 ctx t in
        TmTail (ty, t')


  | TmCons (ty, h, t) when isval h ->
        let t' = eval1 ctx t in
        TmCons (ty, h, t')

  | TmCons (ty, h, t) ->
        let h' = eval1 ctx h in
        TmCons (ty, h', t)

  (* E-TAG: evalúa el payload *)
  | TmTag (label, t, ty) ->
    let t' = eval1 ctx t in
    TmTag(label, t', ty)

(* E-CASE: evalúa el discriminante *)
  | TmCase (t, branches) when not (isval t) ->
    let t' = eval1 ctx t in
    TmCase(t', branches)

(* E-CASE-VALUE: aplica la rama correcta *)
  | TmCase (TmTag(label, v, _), branches) when isval v ->
    (match List.find_opt (fun (l, _, _) -> l = label) branches with
     | None ->
         raise NoRuleApplies
     | Some (_, x, body) ->
         subst x v body)

  | _ ->
      raise NoRuleApplies


let apply_ctx ctx tm =
  List.fold_left
    (fun t x -> subst x (getTermBinding ctx x) t)
    tm
    (free_vars tm)

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
