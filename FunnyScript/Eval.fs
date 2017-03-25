module FunnyScript.Eval

let rec force obj =
  match obj with
  | Lazy x -> x.Force() |> Option.bind force
  | _ -> Some obj

let private apply_ eval f arg =
  match f with
  | Func (BuiltinFunc f) -> f.Apply arg
  | Func (UserFunc f) -> lazy (f.Env |> Map.add (Name f.Def.Arg) arg |> eval f.Def.Body) |> Lazy |> Some
  | List list -> match arg with Int i -> Some list.[i] | _ -> None
  | _ -> None

let rec eval expr env =
  let apply = apply_ eval
  
  let forceEval expr env =
    env |> eval expr |> Option.bind force

  match expr with
  | Obj x -> Some x
  | Ref x -> env |> Map.tryFind (Name x) |> function Some x -> Some x | _ -> printfn "'%s' is not found." x; None
  | RefMember (expr, name) ->
    env |> forceEval expr |> Option.bind (function
      | Record r -> r |> Map.tryFind name
      | _ -> None)
  | Let (name, value, succ) ->
    env |> forceEval value |> Option.bind (fun value ->
      let env = env |> Map.add (Name name) value
      match value with Func (UserFunc f) -> f.Env <- env | _ -> ()  // to enable recursive call
      env |> eval succ)
  | Combine (expr1, expr2) ->
    env |> forceEval expr1 |> ignore
    env |> eval expr2
  | FuncDef def -> Func (UserFunc { Def = def; Env = env }) |> Some
  | Apply (f, arg) ->
    env |> forceEval f   |> Option.bind (fun f ->
    env |> forceEval arg |> Option.bind (apply f))
  | BinaryOp (op, expr1, expr2) ->
    env |> Map.tryFind (Op op)
    |> Option.bind (fun f -> env |> forceEval expr1 |> Option.bind (apply f))
    |> Option.bind (fun f -> env |> forceEval expr2 |> Option.bind (apply f))
  | If (cond, thenExpr, elseExpr) ->
    match env |> forceEval cond |> Option.bind force with
    | Some True  -> env |> eval thenExpr
    | Some False -> env |> eval elseExpr
    | _ -> None
//  | NewTuple fields ->
//    let fields = fields |> Array.map (fun expr -> env |> forceEval expr)
//    if fields |> Array.forall Option.isSome
//      then fields |> Array.map Option.get |> Tuple |> Some
//      else None
  | NewRecord fields ->
    (Some (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
      state |> Option.bind (fun (record, env) ->
        env |> forceEval expr
        |> Option.map (fun x -> record |> Map.add name x, env |> Map.add (Name name) x )))
    |> Option.map (fst >> Record)
  | NewList exprs ->
    let items = exprs |> Array.choose (fun expr -> env |> forceEval expr)
    if items.Length = exprs.Length
      then FunnyList.ofArray items |> List |> Some
      else None
  | _ -> None


let rec evalCps expr env cont =
  let apply f arg cont =
    match f with
    | Func (BuiltinFunc f) -> f.Apply arg |> Option.iter cont
    | Func (UserFunc f) -> let env = f.Env |> Map.add (Name f.Def.Arg) arg in evalCps f.Def.Body env cont
    | _ -> ()
  match expr with
  | Obj x -> cont x
  | Ref x -> env |> Map.tryFind (Name x) |> function Some x -> cont x | _ -> printfn "'%s' is not found." x
  | RefMember (expr, name) ->
    evalCps expr env (function
      | Record r -> r |> Map.tryFind name |> Option.iter cont
      | _ -> ())
  | Let (name, value, succ) ->
    evalCps value env (fun value ->
      let env = env |> Map.add (Name name) value
      match value with Func (UserFunc f) -> f.Env <- env | _ -> ()  // to enable recursive call
      evalCps succ env cont)
  | Combine (expr1, expr2) ->
    evalCps expr1 env (fun _ ->
    evalCps expr2 env cont)
  | FuncDef def -> Func (UserFunc { Def = def; Env = env }) |> cont
  | Apply (f, arg) ->
    evalCps f   env (fun f   ->
    evalCps arg env (fun arg -> apply f arg cont))
  | BinaryOp (op, expr1, expr2) ->
    env |> Map.tryFind (Op op) |> Option.iter (fun f ->
      evalCps expr1 env (fun arg -> apply f arg (fun f ->
      evalCps expr2 env (fun arg -> apply f arg cont))))
  | If (cond, thenExpr, elseExpr) ->
    evalCps cond env (function
      | True  -> evalCps thenExpr env cont
      | False -> evalCps elseExpr env cont
      | _ -> ())
//  | NewTuple fields ->
//    let fields = fields |> Array.map (fun expr -> env |> eval expr)
//    if fields |> Array.forall Option.isSome
//      then fields |> Array.map Option.get |> Tuple |> Some
//      else None
//  | NewRecord fields ->
//    let fields = fields |> Array.map (fun (name, expr) -> env |> eval expr |> Option.map (fun x -> name, x))
//    if fields |> Array.forall Option.isSome
//      then fields |> Array.map Option.get |> Map.ofArray |> Record |> Some
//      else None
  | _ -> ()

let apply = apply_ eval
