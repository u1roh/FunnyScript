﻿module FunnyScript.Eval

let rec force obj =
  match obj with
  | Lazy x -> x.Force() |> Result.bind force
  | Mutable x -> force x.Value
  | _ -> Ok obj

// mutable はそのままキープ
let rec forceLet obj =
  match obj with
  | Lazy x -> x.Force() |> Result.bind forceLet
  | _ -> Ok obj

let rec forceMutable obj =
  match obj with
  | Lazy x -> x.Force() |> Result.bind forceMutable
  | Mutable x -> Ok x
  | _ -> Error NotMutable

let private addTupleToEnv names obj env =
  match names with
  | []     -> env
  | [name] -> env |> Map.add (Name name) obj
  | _ ->
    match obj with
    | List items ->
      names
      |> List.mapi (fun i name -> Name name, items.[i])
      |> List.fold (fun env (name, obj) -> env |> Map.add name obj) env
    | _ -> env

let private apply_ eval f arg =
  match f with
  | Func (BuiltinFunc f) -> f.Apply arg
  | Func (UserFunc f) -> lazy (f.Env |> addTupleToEnv f.Def.Args arg |> eval f.Def.Body) |> Lazy |> Ok
  | List list -> match arg with Int i -> Ok list.[i] | _ -> TypeMismatch (IntType, typeid arg) |> Error
  | _ -> NotApplyable (f, arg) |> Error

let rec eval expr env =
  let apply = apply_ eval
  
  let forceEval expr env =
    env |> eval expr |> Result.bind force

  let letEval expr env =
    env |> eval expr |> Result.bind forceLet

  match expr with
  | Obj x -> Ok x
  | Ref x -> env |> Map.tryFind (Name x) |> function Some x -> Ok x | _ -> Error (IdentifierNotFound (Name x))
  | RefMember (expr, name) ->
    env |> forceEval expr |> Result.bind (fun x ->
      let ret =
        match x with
        | Record r -> r |> Map.tryFind name
        | ClrObj o -> o |> CLR.tryGetInstanceMember name
        | Type { Id = ClrType t } -> t |> CLR.tryGetStaticMember name
        | _ -> None
      if ret.IsSome then Ok ret.Value else
        env |> Map.tryFind (typeid x |> typeName |> Name)
        |> Option.bind (function Type t -> t.Members |> Map.tryFind name | _ -> None)
        |> function Some x -> Ok x | _ -> Error (IdentifierNotFound (Name name))
        |> Result.bind (fun f -> apply (Func f) x))
  | Let (name, value, succ) ->
    env |> letEval value |> Result.bind (fun value ->
      let env = env |> Map.add (Name name) value
      match value with Func (UserFunc f) -> f.Env <- env | _ -> ()  // to enable recursive call
      env |> eval succ)
  | Combine (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun _ ->
    env |> eval expr2)
  | FuncDef def -> Func (UserFunc { Def = def; Env = env }) |> Ok
  | Apply (f, arg) ->
    env |> forceEval f   |> Result.bind (fun f ->
    env |> forceEval arg |> Result.bind (apply f))
  | BinaryOp (op, expr1, expr2) ->
    env |> Map.tryFind (Op op)
    |> function Some f -> Ok f | _ -> Error (IdentifierNotFound (Op op))
    |> Result.bind (fun f -> env |> forceEval expr1 |> Result.bind (apply f))
    |> Result.bind (fun f -> env |> forceEval expr2 |> Result.bind (apply f))
  | UnaryOp (op, expr) ->
    env |> Map.tryFind (Op op)
    |> function Some f -> Ok f | _ -> Error (IdentifierNotFound (Op op))
    |> Result.bind (fun f -> env |> forceEval expr |> Result.bind (apply f))
  | If (cond, thenExpr, elseExpr) ->
    env |> forceEval cond
    |> Result.bind (function
      | True  -> env |> eval thenExpr
      | False -> env |> eval elseExpr
      | x -> Error (TypeMismatch (BoolType, typeid x)))
  | NewRecord fields ->
    (Ok (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
      state |> Result.bind (fun (record, env) ->
        env |> letEval expr
        |> Result.map (fun x -> record |> Map.add name x, env |> Map.add (Name name) x )))
    |> Result.map (fst >> Record)
  | NewList exprs ->
    let items = exprs |> Array.map (fun expr -> env |> forceEval expr)
    let error =
      items |> Array.choose (function Error e -> Some e | _ -> None) |> Array.toList
      |> function [] -> None | [e] -> Some e | es -> Some (ErrorList es)
    match error with
    | Some error -> Error error
    | _ -> items |> Array.choose (function Ok x -> Some x | _ -> None) |> FunnyList.ofArray |> List |> Ok
  | Substitute (expr1, expr2) ->
    env |> eval expr1 |> Result.bind forceMutable
    |> Result.bind (fun dst -> env |> eval expr2 |> Result.map (fun newval -> dst, newval))
    |> Result.map (fun (dst, newval) -> dst.Value <- newval; newval)


let rec evalCps expr env cont =
  let apply f arg cont =
    match f with
    | Func (BuiltinFunc f) -> f.Apply arg |> Result.toOption |> Option.iter cont
    | Func (UserFunc f) -> let env = f.Env |> addTupleToEnv f.Def.Args arg in evalCps f.Def.Body env cont
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
