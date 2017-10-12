module FunnyScript.Eval
open System
open System.Collections

module internal Env =
  let openRecord (record : Record) env =
    (env, record) ||> Seq.fold (fun env x -> env |> Map.add x.Key (Ok x.Value))

  let matchWith pattern (obj : obj) env =
    pattern |> Pattern.tryMatchWith (fun name -> env |> Env.tryGet name) obj
    |> Option.map (Map.fold (fun env name obj -> env |> Map.add name (Ok obj)) env)
    |> function Some env -> Ok env | _ -> error Unmatched

  let findFunnyType (typeName : string) (env : Env) =
    let typeName = typeName.Split '.'
    if typeName.Length = 0 then None else env |> Map.tryFind typeName.[0]
    |> Option.bind Result.toOption
    |> Option.bind (fun x ->
      (Some x, typeName.[1..]) ||> Seq.fold (fun obj item ->
        obj |> Option.bind (function :? Record as r -> r |> Map.tryFind item | _ -> None)))
    |> Option.bind (function :? FunnyType as t -> Some t | _ -> None)

  let findExtMember (o : obj) name env =
    let t = o.GetType()
    Seq.append (t |> Seq.unfold (fun t -> if t = null then None else Some (t, t.BaseType))) (t.GetInterfaces())
    |> Seq.tryPick (fun t -> env |> findFunnyType t.FullName |> Option.bind (fun t -> t.ExtMembers |> Map.tryFind name))

type private IUserFuncObj =
  inherit IFuncObj
  abstract InitSelfName : string -> unit

let private findExtMember (env : Env) (o : obj) name =
  env |> Env.findExtMember o name
  |> function
    | Some f -> f.Apply(o, Map.empty)
    | _ -> error (IdentifierNotFound name)

let rec eval expr env =
  //Result.mapError (fun e -> { e with StackTrace = (expr, env) :: e.StackTrace }) <|
  match expr with
  | Trace (expr, pos) -> eval expr env |> Result.mapError (fun e -> { e with StackTrace = pos :: e.StackTrace })
  | Obj x -> Ok x
  | Ref x -> env |> Env.get x
  | RefMember (expr, name) ->
    env |> forceEval expr |> Result.bind (fun obj ->
      match Obj.findMember obj name with
      | Error { Err = IdentifierNotFound _ } -> findExtMember env obj name
      | result -> result)
  | Let (name, value, succ) ->
    if String.IsNullOrEmpty name then
      env |> forceEval value |> Result.bind (fun _ ->
      env |> eval succ)
    else
      env |> letEval name value |> snd |> eval succ
  | FuncDef def -> createUserFuncObj def env |> box |> Ok
  | Apply (f, arg) ->
    env |> forceEval f   |> Result.bind (fun f ->
    env |> forceEval arg |> Result.bind (Obj.applyCore env f))
  | LogicalAnd (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind Obj.cast<bool>
    |> Result.bind (function
      | false -> box false |> Ok
      | true  -> env |> forceEval expr2 |> Result.bind Obj.cast<bool> |> Result.map box)
  | LogicalOr (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind Obj.cast<bool>
    |> Result.bind (function
      | true -> box true |> Ok
      | false -> env |> forceEval expr2 |> Result.bind Obj.cast<bool> |> Result.map box)
  | If (cond, thenExpr, elseExpr) ->
    env |> forceEval cond |> Result.bind Obj.cast<bool>
    |> Result.bind (fun x -> env |> eval (if x then thenExpr else elseExpr))
  | NewRecord fields ->
    env |> recordEval fields
  | NewArray exprs ->
    let items = exprs |> Array.map (fun expr -> env |> forceEval expr)
    let err =
      items |> Array.choose (function Error { Err = e } -> Some e | _ -> None) |> Array.toList
      |> function [] -> None | [e] -> Some e | es -> Some (ErrorList es)
    match err with
    | Some err -> error err
    | _ -> items |> Array.choose (function Ok x -> Some x | _ -> None) |> box |> Ok
  | NewCase pattern ->
    pattern |> Option.map Case |> Option.defaultWith Case |> box |> Ok
  | Interval (lower, upper) ->
    env |> forceEval lower.Expr |> Result.bind Obj.cast<int> |> Result.bind (fun lowerVal ->
    env |> forceEval upper.Expr |> Result.bind Obj.cast<int> |> Result.bind (fun upperVal ->
      { min = if lower.IsOpen then lowerVal + 1 else lowerVal
        max = if upper.IsOpen then upperVal - 1 else upperVal }
      |> box |> Ok))
  | Substitute (expr1, expr2) ->
    env |> eval expr1 |> Result.bind Obj.forceMutable
    |> Result.bind (fun dst -> env |> eval expr2 |> Result.map (fun newval -> dst, newval))
    |> Result.map (fun (dst, newval) -> dst.Value <- newval; newval)
  | Open (record, succ) ->
    env |> forceEval record |> Result.bind Obj.cast<Record>
    |> Result.bind (fun r -> env |> Env.openRecord r |> eval succ)
  | Load (asm, succ) ->
    env |> CLR.loadAssembly (Reflection.Assembly.LoadFrom asm) |> eval succ
  | OnError (target, handler) ->
    match env |> forceEval target with
    | Error { Err = e } ->
      let e =
        match e with
        | UserError e -> e
        | ExnError  e -> box e
        | MiscError e -> box e
        | _ -> box e
      env |> forceEval handler |> Result.bind (fun f -> Obj.apply f e)
    | x -> x

and forceEval expr env : Result =
  env |> eval expr |> Result.bind Obj.force

and letEval name expr env =
  let value = env |> eval expr |> Result.bind Obj.forceLet
  let env = env |> Map.add name value
  match value with Ok (:? IUserFuncObj as f) -> f.InitSelfName name | _ -> ()  // to enable recursive call
  value, env

and recordEval fields env =
  (Ok (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
    state |> Result.bind (fun (record, env) ->
      let x, env = env |> letEval name expr
      x |> Result.map (fun x -> record |> Map.add name x, env)))
  |> Result.map (fst >> box)


and private createUserFuncObj def env =
  let mutable env = env
  let mutable named = false
  { new IUserFuncObj with
    member __.Apply (arg, _) =
      lazy (env |> Env.matchWith def.Args arg |> Result.bind (eval def.Body)) |> box |> Ok
    member self.InitSelfName name = 
      if not named then
        env <- env |> Map.add name (Ok (box self)) // to enable recursive call
        named <- true
  }
