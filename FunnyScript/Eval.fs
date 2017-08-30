module FunnyScript.Eval
open System.Collections

let rec force (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind force
  | :? IMutable as x -> force x.Value
  | _ -> Ok obj

// mutable はそのままキープ
let rec forceLet (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind forceLet
  | _ -> Ok obj

let rec forceMutable (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind forceMutable
  | :? IMutable as x -> Ok x
  | _ -> Error { Value = NotMutable; Position = None }

let private addTupleToEnv names (obj : obj) env =
  match names with
  | []     -> env
  | [name] -> env |> Map.add name (Ok obj)
  | _ ->
    match obj with
    | :? (obj[]) as items when items.Length = names.Length ->
      names
      |> List.mapi (fun i name -> name, items.[i])
      |> List.fold (fun env (name, obj) -> env |> Map.add name (Ok obj)) env
    | _ -> env

module private Env =
  let tryGet pos id (env : Env) =
    env |> Map.tryFind id |> function Some x -> x | _ -> Error { Value = IdentifierNotFound id; Position = pos }

let rec eval expr env =
  let forceEval expr env =
    env |> eval expr |> Result.bind force

  let letEval expr env =
    env |> eval expr |> Result.bind forceLet

  let apply f arg = apply expr.Position f arg
  let error e = Error { Value = e; Position = expr.Position }
  let tryGet id = env |> Env.tryGet expr.Position id

  match expr.Value with
  | Obj x -> Ok x
  | Ref x -> tryGet x
  | RefMember (expr, name) ->
    let toResult x = match x with Some x -> Ok x | _ -> error (IdentifierNotFound name)
    env |> forceEval expr |> Result.bind (function
      | :? Record as r -> r |> Map.tryFind name |> toResult
      | :? Instance as x -> x.Type.Members |> Map.tryFind name |> toResult |> Result.bind (fun f -> apply (box f) x.Data)
      | :? Type as t ->
        match t.Id with
        | ClrType t -> t |> CLR.tryGetStaticMember name |> toResult
        | UserType (_, ctor) when name = "new" ->
          let ctor arg = apply (box ctor) arg |> Result.bind force |> Result.map (fun x -> box { Data = x; Type = t })
          box (BuiltinFunc { new IFuncObj with member this.Apply arg = ctor arg }) |> Ok
        | _ -> error (IdentifierNotFound name)
      | o -> o |> CLR.tryGetInstanceMember name |> toResult)
  | Let (name, value, succ) ->
    let value = env |> letEval value
    let env = env |> Map.add name value
    match value with
    | Ok (:? Func as f) -> match f with UserFunc f -> f.Env <- f.Env |> Map.add name value | _ -> ()  // to enable recursive call
    | _ -> ()
    env |> eval succ
  | Combine (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun _ ->
    env |> eval expr2)
  | FuncDef def -> UserFunc { Def = def; Env = env } |> box |> Ok
  | Apply (f, arg, apltype) ->
    match apltype with
    | NormalApply ->
      env |> forceEval f   |> Result.bind (fun f ->
      env |> forceEval arg |> Result.bind (apply f))
    | Pipeline ->
      env |> forceEval arg |> Result.bind (fun arg ->
      env |> forceEval f   |> Result.bind (fun f   -> apply f arg))
    | NullPropagationPipeline ->
      match env |> forceEval arg with
      | Ok null -> Ok null
      | Ok arg -> env |> forceEval f |> Result.bind (fun f -> apply f arg)
      | err -> err
  | If (cond, thenExpr, elseExpr) ->
    env |> forceEval cond
    |> Result.bind (function
      | :? bool as x -> env |> eval (if x then thenExpr else elseExpr)
      | x -> error (TypeMismatch (ClrType typeof<bool>, typeid x)))
  | NewRecord fields ->
    (Ok (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
      state |> Result.bind (fun (record, env) ->
        env |> letEval expr
        |> Result.map (fun x -> record |> Map.add name x, env |> Map.add name (Ok x))))
    |> Result.map (fst >> box)
  | NewList exprs ->
    let items = exprs |> Array.map (fun expr -> env |> forceEval expr)
    let error =
      items |> Array.choose (function Error e -> Some e | _ -> None) |> Array.toList
      |> function [] -> None | [e] -> Some e | es -> Some { Value = ErrorList es; Position = expr.Position }
    match error with
    | Some error -> Error error
    | _ -> items |> Array.choose (function Ok x -> Some x | _ -> None) |> box |> Ok
  | ListByRange (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun value1 ->
    env |> forceEval expr2 |> Result.bind (fun value2 ->
      match value1, value2 with
      | (:? int as value1),   (:? int   as value2) -> [| value1 .. value2 |] |> box |> Ok
      | (:? float as value1), (:? float as value2) -> [| value1 .. value2 |] |> box |> Ok
      | (:? int as value1),   (:? float as value2) -> [| float value1 .. value2 |] |> box |> Ok
      | (:? float as value1), (:? int   as value2) -> [| value1 .. float value2 |] |> box |> Ok
      | _ -> error (MiscError "not numeric type") ))
  | Substitute (expr1, expr2) ->
    env |> eval expr1 |> Result.bind forceMutable
    |> Result.bind (fun dst -> env |> eval expr2 |> Result.map (fun newval -> dst, newval))
    |> Result.map (fun (dst, newval) -> dst.Value <- newval; newval)
  | Open (record, succ) ->
    env |> forceEval record |> Result.bind (function
      | :? Record as r -> (env, r) ||> Seq.fold (fun env x -> env |> Map.add x.Key (Ok x.Value)) |> eval succ
      | x -> error (TypeMismatch (ClrType typeof<Record>, typeid x)))
  | OnError (target, handler) ->
    match env |> forceEval target with
    | Error { Value = e } ->
      let e =
        match e with
        | UserError e -> e
        | ExnError  e -> box e
        | MiscError e -> box e
        | _ -> box e
      env |> forceEval handler |> Result.bind (fun f -> apply f e)
    | x -> x

and apply pos f arg =
  let err() = Error { Value = NotApplyable (f, arg); Position = pos }
  match f with
  | :? Func as f ->
    match f with
    | BuiltinFunc f -> f.Apply arg
    | UserFunc f -> lazy (f.Env |> addTupleToEnv f.Def.Args arg |> eval f.Def.Body) |> box |> Ok
  | :? int as a ->
    match arg with
    | :? int   as b -> Ok <| box (a * b)
    | :? float as b -> Ok <| box (float a * b)
    | _ -> err()
  | :? float as a ->
    match arg with
    | :? int   as b -> Ok <| box (a * float b)
    | :? float as b -> Ok <| box (a * b)
    | _ -> err()
  | x ->
    x |> CLR.tryApplyIndexer arg |> Option.map (Result.mapError (fun e -> { Value = e; Position = pos})) |> Option.defaultWith (fun () ->
      match x, arg with
      | (:? IEnumerable as x), (:? int as i) -> Ok (x |> Seq.cast<obj> |> Seq.item i)
      | _ -> Error { Value = TypeMismatch (ClrType typeof<int>, typeid arg); Position = pos })
  //| _ -> err()
