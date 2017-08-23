module FunnyScript.Eval

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
  | _ -> Error { Value = NotMutable; Position = None }

let private addTupleToEnv names obj env =
  match names with
  | []     -> env
  | [name] -> env |> Map.add (Name name) (Ok obj)
  | _ ->
    match obj with
    | List items ->
      names
      |> List.mapi (fun i name -> Name name, items.[i])
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
  | Ref x -> tryGet (Name x)
  | RefMember (expr, name) ->
    let toResult x = match x with Some x -> Ok x | _ -> error (IdentifierNotFound (Name name))
    env |> forceEval expr |> Result.bind (function
      | Record r -> r |> Map.tryFind name |> toResult
      | ClrObj o -> o |> CLR.tryGetInstanceMember name |> toResult
      | Instance (x, t) -> t.Members |> Map.tryFind name |> toResult |> Result.bind (fun f -> apply (Func f) x)
      | Type { Id = ClrType t } -> t |> CLR.tryGetStaticMember name |> toResult
      | Type ({ Id = UserType (_, ctor) } as t) when name = "new" ->
        let ctor arg = apply (Func ctor) arg |> Result.bind force |> Result.map (fun x -> Instance (x, t))
        Func (BuiltinFunc { new IFuncObj with member this.Apply arg = ctor arg }) |> Ok
      | x ->
        tryGet (typeid x |> typeName |> Name)
        |> Result.bind (function Type t -> t.Members |> Map.tryFind name |> toResult | x -> error (TypeMismatch (TypeType, typeid x)))
        |> Result.bind (fun f -> apply (Func f) x))
  | Let (name, value, succ) ->
    let value = env |> letEval value
    let env = env |> Map.add (Name name) value
    match value with Ok (Func (UserFunc f)) -> f.Env <- f.Env |> Map.add (Name name) value | _ -> ()  // to enable recursive call
    env |> eval succ
  | Combine (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun _ ->
    env |> eval expr2)
  | FuncDef def -> Func (UserFunc { Def = def; Env = env }) |> Ok
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
      | Ok Null -> Ok Null
      | Ok arg -> env |> forceEval f |> Result.bind (fun f -> apply f arg)
      | err -> err
  | BinaryOp (op, expr1, expr2) ->
    tryGet (Op op)
    |> Result.bind (fun f -> env |> forceEval expr1 |> Result.bind (apply f))
    |> Result.bind (fun f -> env |> forceEval expr2 |> Result.bind (apply f))
  | UnaryOp (op, expr) ->
    tryGet (Op op)
    |> Result.bind (fun f -> env |> forceEval expr |> Result.bind (apply f))
  | If (cond, thenExpr, elseExpr) ->
    env |> forceEval cond
    |> Result.bind (function
      | True  -> env |> eval thenExpr
      | False -> env |> eval elseExpr
      | x -> error (TypeMismatch (BoolType, typeid x)))
  | NewRecord fields ->
    (Ok (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
      state |> Result.bind (fun (record, env) ->
        env |> letEval expr
        |> Result.map (fun x -> record |> Map.add name x, env |> Map.add (Name name) (Ok x))))
    |> Result.map (fst >> Record)
  | NewList exprs ->
    let items = exprs |> Array.map (fun expr -> env |> forceEval expr)
    let error =
      items |> Array.choose (function Error e -> Some e | _ -> None) |> Array.toList
      |> function [] -> None | [e] -> Some e | es -> Some { Value = ErrorList es; Position = expr.Position }
    match error with
    | Some error -> Error error
    | _ -> items |> Array.choose (function Ok x -> Some x | _ -> None) |> FunnyList.ofArray |> List |> Ok
  | ListByRange (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun value1 ->
    env |> forceEval expr2 |> Result.bind (fun value2 ->
      match value1, value2 with
      | Int   value1, Int   value2 -> [| value1 .. value2 |] |> Array.map Int   |> FunnyList.ofArray |> List |> Ok
      | Float value1, Float value2 -> [| value1 .. value2 |] |> Array.map Float |> FunnyList.ofArray |> List |> Ok
      | Int   value1, Float value2 -> [| float value1 .. value2 |] |> Array.map Float |> FunnyList.ofArray |> List |> Ok
      | Float value1, Int   value2 -> [| value1 .. float value2 |] |> Array.map Float |> FunnyList.ofArray |> List |> Ok
      | _ -> error (MiscError "not numeric type") ))
  | Substitute (expr1, expr2) ->
    env |> eval expr1 |> Result.bind forceMutable
    |> Result.bind (fun dst -> env |> eval expr2 |> Result.map (fun newval -> dst, newval))
    |> Result.map (fun (dst, newval) -> dst.Value <- newval; newval)
  | Open (record, succ) ->
    env |> forceEval record |> Result.bind (function
      | Record r -> (env, r) ||> Seq.fold (fun env x -> env |> Map.add (Name x.Key) (Ok x.Value)) |> eval succ
      | x -> error (TypeMismatch (RecordType, typeid x)))
  | OnError (target, handler) ->
    match env |> forceEval target with
    | Error { Value = e } ->
      let e =
        match e with
        | UserError e -> e
        | ExnError  e -> ClrObj e
        | MiscError e -> ClrObj e
        | _ -> ClrObj e
      env |> forceEval handler |> Result.bind (fun f -> apply f e)
    | x -> x

and apply pos f arg =
  let err() = Error { Value = NotApplyable (f, arg); Position = pos }
  match f with
  | Func (BuiltinFunc f) -> f.Apply arg
  | Func (UserFunc f) -> lazy (f.Env |> addTupleToEnv f.Def.Args arg |> eval f.Def.Body) |> Lazy |> Ok
  | List list -> match arg with Int i -> Ok list.[i] | _ -> Error { Value = TypeMismatch (IntType, typeid arg); Position = pos }
  | Int a ->
    match arg with
    | Int   b -> Ok <| Int (a * b)
    | Float b -> Ok <| Float (float a * b)
    | _ -> err()
  | Float a ->
    match arg with
    | Int   b -> Ok <| Float (a * float b)
    | Float b -> Ok <| Float (a * b)
    | _ -> err()
  | _ -> err()
