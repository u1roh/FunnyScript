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

  let apply f arg =
    match f, arg with
    | Func (ErrHandler f), Error { Value = e } ->
      let e =
        match e with
        | UserError e -> e
        | ExnError  e -> ClrObj e
        | MiscError e -> ClrObj e
        | _ -> ClrObj e
      f.Apply e |> Result.mapError (fun e -> { Value = e; Position = expr.Position })
    | _ -> arg |> Result.bind (apply expr.Position f)

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
      | Instance (x, t) -> t.Members |> Map.tryFind name |> toResult |> Result.bind (fun f -> apply (Func f) (Ok x))
      | Type { Id = ClrType t } -> t |> CLR.tryGetStaticMember name |> toResult
      | Type ({ Id = UserType (_, ctor) } as t) when name = "new" ->
        let ctor arg = apply (Func ctor) (Ok arg) |> Result.bind force |> Result.map (fun x -> Instance (x, t)) |> Result.mapError (fun e -> e.Value)
        Func (BuiltinFunc { new IBuiltinFunc with member this.Apply arg = ctor arg }) |> Ok
      | x ->
        tryGet (typeid x |> typeName |> Name)
        |> Result.bind (function Type t -> t.Members |> Map.tryFind name |> toResult | x -> error (TypeMismatch (TypeType, typeid x)))
        |> Result.bind (fun f -> apply (Func f) (Ok x)))
  | Let (name, value, succ) ->
    let value = env |> letEval value
    let env = env |> Map.add (Name name) value
    match value with Ok (Func (UserFunc f)) -> f.Env <- env | _ -> ()  // to enable recursive call
    env |> eval succ
  | Combine (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind (fun _ ->
    env |> eval expr2)
  | FuncDef def -> Func (UserFunc { Def = def; Env = env }) |> Ok
  | Apply (f, arg) ->
    env |> forceEval f |> Result.bind (fun f -> env |> forceEval arg |> apply f)
  | BinaryOp (op, expr1, expr2) ->
    tryGet (Op op)
    |> Result.bind (fun f -> env |> forceEval expr1 |> apply f)
    |> Result.bind (fun f -> env |> forceEval expr2 |> apply f)
  | UnaryOp (op, expr) ->
    tryGet (Op op)
    |> Result.bind (fun f -> env |> forceEval expr |> apply f)
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

and apply pos f arg =
  match f with
  | Func (BuiltinFunc f) -> f.Apply arg |> Result.mapError (fun e -> { Value = e; Position = pos })
  | Func (UserFunc f) -> lazy (f.Env |> addTupleToEnv f.Def.Args arg |> eval f.Def.Body) |> Lazy |> Ok
  | List list -> match arg with Int i -> Ok list.[i] | _ -> Error { Value = TypeMismatch (IntType, typeid arg); Position = pos }
  | _ -> Error { Value = NotApplyable (f, arg); Position = pos }
