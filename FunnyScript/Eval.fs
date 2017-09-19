module FunnyScript.Eval
open System
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
  | _ -> error NotMutable
    
let cast<'a> (obj : obj) =
  match obj with
  | :? 'a as obj -> Ok obj
  | _ -> error (TypeMismatch (ClrType typeof<'a>, typeid obj))

module private Env =
  let tryGet id (env : Env) =
    env |> Map.tryFind id |> function Some x -> x | _ -> error (IdentifierNotFound id)

type private IUserFuncObj =
  inherit IFuncObj
  abstract InitSelfName : string -> unit

let rec eval expr env =
  let forceEval expr env =
    env |> eval expr |> Result.bind force

  let forceEvalAs expr env =
    env |> forceEval expr |> Result.bind cast<_>

  let letEval expr env =
    env |> eval expr |> Result.bind forceLet

  let tryGet id = env |> Env.tryGet id

  Result.mapError (fun e -> { e with StackTrace = (expr, env) :: e.StackTrace }) <|
  match expr with
  | Trace (expr, _) -> eval expr env
  | Obj x -> Ok x
  | Ref x -> tryGet x
  | RefMember (expr, name) ->
    let toResult x = match x with Some x -> Ok x | _ -> error (IdentifierNotFound name)
    env |> forceEval expr |> Result.bind (function
      | :? Record as r -> r |> Map.tryFind name |> toResult
      | :? Instance as x -> x.Type.Members |> Map.tryFind name |> toResult |> Result.bind (fun f -> apply (box f) x.Data)
      | :? AST.Type as t ->
        match t.Id with
        | ClrType t -> t |> CLR.tryGetStaticMember name |> toResult
        | UserType (_, ctor) when name = "new" ->
          FuncObj.create (ctor.Apply >> Result.bind force >> Result.map (fun x -> box { Data = x; Type = t })) |> box |> Ok
        | _ -> error (IdentifierNotFound name)
      | o ->
        match o |> CLR.tryGetInstanceMember name with
        | Some x -> Ok x
        | _ ->
          let t = o.GetType()
          Seq.append (t |> Seq.unfold (fun t -> if t = null then None else Some (t, t.BaseType))) (t.GetInterfaces())
          |> Seq.tryPick (fun t ->
            let typeName = t.FullName.Split '.'
            if typeName.Length = 0 then None else env |> Map.tryFind typeName.[0]
            |> Option.bind Result.toOption
            |> Option.bind (fun x ->
              (Some x, typeName.[1..]) ||> Seq.fold (fun obj item ->
                obj |> Option.bind (function :? Record as r -> r |> Map.tryFind item | _ -> None)))
            |> Option.bind (function :? AST.Type as t -> t.Members |> Map.tryFind name | _ -> None))
          |> function
            | Some f -> apply (box f) o
            | _ -> error (IdentifierNotFound name))
  | Let (name, value, succ) ->
    if String.IsNullOrEmpty name then
      env |> forceEval value |> Result.bind (fun _ ->
      env |> eval succ)
    else
      let value = env |> letEval value
      let env = env |> Map.add name value
      match value with Ok (:? IUserFuncObj as f) -> f.InitSelfName name | _ -> ()  // to enable recursive call
      env |> eval succ
  | FuncDef def -> createUserFuncObj def env |> box |> Ok
  | Apply (f, arg) ->
    env |> forceEval f   |> Result.bind (fun f ->
    env |> forceEval arg |> Result.bind (apply f))
  | LogicalAnd (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind cast<bool>
    |> Result.bind (function
      | false -> box false |> Ok
      | true  -> env |> forceEval expr2 |> Result.bind cast<bool> |> Result.map box)
  | LogicalOr (expr1, expr2) ->
    env |> forceEval expr1 |> Result.bind cast<bool>
    |> Result.bind (function
      | true -> box true |> Ok
      | false -> env |> forceEval expr2 |> Result.bind cast<bool> |> Result.map box)
  | If (cond, thenExpr, elseExpr) ->
    env |> forceEval cond |> Result.bind cast<bool>
    |> Result.bind (fun x -> env |> eval (if x then thenExpr else elseExpr))
  | NewRecord fields ->
    (Ok (Map.empty, env), fields) ||> List.fold (fun state (name, expr) ->
      state |> Result.bind (fun (record, env) ->
        env |> letEval expr
        |> Result.map (fun x -> record |> Map.add name x, env |> Map.add name (Ok x))))
    |> Result.map (fst >> box)
  | NewList exprs ->
    let items = exprs |> Array.map (fun expr -> env |> forceEval expr)
    let err =
      items |> Array.choose (function Error { Err = e } -> Some e | _ -> None) |> Array.toList
      |> function [] -> None | [e] -> Some e | es -> Some (ErrorList es)
    match err with
    | Some err -> error err
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
    env |> forceEval record |> Result.bind cast<Record>
    |> Result.bind (fun r -> (env, r) ||> Seq.fold (fun env x -> env |> Map.add x.Key (Ok x.Value)) |> eval succ)
  | OnError (target, handler) ->
    match env |> forceEval target with
    | Error { Err = e } ->
      let e =
        match e with
        | UserError e -> e
        | ExnError  e -> box e
        | MiscError e -> box e
        | _ -> box e
      env |> forceEval handler |> Result.bind (fun f -> apply f e)
    | x -> x

and apply f arg =
  let err() = error (NotApplyable (f, arg))
  match f with
  | :? IFuncObj as f -> f.Apply arg
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
    x |> CLR.tryApplyIndexer arg |> Option.defaultWith (fun () ->
      match x, arg with
      | (:? IEnumerable as x), (:? int as i) -> Ok (x |> Seq.cast<obj> |> Seq.item i)
      | _ -> error (TypeMismatch (ClrType typeof<int>, typeid arg)))
  //| _ -> err()

and applyForce f = apply f >> Result.bind force

and private createUserFuncObj def env =
  let addTupleToEnv names (obj : obj) env =
    match names with
    | []     -> env |> Ok
    | [name] -> env |> Map.add name (Ok obj) |> Ok
    | _ ->
      match obj with
      | :? (obj[]) as items when items.Length = names.Length ->
        names
        |> List.mapi (fun i name -> name, items.[i])
        |> List.fold (fun env (name, obj) -> env |> Map.add name (Ok obj)) env
        |> Ok
      | _ -> error Unmatched
  let mutable env = env
  let mutable named = false
  { new IUserFuncObj with
    member __.Apply arg =
      lazy (env |> addTupleToEnv def.Args arg |> Result.bind (eval def.Body)) |> box |> Ok
    member self.InitSelfName name = 
      if not named then
        env <- env |> Map.add name (Ok (box self)) // to enable recursive call
        named <- true
  }