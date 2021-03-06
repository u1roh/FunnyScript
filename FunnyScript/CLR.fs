﻿module FunnyScript.CLR
open System
open System.Reflection
open System.Linq.Expressions
open FSharp.Reflection

let private toFunc1 f = FuncObj.create (f >> Result.mapError ErrInfo.Create)

type private Method = {
    Invoke : obj[] -> obj
    Params : ParameterInfo[]
  } with
  static member OfMethod (m : MethodInfo, self) =
    { Invoke = fun args -> m.Invoke (self, args)
      Params = m.GetParameters() }
  static member OfConstructor (c : ConstructorInfo) =
    { Invoke = fun args -> c.Invoke args
      Params = c.GetParameters() }

let rec private tryConvert (t : Type) (arg : obj) =
  if t.IsAssignableFrom (arg.GetType()) then Some arg
  elif t.IsArray then
    match arg with
    | :? (obj[]) as src -> 
      let mid = src |> Array.choose (tryConvert (t.GetElementType()))
      if mid.Length <> src.Length then None else
        let dst = Array.CreateInstance (t.GetElementType(), src.Length)
        mid |> Array.iteri (fun i x -> dst.SetValue (x, i))
        Some (box dst)
    | _ -> None
  elif t = typeof<double> then
    match arg with
    | :? int as arg -> double arg |> box |> Some
    | _ -> None
  elif t = typeof<float32> then
    match arg with
    | :? int as arg -> float32 arg |> box |> Some
    | _ -> None
  else None

let private tryInvokeMethod (m : Method) args =
  if Array.length args = m.Params.Length then
    let args = Array.zip m.Params args |> Array.choose (fun (param, arg) -> arg |> tryConvert param.ParameterType)
    if args.Length = m.Params.Length
      then Some <| try m.Invoke args |> Ok with e -> Error (ExnError e)
      else None
  else None

let private invokeMethod (overloadMethods : Method[]) (args : obj) =
  overloadMethods |> Array.tryPick (fun m ->
    match args with
    | :? (obj[]) as args -> tryInvokeMethod m args |> Option.orElseWith (fun () -> tryInvokeMethod m [| args |])
    | null -> tryInvokeMethod m [||]
    | args -> tryInvokeMethod m [| args |])
  |> Option.defaultValue (Error (MiscError "Failed to resolve overloaded methods"))


let private ofProperty self (prop : PropertyInfo) =
  if prop.SetMethod = null then
    prop.GetValue (Option.toObj self)
  else
    box { new Mutable() with
      member __.Value
        with get() = prop.GetValue (Option.toObj self)
        and  set x = prop.SetValue (Option.toObj self, tryConvert prop.PropertyType x |> Option.toObj) }

let private ofField self (field : FieldInfo) =
  if field.IsInitOnly then
    field.GetValue (Option.toObj self)
  else
    box { new Mutable() with
      member __.Value
        with get() = field.GetValue (Option.toObj self)
        and  set x = field.SetValue (Option.toObj self, x) }

let private ofEvent self (event : EventInfo) =
  let self = Option.toObj self
  { new IObservable<obj> with
    member __.Subscribe observer =
      let handler = Action<obj, obj>(fun sender e -> observer.OnNext [| sender; e |])
      let handler =
        let sender = Expression.Parameter typeof<obj>
        let arg = Expression.Parameter typeof<obj>
        let body = Expression.Invoke (Expression.Constant handler, sender, arg)
        (Expression.Lambda (event.EventHandlerType, body, sender, arg)).Compile()
      event.AddEventHandler (self, handler)
      { new IDisposable with member __.Dispose() = event.RemoveEventHandler (self, handler) }
  } :> obj

type FunnyEvent (self : obj, event : EventInfo) =
  static let rec force (obj : obj) =
    match obj with
    | :? Lazy<Result> as x -> x.Force() |> Result.bind force
    | :? Mutable as x -> force x.Value
    | _ -> Ok obj
  member __.subscribe (handler : IFuncObj) =
    let handler = Action<obj, obj>(fun sender e ->
      handler.Apply ([| sender; e |], Env.empty)
      |> Result.bind force
      |> function Error e -> printfn "%A" e | _ -> ())
    let handler =
      let sender = Expression.Parameter typeof<obj>
      let arg    = Expression.Parameter typeof<obj>
      let body   = Expression.Invoke (Expression.Constant handler, sender, arg)
      (Expression.Lambda (event.EventHandlerType, body, sender, arg)).Compile()
    event.AddEventHandler (self, handler)
    { new IDisposable with member __.Dispose() = event.RemoveEventHandler (self, handler) }
  interface IObservable<obj> with
    member __.Subscribe observer =
      let handler = Action<obj, obj>(fun sender e -> observer.OnNext [| sender; e |])
      let handler =
        let sender = Expression.Parameter typeof<obj>
        let arg = Expression.Parameter typeof<obj>
        let body = Expression.Invoke (Expression.Constant handler, sender, arg)
        (Expression.Lambda (event.EventHandlerType, body, sender, arg)).Compile()
      event.AddEventHandler (self, handler)
      { new IDisposable with member __.Dispose() = event.RemoveEventHandler (self, handler) }


let private tryGetMember name self (t : Type) =
  let members =
    t.GetInterfaces()
    |> Array.collect (fun t -> t.GetMembers())
    |> Array.append (t.GetMembers (BindingFlags.Public ||| (if Option.isNone self then BindingFlags.Static else BindingFlags.Instance)))
    |> Array.filter (fun m -> m.Name = name)
  if members.Length = 0 then None else
    members |> Array.tryPick (function
      | :? PropertyInfo as x -> x |> ofProperty self |> Some
      | :? FieldInfo    as x -> x |> ofField    self |> Some
      | :? EventInfo    as x -> x |> ofEvent    self |> Some
      | :? Type         as x -> ClrType x |> box |> Some
      | _ -> None)
    |> Option.orElseWith (fun () ->
      members
      |> Array.choose (function :? MethodInfo as x -> Method.OfMethod (x, Option.toObj self) |> Some | _ -> None)
      |> function
        | [||]    -> None
        | methods -> Some <| box (toFunc1 (invokeMethod methods)))

let tryGetConstructor (t : Type) =
  if t.IsNested && FSharpType.IsUnion t.DeclaringType then
    let m = t.DeclaringType.GetMethod ("New" + t.Name)
    let m = Method.OfMethod (m, null)
    Some <| toFunc1 (invokeMethod [| m |])
  else
    t.GetConstructors() |> Array.map Method.OfConstructor |> function
      | [||]  -> None
      | ctors -> Some <| toFunc1 (invokeMethod ctors)

let tryGetStaticMember name t =
  tryGetMember name None t

let tryGetInstanceMember name self =
  Option.ofObj self |> Option.bind (fun self -> tryGetMember name (Some self) (self.GetType()))

let tryApplyIndexer (index : obj) (self : obj) =
  let indexer = self.GetType().GetProperty (match self with :? string -> "Chars" | _ -> "Item")
  if indexer = null then None else
    let index = match index with :? (obj[]) as index -> index | _ -> [| index |]
    if indexer.SetMethod = null then
      Some <| try indexer.GetValue (self, index) |> Ok with e -> error (ExnError e)
    else
      { new Mutable() with
          member __.Value
            with get () = indexer.GetValue (self, index)
            and  set x  = indexer.SetValue (self, x, index)
      } |> box |> Ok |> Some

let createOperatorFuncObj opName =
  let opfunc left = FuncObj.create2 (fun x y ->
    try
      (if left then x else y).GetType().GetMethod opName |> function
        | null -> error (IdentifierNotFound opName)
        | op -> op.Invoke (null, [| x; y |]) |> Ok
    with e -> error (ExnError e))
  FuncObj.ofList2 [opfunc true; opfunc false]

// ---------------

type FsFunc = {
    Method : MethodInfo
    Args : obj list
    ParamN : int
  } with
  static member New m =
    { Method = m; Args = []; ParamN = m.GetParameters().Length }
  interface IFuncObj with
    member this.Apply (arg, _) =
      let f = { this with Args = arg :: this.Args; ParamN = this.ParamN - 1 }
      if f.ParamN = 0 then
        let args = f.Args |> List.rev |> List.toArray
        let prms = f.Method.GetParameters()
        let args = Array.zip prms args |> Array.choose (fun (param, arg) -> arg |> tryConvert param.ParameterType)
        if args.Length = prms.Length then
          try f.Method.Invoke (null, args) |> box |> Ok
          with e -> Error (ErrInfo.Create (ExnError e))
        else Error (ErrInfo.Create (MiscError "function signature mismatch"))
      else f |> box |> Ok

let private ofFunction (m : MethodInfo) =
  if m.GetParameters().Length = 0 then
    { new IFuncObj with
        member this.Apply (arg, _) =
          if arg = null
            then m.Invoke (null, Array.empty) |> Ok
            else Error (ErrInfo.Create (NotApplyable (this, arg)))
    } |> box
  else FsFunc.New m |> box

let rec private ofModule (t : Type) =
  assert (FSharpType.IsModule t)
  t.GetMembers() |> Array.choose (fun m ->
    let obj =
      match m with
      | :? Type as t -> if FSharpType.IsModule t then ofModule t else box (ClrType t)
      | :? PropertyInfo as prop -> ofProperty None prop
      | :? MethodInfo as m -> ofFunction m
      | _ -> null
    if obj = null then None else Some (m.Name, obj))
  |> Map.ofArray
  |> box

let private makeTypeConstructor (t : Type) =
  let argNum = t.GetGenericArguments().Length
  FuncObj.create (fun args ->
    match args with
    | :? FunnyType as arg when argNum = 1 ->
      match arg with
      | ClrType arg -> ok (ClrType (t.MakeGenericType [| arg |]))
      | _ -> error (MiscError "Not CLR Type")
    | FunnyArray args when args.Count = argNum ->
      let args = args |> FunnyArray.map (function
        | :? FunnyType as t ->  match t with ClrType t -> Ok t | _ -> error (MiscError "Not CLR Type")
        | a -> error (TypeMismatch (ClrType typeof<FunnyType>, FunnyType.ofObj a)))
      let errors = args |> Array.choose Result.toErrorOption |> Array.toList
      if errors.Length > 0 then error (ErrorList errors) else
        let args = args |> Array.choose Result.toOption
        ok (ClrType (t.MakeGenericType args))
    | _ -> error (MiscError (sprintf "Failed to Construct Generic Type of %s" t.FullName)))

let rec private typesToFunnyObjs (types : (string list * Type)[]) =
  let leaves, branches = types |> Array.partition (fst >> List.isEmpty)
  let leaves = leaves |> Array.map (snd >> fun t ->
    if FSharpType.IsModule t then
      t.Name, ofModule t
    elif t.IsGenericTypeDefinition && not t.IsNested
      then t.Name.Substring (0, t.Name.IndexOf '`'), box (makeTypeConstructor t)
      else t.Name, box (ClrType t))
  branches
  |> Array.groupBy (fst >> List.head)
  |> Array.map (fun (name, items) ->
    let record =
      items
      |> Array.map (fun (ns, t) -> ns.Tail, t)
      |> typesToFunnyObjs
      |> Map.ofArray
      |> box
    name, record)
  |> Array.append leaves

let rec private mergeRecord (r1 : Record) (r2 : Record) =
  (r1, r2) ||> Seq.fold (fun acc item ->
    match acc |> Map.tryFind item.Key, item.Value with
    | Some (:? Record as r1), (:? Record as r2) -> acc |> Map.add item.Key (mergeRecord r1 r2 |> box)
    | _ -> acc |> Map.add item.Key item.Value)

let loadAssembly (asm : System.Reflection.Assembly) (env : Env) =
  asm.GetTypes()
  |> Array.filter (fun t -> not t.IsNested)
  |> Array.map (fun t -> (if t.Namespace = null then [] else t.Namespace.Split '.' |> Array.toList), t)
  |> typesToFunnyObjs
  |> Array.fold (fun (env : Env) (name, item) ->
    match env |> Env.tryFind name, item with
    | Some (Ok (:? Record as r1)), (:? Record as r2) -> env |> Env.add name (mergeRecord r1 r2 |> box |> Ok)
    | _ -> env |> Env.add name (Ok item)) env

let loadSystemAssembly env =
  env
  |> loadAssembly typeof<System.Object>.Assembly
  |> loadAssembly typeof<System.Timers.Timer>.Assembly

