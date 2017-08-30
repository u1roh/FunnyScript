module FunnyScript.CLR
open System
open System.Reflection

let rec private typesToFunnyObjs (types : (string list * System.Type)[]) =
  let leaves, branches = types |> Array.partition (fst >> List.isEmpty)
  let leaves = leaves |> Array.map (snd >> fun t -> t.Name, box { Id = ClrType t; Members = Map.empty })
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
  |> Array.map (fun t -> (if t.Namespace = null then [] else t.Namespace.Split '.' |> Array.toList), t)
  |> typesToFunnyObjs
  |> Array.fold (fun (env : Env) (name, item) ->
    match env |> Map.tryFind name, item with
    | Some (Ok (:? Record as r1)), (:? Record as r2) -> env |> Map.add name (mergeRecord r1 r2 |> box |> Ok)
    | _ -> env |> Map.add name (Ok item)) env

let loadSystemAssembly env =
  env
  |> loadAssembly typeof<System.Object>.Assembly
  |> loadAssembly typeof<System.Timers.Timer>.Assembly




let private builtinFunc f = BuiltinFunc { new IFuncObj with member __.Apply a = f a |> Result.mapError (fun e -> { Value = e; Position = None }) }
let private toFunc1 f = box (builtinFunc f)

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

let private invokeMethod (overloadMethods : Method[]) (args : obj) =
  overloadMethods |> Array.tryPick (fun m ->
    let invoke args = (try m.Invoke args |> Ok with e -> Error (ExnError e)) |> Some
    match args with
    | null -> if m.Params.Length = 0 then invoke [||] else None
    | :? (obj[]) as args ->
      if args.Length = m.Params.Length then
        if args |> Array.mapi (fun i arg -> m.Params.[i].ParameterType.IsAssignableFrom (arg.GetType())) |> Array.forall id
          then invoke args
          else None
      else None
    | _ when m.Params.Length = 1 ->
      if m.Params.[0].ParameterType.IsAssignableFrom (args.GetType())
        then invoke [| args |]
        else None
    | _ -> None)
  |> Option.defaultValue (Error (MiscError "Failed to resolve overloaded methods"))

let private ofProperty self (prop : PropertyInfo) =
  if prop.SetMethod = null then
    prop.GetValue (Option.toObj self)
  else
    box { new IMutable with
      member __.Value
        with get() = prop.GetValue (Option.toObj self)
        and  set x = prop.SetValue (Option.toObj self, x) }

let private ofField self (field : FieldInfo) =
  if field.IsInitOnly then
    field.GetValue (Option.toObj self)
  else
    box { new IMutable with
      member __.Value
        with get() = field.GetValue (Option.toObj self)
        and  set x = field.SetValue (Option.toObj self, x) }

let private tryGetMember name self (t : Type) =
  let members =
    t.GetMembers (BindingFlags.Public ||| (if Option.isNone self then BindingFlags.Static else BindingFlags.Instance))
    |> Array.filter (fun m -> m.Name = name)
  if members.Length = 0 then None else
    members |> Array.tryPick (function
      | :? PropertyInfo as x -> x |> ofProperty self |> Some
      | :? FieldInfo    as x -> x |> ofField    self |> Some
      | _ -> None)
    |> Option.orElseWith (fun () ->
      members
      |> Array.choose (function :? MethodInfo as x -> Method.OfMethod (x, Option.toObj self) |> Some | _ -> None)
      |> function
        | [||]    -> None
        | methods -> Some <| toFunc1 (invokeMethod methods))

let private tryGetConstructor (t : Type) =
  t.GetConstructors() |> Array.map Method.OfConstructor |> function
    | [||]  -> None
    | ctors -> Some <| toFunc1 (invokeMethod ctors)

let tryGetStaticMember name t =
  if name = "new"
    then tryGetConstructor t
    else tryGetMember name None t

let tryGetInstanceMember name self =
  tryGetMember name (Some self) (self.GetType())

let tryApplyIndexer (index : obj) (self : obj) =
  let indexer = self.GetType().GetProperty (match self with :? string -> "Chars" | _ -> "Item")
  if indexer = null then None else
    let index = match index with :? (obj[]) as index -> index | _ -> [| index |]
    Some <| try indexer.GetValue (self, index) |> Ok with e -> Error (ExnError e)
