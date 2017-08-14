module FunnyScript.CLR
open System
open System.Reflection

let rec private typesToFunnyObjs (types : (string list * System.Type)[]) =
  let leaves, branches = types |> Array.partition (fst >> List.isEmpty)
  let leaves = leaves |> Array.map (snd >> fun t -> t.Name, AST.Type { Id = ClrType t; Members = Map.empty })
  branches
  |> Array.groupBy (fst >> List.head)
  |> Array.map (fun (name, items) ->
    let record =
      items
      |> Array.map (fun (ns, t) -> ns.Tail, t)
      |> typesToFunnyObjs
      |> Map.ofArray
      |> Record
    name, record)
  |> Array.append leaves

let rec private mergeRecord (r1 : Map<string, Obj>) (r2 : Map<string, Obj>) =
  (r1, r2) ||> Seq.fold (fun acc item ->
    match acc |> Map.tryFind item.Key, item.Value with
    | Some (Record r1), Record r2 -> acc |> Map.add item.Key (mergeRecord r1 r2 |> Record)
    | _ -> acc |> Map.add item.Key item.Value)

let loadAssembly (asm : System.Reflection.Assembly) env =
  asm.GetTypes()
  |> Array.map (fun t -> (if t.Namespace = null then [] else t.Namespace.Split '.' |> Array.toList), t)
  |> typesToFunnyObjs
  |> Array.fold (fun env (name, item) ->
    match env |> Map.tryFind (Name name), item with
    | Some (Ok (Record r1)), Record r2 -> env |> Map.add (Name name) (mergeRecord r1 r2 |> Record |> Ok)
    | _ -> env |> Map.add (Name name) (Ok item)) env

let loadSystemAssembly env =
  env
  |> loadAssembly typeof<System.Object>.Assembly
  |> loadAssembly typeof<System.Timers.Timer>.Assembly


let rec ofFunnyObj obj =
  match obj with
  | True    -> box true
  | False   -> box false
  | Int i   -> box i
  | Float x -> box x
  | Record r -> r :> _
  | Func f  -> f :> _
  | List ls -> ls |> FunnyList.toSeq |> Seq.map ofFunnyObj |> Seq.toArray :> _
  | ClrObj x -> x
  | Type { Id = t } ->
    match t with
    | NullType  -> typeof<Unit> :> _
    | BoolType  -> typeof<bool> :> _
    | IntType   -> typeof<int> :> _
    | FloatType -> typeof<float> :> _
    | RecordType  -> null
    | FuncType  -> null
    | ListType  -> typeof<list<obj>> :> _
    | TypeType  -> typeof<System.Type> :> _
    | LazyType  -> null
    | UserType _ -> null
    | ClrType t -> t :> _
  | _ -> null


let toFunnyObj (obj : obj) =
  match obj with
  | null -> Null
  | :? Obj    as x -> x
  | :? bool   as x -> if x then True else False
  | :? int    as x -> Int x
  | :? float  as x -> Float x
  | _ -> ClrObj obj


let private builtinFunc f = BuiltinFunc { new IBuiltinFunc with member __.Apply a = f a }
let private toFunc1 f = Func (builtinFunc f)

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

let private invokeMethod (overloadMethods : Method[]) args =
  overloadMethods |> Array.tryPick (fun m ->
    let invoke args = (try m.Invoke args |> toFunnyObj |> Ok with e -> Error (ExnError e)) |> Some
    match args with
    | Null -> if m.Params.Length = 0 then invoke [||] else None
    | List args ->
      if args.Length = Definite m.Params.Length then
        let args = args |> FunnyList.toSeq |> Seq.map ofFunnyObj |> Seq.toArray
        if args |> Array.mapi (fun i arg -> m.Params.[i].ParameterType.IsAssignableFrom (arg.GetType())) |> Array.forall id
          then invoke args
          else None
      else None
    | _ when m.Params.Length = 1 ->
      let a = ofFunnyObj args
      if m.Params.[0].ParameterType.IsAssignableFrom (a.GetType())
        then invoke [| a |]
        else None
    | _ -> None)
  |> Option.defaultValue (Error (MiscError "Failed to resolve overloaded methods"))

let private ofProperty self (prop : PropertyInfo) =
  if prop.SetMethod = null then
    prop.GetValue (Option.toObj self) |> toFunnyObj
  else
    Mutable { new IMutable with
      member __.Value
        with get() = prop.GetValue (Option.toObj self) |> toFunnyObj
        and  set x = prop.SetValue (Option.toObj self, ofFunnyObj x) }

let private ofField self (field : FieldInfo) =
  if field.IsInitOnly then
    field.GetValue (Option.toObj self) |> toFunnyObj
  else
    Mutable { new IMutable with
      member __.Value
        with get() = field.GetValue (Option.toObj self) |> toFunnyObj
        and  set x = field.SetValue (Option.toObj self, ofFunnyObj x) }

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
