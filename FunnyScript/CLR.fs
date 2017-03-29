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

let loadAssembly (asm : System.Reflection.Assembly) env =
  asm.GetTypes()
  |> Array.map (fun t -> (if t.Namespace = null then [] else t.Namespace.Split '.' |> Array.toList), t)
  |> typesToFunnyObjs
  |> Array.fold (fun env (name, item) -> env |> Map.add (Name name) item) env

let loadSystemAssembly env =
  env |> loadAssembly typeof<System.Object>.Assembly


let rec ofFunnyObj obj =
  match obj with
  | True    -> box true
  | False   -> box false
  | Int i   -> box i
  | Float x -> box x
  | Str s   -> s :> _
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
    | StrType   -> typeof<string> :> _
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
  | :? string as x -> Str x
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
    let invoke args = m.Invoke args |> toFunnyObj |> Some
    if m.Params.Length = 1 && m.Params.[0].ParameterType = typeof<Obj> then invoke [|args|] else
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

let private tryGetMember name self (t : Type) =
  let members =
    t.GetMembers (BindingFlags.Public ||| (if Option.isNone self then BindingFlags.Static else BindingFlags.Instance))
    |> Array.filter (fun m -> m.Name = name)
  if members.Length = 0 then None else
    members |> Array.tryPick (function
      | :? PropertyInfo as x -> x.GetValue (Option.toObj self) |> toFunnyObj |> Some
      | :? FieldInfo    as x -> x.GetValue (Option.toObj self) |> toFunnyObj |> Some
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
