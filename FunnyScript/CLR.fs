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
  | List ls -> ls |> FunnyList.toSeq |> Seq.map ofFunnyObj :> _
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
  | :? bool   as x -> if x then True else False
  | :? int    as x -> Int x
  | :? float  as x -> Float x
  | :? string as x -> Str x
  | _ -> ClrObj obj


let invoke f args =
  let args =
    match args with
    | Null -> [||]
    | List args -> args |> FunnyList.toSeq |> Seq.map ofFunnyObj |> Seq.toArray
    | _ -> [| ofFunnyObj args |]
  f args |> toFunnyObj

let private builtinFunc f = BuiltinFunc { new IBuiltinFunc with member __.Apply a = f a }
let private toFunc1 f = Func (builtinFunc f)
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Some)

let methodToFunnyObj (m : MethodInfo) =
  if m.IsStatic
    then toFunc1 (invoke (fun args -> m.Invoke (null, args)) >> Some)
    else toFunc2 (fun self args -> args |> invoke (fun args -> m.Invoke (ofFunnyObj self, args)) |> Some)

let tryGetStaticMethod name (t : Type) =
  let m = t.GetMethod (name, BindingFlags.Static ||| BindingFlags.Public)
  if m = null then None else methodToFunnyObj m |> Some
