module FunnyScript.Builtin
open System.Collections

let private funcObj f = { new IFuncObj with member __.Apply a = f a }
let private toResult r = r |> Result.mapError (fun e -> { Value = e; Position = None })
let private builtinFunc f = BuiltinFunc (funcObj (f >> toResult))
let private toFunc0 f = box (builtinFunc (fun _ -> f()))
let private toFunc1 f = box (builtinFunc f)
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Ok)
let private toFunc3 f = toFunc1 (f >> toFunc2 >> Ok)

let private tryToFloat (obj : obj) =
  match obj with
  | :? float as x -> Some x
  | :? int   as x -> Some (float x)
  | _       -> None

let private numOp intf floatf =
  let f (x : obj) (y : obj) =
    match x, y with
    | (:? int as x), (:? int as y) -> intf x y |> Ok
    | _ ->
      match tryToFloat x, tryToFloat y with
      | Some x, Some y -> floatf x y |> Ok
      | None, _ -> Error (TypeMismatch (ClrType typeof<float>, typeid x))
      | _, None -> Error (TypeMismatch (ClrType typeof<float>, typeid y))
  toFunc2 f

let private arith intf floatf =
  numOp (fun x y -> intf   x y |> box<int>)
        (fun x y -> floatf x y |> box<float>)

let private compare intf floatf =
  numOp (fun x y -> intf   x y |> box<bool>)
        (fun x y -> floatf x y |> box<bool>)

let private logical op =
  let f (x : obj) (y : obj) =
    let x = match x with :? bool as x -> Ok x | _ -> Error (TypeMismatch (ClrType typeof<bool>, typeid x))
    let y = match y with :? bool as y -> Ok y | _ -> Error (TypeMismatch (ClrType typeof<bool>, typeid y))
    match x, y with
    | Ok x, Ok y -> op x y |> box<bool> |> Ok
    | Error e, _ -> Error e
    | _, Error e -> Error e
  toFunc2 f

let private trace arg =
  printfn "%A" arg; Ok null

let private castModule =
  let rec castToInt (x : obj) =
    match x with
    | :? int -> x |> Ok
    | :? float as x -> int x |> box |> Ok
    | :? bool  as x -> box (if x then 1 else 0) |> Ok
    | :? IMutable as x -> castToInt x.Value
    | _ -> Error (MiscError "casting to int failed")
  [
    "int", toFunc1 castToInt
  ] |> Map.ofList |> box

let private deftype id members =
  let members =
    members
    |> List.map (fun (name, f) -> name, builtinFunc f)
    |> Map.ofList
  typeName id, box { Id = id; Members = members }

let load env =
  [ "+",  arith (+) (+)
    "-",  arith (-) (-)
    "*",  arith (*) (*)
    "/",  arith (/) (/)
    "%",  toFunc2 (fun a b -> match a, b with (:? int as a), (:? int as b) -> Ok (a % b |> box) | _ -> Error (TypeMismatch (ClrType typeof<int>, typeid a)))
    "==", toFunc2 (fun a b -> box (a =  b) |> Ok)
    "!=", toFunc2 (fun a b -> box (a <> b) |> Ok)
    "<",  compare (<)  (<)
    "<=", compare (<=) (<=)
    ">",  compare (>)  (>)
    ">=", compare (>=) (>=)
    "&&", logical (&&)
    "||", logical (||)
    ":?", toFunc2 (fun o t  -> match t  with :? Type as t  -> Ok (box (t.Id = typeid o)) | _ -> Error (TypeMismatch (ClrType typeof<Type>, typeid t)))
    "~!", toFunc1 (function :? bool as x -> not x |> box |> Ok | x -> Error (TypeMismatch (ClrType typeof<bool>, typeid x)))
    "~+", toFunc1 (function :? int as x -> Ok (box +x) | :? float as x -> Ok (box +x) | x -> Error (TypeMismatch (ClrType typeof<int>, typeid x)))
    "~-", toFunc1 (function :? int as x -> Ok (box -x) | :? float as x -> Ok (box -x) | x -> Error (TypeMismatch (ClrType typeof<int>, typeid x)))
    //"::", toFunc2 (fun a ls -> match ls with List ls -> FunnyList.cons a ls |> AST.List |> Ok | _ -> Error (TypeMismatch (ListType, typeid ls)))

    "class", toFunc2 makeClass
    "mutable", toFunc1 (toMutable >> box >> Ok)
    "error", toFunc1 (fun x -> Error (UserError x))
    "trace", toFunc1 trace

    "array", toFunc2 (fun len f ->
      let f = Eval.apply None f >> Result.bind Eval.force >> function Ok x -> x | Error e -> failwith (e.ToString())
      match len with
      | :? int as len -> Array.init len f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<int>, typeid len)))

    "isEmpty", toFunc1 (function
      | :? ICollection as a -> Ok <| box (a.Count = 0)
      | :? IEnumerable as a -> Ok <| box (Seq.cast<obj> a |> Seq.isEmpty)
      | a -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid a)))

    "length", toFunc1 (function
      | (:? ICollection as a) -> Ok (box a.Count)
      | (:? IEnumerable as a) -> Ok (Seq.cast<obj> a |> Seq.length |> box)
      | a -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid a)))

    "foreach", toFunc2 (fun f src ->
      let f = Eval.apply None f >> Result.bind Eval.force >> ignore
      match src with
      | (:? (obj[]) as src) -> src |> Array.iter f; Ok null
      | (:? IEnumerable as src) -> Seq.cast<obj> src |> Seq.iter f; Ok null
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "map", toFunc2 (fun f src ->
      let f = Eval.apply None f >> Result.bind Eval.force >> function Ok x -> x | Error e -> failwith (e.ToString())
      match src with
      | (:? (obj[]) as src) -> src |> Array.map f |> box |> Ok
      | (:? IEnumerable as src) -> Seq.cast<obj> src |> Seq.map f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "choose", toFunc2 (fun f src ->
      let f = Eval.apply None f >> Result.bind Eval.force >> function Ok null -> None | Ok x -> Some x | Error e -> failwith (e.ToString())
      match src with
      | (:? (obj[]) as src) -> src |> Array.choose f |> box |> Ok
      | (:? IEnumerable as src) -> Seq.cast<obj> src |> Seq.choose f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    deftype (ClrType typeof<unit>)   []
    deftype (ClrType typeof<bool>)   []
    deftype (ClrType typeof<int>)    []
    deftype (ClrType typeof<float>)  []
    deftype (ClrType typeof<Record>) []
    deftype (ClrType typeof<Func>)   []
//    deftype ListType [
//        "head",     asList (fun x -> x.Head)
//        "tail",     asList (fun x -> List x.Tail)
//      ]
    deftype (ClrType typeof<Type>) []

    "Cast", castModule
//    "List", listModule

    "Stack", toFunc0 (fun () -> Stack() :> obj |> Ok)
  ]
  |> List.fold (fun env (name, obj) -> env |> Map.add name (Ok obj)) env

