﻿module FunnyScript.Builtin
open System
open System.Collections

let private toFunc1 f = box (FuncObj.create (f >> Result.mapError ErrInfo.Create))
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Ok)

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
    |> List.map (fun (name, f) -> name, FuncObj.create f)
    |> Map.ofList
  typeName id, box { Id = id; ExtMembers = members }

let load env =
  let env = env |> Map.add "unmatched" (error Unmatched)
  [ "+",  arith (+) (+)
    "-",  arith (-) (-)
    "*",  arith (*) (*)
    "/",  arith (/) (/)
    "%",  FuncObj.ofFun2 (fun a b -> a % b) :> obj
    "==", FuncObj.ofFun2 (fun a b -> a =  b) :> obj
    "!=", FuncObj.ofFun2 (fun a b -> a <> b) :> obj
    "<",  compare (<)  (<)
    "<=", compare (<=) (<=)
    ">",  compare (>)  (>)
    ">=", compare (>=) (>=)
    "|>",  FuncObj.create2 (fun arg f -> Eval.apply f arg) |> box
    "|?>", FuncObj.create2 (fun arg f -> if arg = null then Ok null else Eval.apply f arg) |> box
    "&&", logical (&&)
    "||", logical (||)
    ":?", FuncObj.ofFun2 (fun o (t : FunnyType) -> t.Id = typeid o) :> obj
    "~!", FuncObj.ofFun not :> obj
    "~+", FuncObj.ofList [FuncObj.ofFun (fun (x : int) -> +x); FuncObj.ofFun (fun (x : float) -> +x)] :> obj
    "~-", FuncObj.ofList [FuncObj.ofFun (fun (x : int) -> -x); FuncObj.ofFun (fun (x : float) -> -x)] :> obj
    //"::", toFunc2 (fun a ls -> match ls with List ls -> FunnyList.cons a ls |> AST.List |> Ok | _ -> Error (TypeMismatch (ListType, typeid ls)))

    "class", FuncObj.create2 makeClass |> box
    "mutable", FuncObj.ofFun toMutable :> obj
    "error", FuncObj.create (fun x -> error (UserError x)) :> obj
    "trace", FuncObj.create trace :> obj

    "match", FuncObj.create2 (fun handlers x ->
      match handlers with
      | :? (obj[]) as handlers ->
        handlers |> Array.tryPick (fun f ->
          match Eval.applyForce f x with
          | Error { Err = Unmatched } -> None
          | result -> Some result)
        |> Option.defaultValue (error Unmatched)
      | _ -> error (TypeMismatch (ClrType typeof<IFuncObj[]>, typeid handlers))) |> box

    "extend", FuncObj.ofFun2 extendType :> obj

    "array", toFunc2 (fun len f ->
      let f = Eval.applyForce f >> function Ok x -> x | Error e -> raiseErrInfo e
      match len with
      | :? int as len -> Array.init len f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<int>, typeid len)))

    "isEmpty", toFunc1 (function
      | :? ICollection as a -> Ok <| box (a.Count = 0)
      | :? IEnumerable as a -> Ok <| box (Seq.cast<obj> a |> Seq.isEmpty)
      | a -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid a)))

    "length", toFunc1 (function
      | :? ICollection as a -> Ok (box a.Count)
      | :? IEnumerable as a -> Ok (Seq.cast<obj> a |> Seq.length |> box)
      | a -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid a)))

    "foreach", toFunc2 (fun f src ->
      let f = Eval.applyForce f >> ignore
      match src with
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.iter f; Ok null
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "map", toFunc2 (fun f src ->
      let f = Eval.applyForce f >> function Ok x -> x | Error e -> raiseErrInfo e
      match src with
      | :? (obj[]) as src -> src |> Array.map f |> box |> Ok
      | :? Array as src -> Array.init src.Length (src.GetValue >> f) |> box |> Ok
      | :? IFunnyArray as src -> Array.init src.Length (fun i -> f src.[i]) |> box |> Ok
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.map f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "choose", toFunc2 (fun f src ->
      let f = Eval.applyForce f >> function
        | Ok null -> None | Ok x -> Some x
        | Error { Err = Unmatched } -> None | Error e -> raiseErrInfo e
      match src with
      | :? (obj[]) as src -> src |> Array.choose f |> box |> Ok
      | :? Array as src -> Array.init src.Length (src.GetValue >> f) |> Array.choose id |> box |> Ok
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.choose f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "fold", FuncObj.create3 (fun acc0 f src ->
      let f acc x = acc |> Result.bind (Eval.applyForce f) |> Result.bind (fun f -> x |> Eval.applyForce f)
      match src with
      | (:? IEnumerable as src) -> Seq.cast<obj> src |> Seq.fold f (Ok acc0)
      | _ -> error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src))) |> box

    deftype (ClrType typeof<unit>)   []
    deftype (ClrType typeof<bool>)   []
    deftype (ClrType typeof<int>)    []
    deftype (ClrType typeof<float>)  []
    deftype (ClrType typeof<Record>) []
    deftype (ClrType typeof<IFuncObj>) []
//    deftype ListType [
//        "head",     asList (fun x -> x.Head)
//        "tail",     asList (fun x -> List x.Tail)
//      ]
    deftype (ClrType typeof<FunnyType>) []

    "Cast", castModule
//    "List", listModule

  ]
  |> List.fold (fun env (name, obj) -> env |> Map.add name (Ok obj)) env

