module FunnyScript.Builtin
open System.Collections

let private builtinFunc f = BuiltinFunc { new IBuiltinFunc with member __.Apply a = f a }
let private toFunc0 f = Func (builtinFunc (fun _ -> f()))
let private toFunc1 f = Func (builtinFunc f)
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Ok)
let private toFunc3 f = toFunc1 (f >> toFunc2 >> Ok)

let private tryToFloat obj =
  match obj with
  | Float x -> Some x
  | Int   x -> Some (float x)
  | _       -> None

let private numOp intf floatf =
  let f x y =
    match x, y with
    | Int x, Int y -> intf x y |> Ok
    | _ ->
      match tryToFloat x, tryToFloat y with
      | Some x, Some y -> floatf x y |> Ok
      | None, _ -> Error (TypeMismatch (FloatType, typeid x))
      | _, None -> Error (TypeMismatch (FloatType, typeid y))
  toFunc2 f

let private arith intf floatf =
  numOp (fun x y -> intf   x y |> Int)
        (fun x y -> floatf x y |> Float)

let private compare intf floatf =
  numOp (fun x y -> if intf   x y then True else False)
        (fun x y -> if floatf x y then True else False)

let private logical op =
  let f x y =
    let x = match x with True -> Ok true | False -> Ok false | _ -> Error (TypeMismatch (BoolType, typeid x))
    let y = match y with True -> Ok true | False -> Ok false | _ -> Error (TypeMismatch (BoolType, typeid y))
    match x, y with
    | Ok x, Ok y -> (if op x y then True else False) |> Ok
    | Error e, _ -> Error e
    | _, Error e -> Error e
  toFunc2 f

let private sin x =
  match x with
  | Float x -> Float (sin x) |> Ok
  | _ -> TypeMismatch (FloatType, typeid x) |> Error

let private trace arg =
  printfn "%A" arg; Ok Null

let private listModule =
  let init len f =
    match len with
    | Int len ->
      let a = Array.init len (fun i -> Eval.apply f (Int i) |> Result.bind Eval.force)
      let error =
        a |> Array.choose Result.toErrorOption |> Array.toList
        |> function [] -> None | [e] -> Some e | es -> Some (ErrorList es)
      match error with
      | Some error -> Error error
      | _ -> a |> Array.choose Result.toOption |> FunnyList.ofArray |> List |> Ok
    | _ -> Error (TypeMismatch (IntType, typeid len))

  [ "init", toFunc2 init
  ] |> Map.ofList |> Record

let private deftype id members =
  let members =
    members
    |> List.map (fun (name, f) -> name, builtinFunc f)
    |> Map.ofList
  Name (typeName id), Type { Id = id; Members = members }

let private asList f arg =
  match arg with
  | List x -> Ok (f x)
  | _ -> Error (TypeMismatch (ListType, typeid arg))

let load env =
  [ Op Plus,  arith (+) (+)
    Op Minus, arith (-) (-)
    Op Mul,   arith (*) (*)
    Op Div,   arith (/) (/)
    Op Mod,   toFunc2 (fun a b -> match a, b with Int a, Int b -> Ok (a % b |> Int) | _ -> Error (TypeMismatch (IntType, typeid a)))
    Op Equal, toFunc2 (fun a b -> Ok (if a = b then True else False))
    Op NotEq, toFunc2 (fun a b -> Ok (if a = b then False else True))
    Op Less,      compare (<)  (<)
    Op LessEq,    compare (<=) (<=)
    Op Greater,   compare (>)  (>)
    Op GreaterEq, compare (>=) (>=)
    Op LogicalAnd, logical (&&)
    Op LogicalOr,  logical (||)
    Op LogicalNot, toFunc1 (function True -> Ok False | False -> Ok True | x -> Error (TypeMismatch (BoolType, typeid x)))
    Op Is,   toFunc2 (fun o t  -> match t  with Type t  -> Ok (if t.Id = typeid o then True else False) | _ -> Error (TypeMismatch (TypeType, typeid t)))
    Op Cons, toFunc2 (fun a ls -> match ls with List ls -> FunnyList.cons a ls |> AST.List |> Ok | _ -> Error (TypeMismatch (ListType, typeid ls)))

    Name "mutable", toFunc1 (ref >> Mutable >> Ok)
    Name "trace", toFunc1 trace
    Name "sin", toFunc1 sin

    deftype NullType   []
    deftype BoolType   []
    deftype IntType    []
    deftype FloatType  []
    deftype RecordType []
    deftype FuncType   []
    deftype ListType [
        "isEmpty",  asList (fun x -> if x.IsEmpty then True else False)
        "head",     asList (fun x -> x.Head)
        "tail",     asList (fun x -> List x.Tail)
        "length",   asList (fun x -> match x.Length with Definite n -> Int n | _ -> Null)
      ]
    deftype TypeType []

    Name "List", listModule

    Name "Stack", toFunc0 (fun () -> Stack() :> obj |> ClrObj |> Ok)
  ]
  |> List.fold (fun env (id, obj) -> env |> Map.add id obj) env

