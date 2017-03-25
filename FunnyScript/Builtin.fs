module FunnyScript.Builtin

let private toFunc1 f = Func (BuiltinFunc { new IBuiltinFunc with member __.Apply a = f a })
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Some)
let private toFunc3 f = toFunc1 (f >> toFunc2 >> Some)

let private tryToFloat obj =
  match obj with
  | Float x -> Some x
  | Int   x -> Some (float x)
  | _       -> None

let private numOp intf floatf =
  let f x y =
    match x, y with
    | Int x, Int y -> intf x y |> Some
    | _ ->
      match tryToFloat x, tryToFloat y with
      | Some x, Some y -> floatf x y |> Some
      | _ -> None
  toFunc2 f

let private arith intf floatf =
  numOp (fun x y -> intf   x y |> Int)
        (fun x y -> floatf x y |> Float)

let private compare intf floatf =
  numOp (fun x y -> if intf   x y then True else False)
        (fun x y -> if floatf x y then True else False)

let private sin x =
  match x with
  | Float x -> Float (sin x) |> Some
  | _ -> None

let private trace arg =
  printfn "%A" arg; Some Null

let private listModule =
  let init len f =
    match len with
    | Int len ->
      let a = Array.init len (fun i -> Eval.apply f (Int i) |> Option.bind Eval.force)
      if a |> Array.forall Option.isSome
        then a |> Array.choose id |> FunnyList.ofArray |> List |> Some
        else None
    | _ -> None

  [ "init", toFunc2 init
  ] |> Map.ofList |> Record

let private typeOf id =
  Type { Id = id; Members = Map.empty }

let load () =
  [ Op Plus,  arith (+) (+)
    Op Minus, arith (-) (-)
    Op Mul,   arith (*) (*)
    Op Div,   arith (/) (/)
    Op Equal, toFunc2 (fun a b -> Some (if a = b then True else False))
    Op NotEq, toFunc2 (fun a b -> Some (if a = b then False else True))
    Op Less,      compare (<)  (<)
    Op LessEq,    compare (<=) (<=)
    Op Greater,   compare (>)  (>)
    Op GreaterEq, compare (>=) (>=)
    Op Is, toFunc2 (fun o t -> match t with Type t -> Some (if t.Id = typeid o then True else False) | _ -> None)

    Name "trace", toFunc1 trace
    Name "sin", toFunc1 sin

    Name "null",    typeOf NullType
    Name "bool",    typeOf BoolType
    Name "int",     typeOf IntType
    Name "float",   typeOf FloatType
    Name "string",  typeOf StrType
    Name "record",  typeOf RecordType
    Name "function",typeOf FuncType
    Name "list",    typeOf ListType
    Name "type",    typeOf TypeType

    Name "List", listModule
  ] |> Map.ofList

