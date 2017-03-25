module FunnyScript.Lib

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

    Name "trace", toFunc1 trace
    Name "sin", toFunc1 sin

    Name "typeof", toFunc1 (typeof >> Type >> Some)
    Name "null",    Type NullType
    Name "bool",    Type BoolType
    Name "int",     Type IntType
    Name "float",   Type FloatType
    Name "string",  Type StrType
    Name "record",  Type RecordType
    Name "function",Type FuncType
    Name "list",    Type ListType
    Name "type",    Type TypeType
  ] |> Map.ofList

