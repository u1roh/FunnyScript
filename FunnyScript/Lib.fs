module FunnyScript.Lib
open AST

let private toFunc1 f = Func (BuiltinFunc { new IBuiltinFunc with member __.Apply a = f a })
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Some)
let private toFunc3 f = toFunc1 (f >> toFunc2 >> Some)

let private add x y =
    match x, y with
    | Int   x, Int   y -> Int   (x + y) |> Some
    | Float x, Float y -> Float (x + y) |> Some
    | _ -> None

let private sub x y =
    match x, y with
    | Int   x, Int   y -> Int   (x - y) |> Some
    | Float x, Float y -> Float (x - y) |> Some
    | _ -> None

let private mul x y =
  match x, y with
  | Int   x, Int   y -> Int   (x * y) |> Some
  | Float x, Float y -> Float (x * y) |> Some
  | _ -> None

let private div x y =
  match x, y with
  | Int   x, Int   y -> Int   (x / y) |> Some
  | Float x, Float y -> Float (x / y) |> Some
  | _ -> None

let private sin x =
  match x with
  | Float x -> Float (sin x) |> Some
  | _ -> None

let private trace arg =
  printfn "%A" arg; Some Null

let load () =
  [ Name "+", toFunc2 add
    Name "trace", toFunc1 trace
    Op Plus,  toFunc2 add
    Op Minus, toFunc2 sub
    Op Mul,   toFunc2 mul
    Op Div,   toFunc2 div
    Op Equal, toFunc2 (fun a b -> Some (if a = b then True else False))
    Name "sin", toFunc1 sin
  ] |> Map.ofList

let eval expr =
  load() |> Eval.eval expr
