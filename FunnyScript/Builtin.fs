module FunnyScript.Builtin
open System
open System.Collections

let private applyForce f = Obj.apply f >> Result.bind Obj.force

let private toFunc1 f = box (FuncObj.create (f >> Result.mapError ErrInfo.Create))
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Ok)

type private NumOperands =
  | IntOperands of int * int
  | FloatOperands of float * float
  static member Of (x : obj, y : obj) =
    let error = error (MiscError "not numeric")
    match x with
    | :? int as x ->
      match y with
      | :? int   as y -> IntOperands (x, y) |> Ok
      | :? float as y -> FloatOperands (float x, y) |> Ok
      | _ -> error
    | :? float as x ->
      match y with
      | :? int   as y -> FloatOperands (x, float y) |> Ok
      | :? float as y -> FloatOperands (x, y) |> Ok
      | _ -> error
    | _ -> error
      

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

let private stdlib1 =
  [
    "+", FuncObj.ofList2 [
      FuncObj.create2 (fun x y ->
        NumOperands.Of (x, y) |> Result.map (function
          | IntOperands   (x, y) -> x + y |> box
          | FloatOperands (x, y) -> x + y |> box))
      FuncObj.ofFun2 (fun (x : string) (y : string) -> x + y)
      CLR.createOperatorFuncObj "op_Addition"
    ] :> obj

    "-", FuncObj.ofList2 [
      FuncObj.create2 (fun x y ->
        NumOperands.Of (x, y) |> Result.map (function
          | IntOperands   (x, y) -> x - y |> box
          | FloatOperands (x, y) -> x - y |> box))
      CLR.createOperatorFuncObj "op_Subtraction"
    ] :> obj

    "*", FuncObj.ofList2 [
      FuncObj.create2 (fun x y ->
        NumOperands.Of (x, y) |> Result.map (function
          | IntOperands   (x, y) -> x * y |> box
          | FloatOperands (x, y) -> x * y |> box))
      CLR.createOperatorFuncObj "op_Multiply"
    ] :> obj

    "/", FuncObj.ofList2 [
      FuncObj.create2 (fun x y ->
        NumOperands.Of (x, y) |> Result.map (function
          | IntOperands   (x, y) -> x / y |> box
          | FloatOperands (x, y) -> x / y |> box))
      CLR.createOperatorFuncObj "op_Division"
    ] :> obj

    "%",  FuncObj.ofFun2 (fun a b -> a % b) :> obj
    "==", FuncObj.create2 (fun a b -> (a =  b) |> box |> Ok) :> obj
    "!=", FuncObj.create2 (fun a b -> (a <> b) |> box |> Ok) :> obj
    "<",  compare (<)  (<)
    "<=", compare (<=) (<=)
    ">",  compare (>)  (>)
    ">=", compare (>=) (>=)
    "|>",  FuncObj.create2 (fun arg f -> Obj.apply f arg) |> box
    "|?>", FuncObj.create2 (fun arg f -> if arg = null then Ok null else Obj.apply f arg) |> box
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

    "eval",
    { new IFuncObj with
      member __.Apply (arg, env) =
        match arg with
        | :? string as script ->
          Parser.parse "" script
          |> Result.mapError (ParseError >> ErrInfo.Create)
          |> Result.bind (fun expr -> env |> Eval.eval expr)
        | x -> error (TypeMismatch (ClrType typeof<string>, ClrType (x.GetType())))
    } :> obj

    "match", FuncObj.create2 (fun handlers x ->
      match handlers with
      | :? (obj[]) as handlers ->
        handlers |> Array.tryPick (fun f ->
          match applyForce f x with
          | Error { Err = Unmatched } -> None
          | result -> Some result)
        |> Option.defaultValue (error Unmatched)
      | _ -> error (TypeMismatch (ClrType typeof<IFuncObj[]>, typeid handlers))) |> box

    "extend", FuncObj.ofFun2 extendType :> obj

    "array", toFunc2 (fun len f ->
      let f = applyForce f >> function Ok x -> x | Error e -> raiseErrInfo e
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

    "foreach", FuncObj.create2 (fun f -> Obj.cast<IEnumerable> >> Result.bind (fun src ->
        let f = applyForce f
        Seq.cast<obj> src
        |> Seq.tryPick (f >> function Error e -> Some (Error e) | _ -> None)
        |> Option.defaultValue (Ok null))) :> obj

    "map", toFunc2 (fun f src ->
      let f = applyForce f >> function Ok x -> x | Error e -> raiseErrInfo e
      match src with
      | FunnyArray src -> src |> FunnyArray.map f |> box |> Ok
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.map f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "choose", toFunc2 (fun f src ->
      let f = applyForce f >> function
        | Ok null -> None | Ok x -> Some x
        | Error { Err = Unmatched } -> None | Error e -> raiseErrInfo e
      match src with
      | FunnyArray src -> src |> FunnyArray.choose f |> box |> Ok
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.choose f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src)))

    "fold", FuncObj.create3 (fun acc0 f src ->
      let f acc x = acc |> Result.bind (applyForce f) |> Result.bind (fun f -> x |> applyForce f)
      match src with
      | (:? IEnumerable as src) -> Seq.cast<obj> src |> Seq.fold f (Ok acc0)
      | _ -> error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src))) |> box
      
    "filter", FuncObj.create2 (fun pred src ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> function Ok x -> x | Error e -> raiseErrInfo e
      match src with
      | FunnyArray src -> src |> FunnyArray.filter pred |> box |> Ok
      | :? IEnumerable as src -> Seq.cast<obj> src |> Seq.filter pred |> box |> Ok
      | _ -> error (TypeMismatch (ClrType typeof<IEnumerable>, typeid src))) :> obj

    "forall", FuncObj.create2 (fun pred src ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> function Ok x -> x | Error e -> raiseErrInfo e
      src |> Obj.cast<IEnumerable> |> Result.map (Seq.cast<obj> >> Seq.forall pred >> box)) :> obj

    "exists", FuncObj.create2 (fun pred src ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> function Ok x -> x | Error e -> raiseErrInfo e
      src |> Obj.cast<IEnumerable> |> Result.map (Seq.cast<obj> >> Seq.exists pred >> box)) :> obj

    "record",   { Id = ClrType typeof<Record>;    ExtMembers = Map.empty } :> obj
    "function", { Id = ClrType typeof<IFuncObj>;  ExtMembers = Map.empty } :> obj
    "type",     { Id = ClrType typeof<FunnyType>; ExtMembers = Map.empty } :> obj

    "Cast", castModule
//    "List", listModule

  ] |> List.map (fun (name, obj) -> name, Obj obj)

let private stdlib2 =
  """
  rec := f -> x -> f (rec f) x; // Yコンビネータ
  """
  |> Parser.parseModule "stdlib"
  |> function Ok lib -> lib | _ -> failwith "parse error in stdlib"

let load env =
  let env = env |> Map.add "unmatched" (error Unmatched)
  env |> Eval.recordEval (stdlib1 @ stdlib2)
  |> Result.map (fun r ->
    env |> Map.add "std" (Ok r) |> Eval.Env.openRecord (r :?> Record))
  |> function Ok env -> env | Error { Err = e } -> failwith (sprintf "error in stdlib: %A" e)
