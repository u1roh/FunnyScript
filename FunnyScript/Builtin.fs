module FunnyScript.Builtin
open System
open System.Collections

let private applyForce f = Obj.apply f >> Result.bind Obj.force
let private getOrRaise r = match r with Ok x -> x | Error e -> raiseErrInfo e

let private toFunc1 f = box (FuncObj.create (f >> Result.mapError ErrInfo.Create))
let private toFunc2 f = toFunc1 (f >> toFunc1 >> Ok)

module private FuncObj =
  let forArray f = FuncObj.create (Obj.toFunnyArray >> Result.map (f >> box))
  let forSeq   f = FuncObj.create (Obj.toSeq        >> Result.map (f >> box))


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
      | None, _ -> Error (TypeMismatch (ClrType typeof<float>, FunnyType.ofObj x))
      | _, None -> Error (TypeMismatch (ClrType typeof<float>, FunnyType.ofObj y))
  toFunc2 f

let private compare intf floatf =
  numOp (fun x y -> intf   x y |> box<bool>)
        (fun x y -> floatf x y |> box<bool>)

let private logical op =
  let f (x : obj) (y : obj) =
    let x = match x with :? bool as x -> Ok x | _ -> Error (TypeMismatch (ClrType typeof<bool>, FunnyType.ofObj x))
    let y = match y with :? bool as y -> Ok y | _ -> Error (TypeMismatch (ClrType typeof<bool>, FunnyType.ofObj y))
    match x, y with
    | Ok x, Ok y -> op x y |> box<bool> |> Ok
    | Error e, _ -> Error e
    | _, Error e -> Error e
  toFunc2 f

module private Cast =
  let rec toInt (x : obj) =
    match x with
    | :? int -> x
    | :? float as x -> int x |> box
    | :? bool  as x -> box (if x then 1 else 0)
    | :? string as x -> let ret, x = Int32.TryParse x in if ret then box x else null
    | :? IMutable as x -> toInt x.Value
    | _ -> null

  let rec toFloat (x : obj) =
    match x with
    | :? float -> x
    | :? float32 as x -> float x |> box
    | :? int as x -> float x |> box
    | :? string as x -> let ret, x = Double.TryParse x in if ret then box x else null
    | :? IMutable as x -> toFloat x.Value
    | _ -> null
  
let private asArray x =
  match x with
  | FunnyArray x -> Ok x
  | _ -> error (TypeMismatch (ClrType typeof<IFunnyArray>, FunnyType.ofObj x))

let private stdlib1 =
  [
    "+", FuncObj.ofList2 [
      FuncObj.create2 (fun x y ->
        NumOperands.Of (x, y) |> Result.map (function
          | IntOperands   (x, y) -> x + y |> box
          | FloatOperands (x, y) -> x + y |> box))
      FuncObj.ofFun2 (fun (x : string) (y : string) -> x + y)
      FuncObj.create2 (fun x y ->
        asArray x |> Result.bind (fun x ->
        asArray y |> Result.map  (fun y -> FunnyArray.append x y |> box)))
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
    ":?", FuncObj.ofFun2 (fun o (t : FunnyType) -> t = FunnyType.ofObj o) :> obj
    "~!", FuncObj.ofFun not :> obj
    "~+", FuncObj.ofList [FuncObj.ofFun (fun (x : int) -> +x); FuncObj.ofFun (fun (x : float) -> +x)] :> obj
    "~-", FuncObj.ofList [FuncObj.ofFun (fun (x : int) -> -x); FuncObj.ofFun (fun (x : float) -> -x)] :> obj
    //"::", toFunc2 (fun a ls -> match ls with List ls -> FunnyList.cons a ls |> AST.List |> Ok | _ -> Error (TypeMismatch (ListType, FunnyType.ofObj ls)))

    "class", FuncObj.create2 FunnyClass.create |> box
    "mutable", FuncObj.create (toMutable >> box >> Ok) :> obj
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
      | _ -> error (TypeMismatch (ClrType typeof<IFuncObj[]>, FunnyType.ofObj handlers))) |> box

    "extend", FuncObj.ofFun (function
      | ClrType t ->
        { new IFuncObj with
          member __.Apply (a, env) =
            match a with
            | :? Record as a -> env |> Env.extendType t a
            | x -> error (TypeMismatch (ClrType typeof<Record>, ClrType (x.GetType())))
        } :> obj
      | FunnyClass t -> FuncObj.ofFun (fun vtbl -> t |> FunnyClass.extend vtbl) :> obj
      ) :> obj

    "as",
      (let castFuncs =
        [ ClrType typeof<int>,    FuncObj.create (Cast.toInt   >> ok)
          ClrType typeof<float>,  FuncObj.create (Cast.toFloat >> ok)
        ] |> dict
      FuncObj.ofFun (fun (t : FunnyType) ->
        let contains, f = castFuncs.TryGetValue t
        if contains then f else FuncObj.create (fun _ -> Ok null)
      ) :> obj)

    "array", toFunc2 (fun len f ->
      let f = applyForce f >> getOrRaise
      match len with
      | :? int as len -> Array.init len f |> box |> Ok
      | _ -> Error (TypeMismatch (ClrType typeof<int>, FunnyType.ofObj len)))

    "isEmpty", FuncObj.ofList [
      FuncObj.ofFun (fun (a : ICollection) -> a.Count = 0)
      FuncObj.forSeq Seq.isEmpty
    ] :> obj

    "length", FuncObj.ofList [
      FuncObj.ofFun (fun (a : ICollection) -> a.Count)
      FuncObj.forSeq Seq.length
    ] :> obj

    "foreach", FuncObj.create2 (fun f -> Obj.cast<IEnumerable> >> Result.bind (fun src ->
        let f = applyForce f
        Seq.cast<obj> src
        |> Seq.tryPick (f >> function Error e -> Some (Error e) | _ -> None)
        |> Option.defaultValue (Ok null))) :> obj

    "map", FuncObj.create (fun f ->
      let f = applyForce f >> getOrRaise
      FuncObj.ofList [
        FuncObj.forArray (FunnyArray.map f)
        FuncObj.forSeq   (Seq.map f)
      ] |> ok) :> obj

    "choose", FuncObj.create (fun f ->
      let f = applyForce f >> function
        | Ok null -> None | Ok x -> Some x
        | Error { Err = Unmatched } -> None | Error e -> raiseErrInfo e
      FuncObj.ofList [
        FuncObj.forArray (FunnyArray.choose f)
        FuncObj.forSeq   (Seq.choose f)
      ] |> ok) :> obj

    "collect", FuncObj.create (fun f ->
      let f = applyForce f >> getOrRaise
      FuncObj.ofList [
        FuncObj.forArray (FunnyArray.collect (f >> Obj.toFunnyArray >> getOrRaise))
        FuncObj.forSeq   (Seq.collect (f >> Obj.toSeq >> getOrRaise))
      ] |> ok) :> obj

    "fold", FuncObj.create3 (fun acc0 f src ->
      let f acc x = acc |> Result.bind (applyForce f) |> Result.bind (fun f -> x |> applyForce f)
      src |> Obj.toSeq |> Result.bind (Seq.fold f (Ok acc0))) :> obj
      
    "filter", FuncObj.create (fun pred ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> getOrRaise
      FuncObj.ofList [
        FuncObj.forArray (FunnyArray.filter pred)
        FuncObj.forSeq   (Seq.filter pred)
      ] |> ok) :> obj

    "distinct", FuncObj.ofList [
        FuncObj.forArray FunnyArray.distinct
        FuncObj.forSeq   Seq.distinct
      ] :> obj

    "forall", FuncObj.create2 (fun pred src ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> getOrRaise
      src |> Obj.toSeq |> Result.map (Seq.forall pred >> box)) :> obj

    "exists", FuncObj.create2 (fun pred src ->
      let pred = applyForce pred >> Result.bind Obj.cast<bool> >> getOrRaise
      src |> Obj.toSeq |> Result.map (Seq.exists pred >> box)) :> obj

    "∪", FuncObj.create2 (fun a b ->
      a |> Obj.toFunnyArray |> Result.bind (fun a ->
      b |> Obj.toFunnyArray |> Result.map  (fun b ->
        FunnyArray.append a b |> Array.distinct |> box))) :> obj

    "∩", FuncObj.create2 (fun a b ->
      a |> Obj.toFunnyArray |> Result.bind (fun a ->
      b |> Obj.toFunnyArray |> Result.map  (fun b ->
        a |> FunnyArray.filter (fun x -> b |> Seq.cast<obj> |> Seq.exists ((=) x)) |> box))) :> obj

    "∈", FuncObj.ofList [
        FuncObj.ofFun2 (fun (set : IntInterval) (elm : int) -> set.contains elm)
        FuncObj.create (fun elm -> FuncObj.forSeq (Seq.exists ((=) elm)) |> ok)
      ] :> obj

    "record",   ClrType typeof<Record>    :> obj
    "function", ClrType typeof<IFuncObj>  :> obj
    "type",     ClrType typeof<FunnyType> :> obj

    // Map（FSharpMap） のコンストラクタ
    "Map", FuncObj.forSeq (Seq.choose (function
      | :? (obj[]) as a -> match a with [| (:? System.IComparable as key); value |] -> Some (key, value) | _ -> None
      | _ -> None) >> Map.ofSeq) :> obj

  ] |> List.map (fun (name, obj) -> name, Obj obj)

let private stdlib2 =
  """
  rec := f -> x -> f (rec f) x; // Yコンビネータ
  `∋` := set -> elm -> elm ∈ set;
  `in` := `∈`;
  Set := Microsoft.FSharp.Collections.FSharpSet System.Object;
  """
  |> Parser.parseModule "stdlib"
  |> function Ok lib -> lib | _ -> failwith "parse error in stdlib"

let load env =
  let env = env |> Env.add "unmatched" (error Unmatched)
  env |> Eval.recordEval (stdlib1 @ stdlib2)
  |> Result.map (fun r ->
    env |> Env.add "std" (Ok r) |> Eval.Env.openRecord (r :?> Record))
  |> function Ok env -> env | Error { Err = e } -> failwith (sprintf "error in stdlib: %A" e)
