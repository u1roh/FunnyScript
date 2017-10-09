[<AutoOpen>]
module FunnyScript.AST

type Env = Map<string, Result>

and Err =
  | IdentifierNotFound of string
  | NotApplyable of f:obj * arg:obj
  | TypeMismatch of expected:TypeId * actual:TypeId
  | Unmatched
  | NotImplemented of string
  | ParseError of string
  | UserError of obj
  | MiscError of string
  | ExnError of exn
  | ErrorList of Err list
  | NotMutable
  | ClassDefError

and ErrInfo = {
    Err : Err
    //StackTrace : (Expr * Env) list
    StackTrace : Position list
  } with
  static member Create err = { Err = err; StackTrace = [] }

and Result = Result<obj, ErrInfo>

and IFuncObj =
  abstract Apply : obj * Env -> Result

and Instance = {
    Data : obj
    Type : FunnyType
  }

and Expr =
  | Obj of obj
  | Ref of name:string
  | RefMember of self:Expr * name:string
  | Let of name:string * value:Expr * succ:Expr
  | Apply of func:Expr * para:Expr
  | LogicalAnd of Expr * Expr
  | LogicalOr  of Expr * Expr
  | FuncDef of FuncDef
  | NewRecord of (string * Expr) list
  | NewArray of Expr[]
  | Interval of IntervalBound * IntervalBound
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr
  | Substitute of Expr * Expr
  | Open of value:Expr * succ:Expr
  | Load of asm:string * succ:Expr
  | OnError of target:Expr * handler:Expr
  | Trace of Expr * Position

and Pattern =
  | Identifier of string
  | Tuple of Pattern list
  | Record of list<string * Pattern>

and FuncDef = {
    Args : Pattern
    Body : Expr
  }

and IntervalBound = {
    Expr   : Expr
    IsOpen : bool
  }

and TypeId =
  | UserType of ctor:IFuncObj * members:Map<string, IFuncObj>
  | ClrType  of System.Type

and FunnyType = {
    Id : TypeId
    mutable ExtMembers : Map<string, IFuncObj>
  }

type private ErrInfoException (e : ErrInfo) =
  inherit exn()
  member val ErrInfo = e

let raiseErrInfo e = raise (ErrInfoException e)

let error e =
  Error { Err = e; StackTrace = [] }


module FuncObj =
  let create f = {
    new IFuncObj with
      member __.Apply (a, _) =
        try f a with
        | :? ErrInfoException as e -> Error e.ErrInfo
        | e -> error (ExnError e)
  }

  let create2 f = create (f >> create  >> box >> Ok)
  let create3 f = create (f >> create2 >> box >> Ok)

  let ofFun (f : 'a -> 'r) =
    create (function
      | :? 'a as a -> f a |> box |> Ok
      | x -> error (TypeMismatch (ClrType typeof<'a>, ClrType (x.GetType()))))

  let ofFun2 f = ofFun (f >> ofFun)
  let ofFun3 f = ofFun (f >> ofFun2)

  let invoke (f : obj) arg =
    match f with
    | :? IFuncObj as f -> f.Apply (arg, Map.empty)
    | _ -> error (NotApplyable (f, arg))

  let invoke2 f arg1 arg2 =
    invoke f arg1 |> Result.bind (fun f -> invoke f arg2)

  let ofList flist =
    let rec execute errors flist arg =
      match flist with
      | f :: flist -> match invoke f arg with Error e -> execute (e.Err :: errors) flist arg | x -> x
      | [] -> error (ErrorList errors)
    create (execute [] flist)

  let ofList2 flist =
    let rec execute errors flist arg1 arg2 =
      match flist with
      | f :: flist -> match invoke2 f arg1 arg2 with Error e -> execute (e.Err :: errors) flist arg1 arg2 | x -> x
      | [] -> error (ErrorList errors)
    create2 (execute [] flist)


module Type =
  let create ctor members =
    { Id = UserType (ctor, members); ExtMembers = Map.empty }

  let extend members (t : FunnyType) =
    t.ExtMembers <- (t.ExtMembers, members) ||> Map.fold (fun vtbl name m -> vtbl |> Map.add name m)


let makeClass (ctor : obj) (vtbl : obj) =
  match ctor, vtbl with
    | (:? IFuncObj as ctor), (:? Record as vtbl) -> Ok (ctor, vtbl)
    | _ -> error ClassDefError
  |> Result.bind (fun (ctor, vtbl) ->
    let members, invalids = vtbl |> Map.partition (fun _ m -> match m with :? IFuncObj -> true | _ -> false)
    if invalids.IsEmpty
      then Ok (ctor, members)
      else error ClassDefError)
  |> Result.map (fun (ctor, members) ->
    let members = members |> Map.map (fun _ m -> match m with :? IFuncObj as m -> m | _ -> failwith "fatal error")
    box (Type.create ctor members))

let extendType (t : FunnyType) (vtbl : Record) =
  let members, invalids = vtbl |> Map.partition (fun _ m -> match m with :? IFuncObj -> true | _ -> false)
  if not invalids.IsEmpty then error ClassDefError else
    t |> Type.extend (members |> Map.map (fun _ m -> m :?> IFuncObj))
    Ok null

let rec typeid (obj : obj) =
  match obj with
  | :? Instance as obj -> obj.Type.Id
  | :? IFuncObj as f -> ClrType typeof<IFuncObj>
  | null -> ClrType typeof<unit>
  | _ -> ClrType (obj.GetType())

let toMutable obj =
  let mutable obj = obj
  { new IMutable with member __.Value with get() = obj and set x = obj <- x }
