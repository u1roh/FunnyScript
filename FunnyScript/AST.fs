[<AutoOpen>]
module FunnyScript.AST
open System.Collections.Generic

type Env = private {
    Vals : Map<string, Result>
    ExtMembers : Dictionary<System.Type, Map<string, IFuncObj>>
  }

and Err =
  | IdentifierNotFound of string
  | NotApplyable of f:obj * arg:obj
  | TypeMismatch of expected:FunnyType * actual:FunnyType
  | Unmatched
  | NullReference
  | NotImplemented of string
  | ParseError of string
  | UserError of obj
  | MiscError of string
  | ExnError of exn
  | ErrorList of ErrInfo list
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

and Pattern =
  | Any
  | Tuple of Pattern list
  | Array of Pattern list * Pattern list option
  | Record of list<string * Pattern>
  | Typed of FunnyType
  | Case of string * Pattern
  | Named of string * Pattern
with
  static member Empty = Tuple[]
    
and PatternExpr =
  | XAny
  | XTuple of PatternExpr list
  | XArray of PatternExpr list * PatternExpr list option
  | XRecord of list<string * PatternExpr>
  | XTyped of Expr
  | XCase of string * PatternExpr
  | XNamed of string * PatternExpr
with
  static member Empty = XTuple[]

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
  | NewCase of PatternExpr option
  | Interval of IntervalBound * IntervalBound
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr
  | Substitute of Expr * Expr
  | Open of value:Expr * succ:Expr
  | Load of asm:string * succ:Expr
  | OnError of target:Expr * handler:Expr
  | Trace of Expr * Position

and FuncDef = {
    Args : PatternExpr
    Body : Expr
  }

and IntervalBound = {
    Expr   : Expr
    IsOpen : bool
  }

and FunnyType =
  | ClrType  of System.Type
  | FunnyClass of FunnyClass

and FunnyClass = {
    Ctor : IFuncObj
    Members : Map<string, IFuncObj>
    mutable ExtMembers : Map<string, IFuncObj>
  }

type Instance = {
    Data : obj
    Type : FunnyClass
  }

type Case (pat : Pattern) =
  new () = Case Pattern.Empty
  member __.Pattern = pat

type CaseValue = CaseValue of Case * obj

type ErrInfoException (e : ErrInfo) =
  inherit exn()
  member val ErrInfo = e

let raiseErrInfo e = raise (ErrInfoException e)

let ok x = Ok (box x)
let error e = Error { Err = e; StackTrace = [] }


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
  let ofObj x =
    if x = null then error NullReference else Ok x


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Env =
  let empty =
    { Vals = Map.empty; ExtMembers = Dictionary() }

  let add name value env =
    { env with Vals = env.Vals |> Map.add name value }

  let tryFind name env =
    env.Vals |> Map.tryFind name

  let get id (env : Env) =
    env.Vals |> Map.tryFind id |> function Some x -> x | _ -> error (IdentifierNotFound id)

  let tryGet id (env : Env) =
    env.Vals |> Map.tryFind id |> Option.bind Result.toOption

  let findExtMember (t : System.Type) name (env : Env) =
    let found, members = env.ExtMembers.TryGetValue t
    if found then members |> Map.tryFind name else None

  let extendType (t : System.Type) (vtbl : Record) (env : Env) =
    let members, invalids = vtbl |> Map.partition (fun _ m -> match m with :? IFuncObj -> true | _ -> false)
    if not invalids.IsEmpty then error ClassDefError else
      let members = members |> Map.map (fun _ m -> m :?> IFuncObj)
      let extMems =
        let found, extMems = env.ExtMembers.TryGetValue t
        if found then extMems else Map.empty
      env.ExtMembers.[t] <- (extMems, members) ||> Map.fold (fun vtbl name m -> vtbl |> Map.add name m)
      Ok null
  

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

  let invoke (f : IFuncObj) arg = f.Apply (arg, Env.empty)

  let invoke2 f arg1 arg2 =
    invoke f arg1
    |> Result.bind (function :? IFuncObj as f -> Ok f | f -> error (NotApplyable (f, arg2)))
    |> Result.bind (fun f -> invoke f arg2)

  let ofList flist =
    let rec execute errors flist arg =
      match flist with
      | f :: flist -> match invoke f arg with Error e -> execute (e.Err :: errors) flist arg | x -> x
      | [] -> errors |> List.map ErrInfo.Create |> ErrorList |> error
    create (execute [] flist)

  let ofList2 flist =
    let rec execute errors flist arg1 arg2 =
      match flist with
      | f :: flist -> match invoke2 f arg1 arg2 with Error e -> execute (e.Err :: errors) flist arg1 arg2 | x -> x
      | [] -> errors |> List.map ErrInfo.Create |> ErrorList |> error
    create2 (execute [] flist)


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FunnyType =
  let create ctor members =
    FunnyClass { Ctor = ctor; Members = members; ExtMembers = Map.empty }

  let ofObj (obj : obj) =
    match obj with
    | :? Instance as obj -> FunnyClass obj.Type
    | :? IFuncObj as f -> ClrType typeof<IFuncObj>
    | :? FunnyType as t -> ClrType typeof<FunnyType>
    | null -> ClrType typeof<unit>
    | _ -> ClrType (obj.GetType())


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FunnyClass =
  let extend (vtbl : Record) t =
    let members, invalids = vtbl |> Map.partition (fun _ m -> match m with :? IFuncObj -> true | _ -> false)
    if not invalids.IsEmpty then error ClassDefError else
      let members = members |> Map.map (fun _ m -> m :?> IFuncObj)
      t.ExtMembers <- (t.ExtMembers, members) ||> Map.fold (fun vtbl name m -> vtbl |> Map.add name m)
      Ok null

  let create (ctor : obj) (vtbl : obj) =
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
      box (FunnyType.create ctor members))


let toMutable obj =
  let mutable obj = obj
  { new IMutable with member __.Value with get() = obj and set x = obj <- x }
