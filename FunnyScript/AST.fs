[<AutoOpen>]
module FunnyScript.AST

type Position = {
    FilePath  : string
    Line      : int
    Column    : int
  } with
  override this.ToString () = sprintf "%s(%d, %d)" this.FilePath this.Line this.Column

type Trace<'value> = {
    Value : 'value
    Position : Position option
  }

type Env = Map<string, Result>

and Error =
  | IdentifierNotFound of string
  | NotApplyable of f:obj * arg:obj
  | TypeMismatch of expected:TypeId * actual:TypeId
  | NotImplemented of string
  | UserError of obj
  | MiscError of string
  | ExnError of exn
  | ErrorList of Err list
  | NotMutable
  | ClassDefError

and Err = Trace<Error>

and Result = Result<obj, Err>

and IMutable =
  abstract Value : obj with get, set

and Instance = {
    Data : obj
    Type : Type
  }

and Record = Map<string, obj>

and Expression =
  | Obj of obj
  | Ref of name:string
  | RefMember of self:Expr * name:string
  | Let of name:string * value:Expr * succ:Expr
  | Combine of Expr * Expr
  | Apply of func:Expr * para:Expr
  | FuncDef of FuncDef
  | NewRecord of (string * Expr) list
  | NewList of Expr[]
  | ListByRange of Expr * Expr
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr
  | Substitute of Expr * Expr
  | Open of value:Expr * succ:Expr
  | OnError of target:Expr * handler:Expr

and Expr = Trace<Expression>

and FuncDef = {
    Args : string list
    Body : Expr
  }

and UserFunc = {
    Def : FuncDef
    mutable Env : Env
  }

and IFuncObj =
  abstract Apply : obj -> Result

and Func =
  | UserFunc    of UserFunc
  | BuiltinFunc of IFuncObj

and TypeId =
  | UserType of string * Func
  | ClrType  of System.Type

and Type = {
    Id : TypeId
    Members : Map<string, Func>
  }

let makeClass (ctor : obj) (vtbl : obj) =
  match ctor, vtbl with
    | (:? Func as ctor), (:? Record as vtbl) -> Ok (ctor, vtbl)
    | _ -> Error ClassDefError
  |> Result.bind (fun (ctor, vtbl) ->
    let members, invalids = vtbl |> Map.partition (fun _ m -> match m with :? Func -> true | _ -> false)
    if invalids.IsEmpty
      then Ok (ctor, members)
      else Error ClassDefError)
  |> Result.map (fun (ctor, members) ->
    let members = members |> Map.map (fun _ m -> match m with :? Func as m -> m | _ -> failwith "fatal error")
    box { Id = UserType ("", ctor); Members = members })


let rec typeid (obj : obj) =
  match obj with
  | :? Instance as obj -> obj.Type.Id
  | :? Func as f -> ClrType typeof<Func>
  | null -> ClrType typeof<unit>
  | _ -> ClrType (obj.GetType())

let typeName id =
  match id with
  | UserType (x, _) -> x
  | ClrType t ->
    if t = typeof<int>    then "int"      else
    if t = typeof<float>  then "float"    else
    if t = typeof<bool>   then "bool"     else
    if t = typeof<Func>   then "function" else
    if t = typeof<Record> then "record"   else
    if t = typeof<Type>   then "type"     else
    t.FullName

let toMutable obj =
  let mutable obj = obj
  { new IMutable with member __.Value with get() = obj and set x = obj <- x }
