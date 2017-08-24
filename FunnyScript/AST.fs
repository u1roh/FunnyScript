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

type Operator =
  | Plus
  | Minus
  | UnaryPlus
  | UnaryMinus
  | Mul
  | Div
  | Mod
  | Equal
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LogicalNot
  | LogicalAnd
  | LogicalOr
  | Is
  | Cons

type ID =
  | Name of string
  | Op   of Operator

type ApplType =
  | NormalApply
  | Pipeline
  | NullPropagationPipeline

type Env = Map<ID, Result>

and Error =
  | IdentifierNotFound of ID
  | NotApplyable of f:Obj * arg:Obj
  | TypeMismatch of expected:TypeId * actual:TypeId
  | NotImplemented of string
  | UserError of Obj
  | MiscError of string
  | ExnError of exn
  | ErrorList of Err list
  | NotMutable
  | ClassDefError

and Err = Trace<Error>

and Result = Result<Obj, Err>

and IMutable =
  abstract Value : Obj with get, set

and Instance = {
    Data : Obj
    Type : Type
  }

and Obj =
  | Null
  | True
  | False
  | Int     of int
  | Float   of float
  | Record  of Map<string, Obj>
  | Func    of Func
  | Mutable of IMutable
  | Instance of Instance
  | ClrObj  of obj
  | Type    of Type
  | Lazy    of Lazy<Result> // for tail-recursion

and Expression =
  | Obj of Obj
  | Ref of name:string
  | RefMember of self:Expr * name:string
  | BinaryOp of Operator * Expr * Expr
  | UnaryOp  of Operator * Expr
  | Let of name:string * value:Expr * succ:Expr
  | Combine of Expr * Expr
  | Apply of func:Expr * para:Expr * ApplType
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
  abstract Apply : Obj -> Result

and Func =
  | UserFunc    of UserFunc
  | BuiltinFunc of IFuncObj

and TypeId =
  | NullType
  | BoolType
  | IntType
  | FloatType
  | RecordType
  | FuncType
  | ListType
  | TypeType
  | LazyType
  | UserType of string * Func
  | ClrType  of System.Type

and Type = {
    Id : TypeId
    Members : Map<string, Func>
  }

let makeClass ctor vtbl =
  match ctor, vtbl with
    | Func ctor, Record vtbl -> Ok (ctor, vtbl)
    | _ -> Error ClassDefError
  |> Result.bind (fun (ctor, vtbl) ->
    let members, invalids = vtbl |> Map.partition (fun _ m -> match m with Func _ -> true | _ -> false)
    if invalids.IsEmpty
      then Ok (ctor, members)
      else Error ClassDefError)
  |> Result.map (fun (ctor, members) ->
    let members = members |> Map.map (fun _ m -> match m with Func m -> m | _ -> failwith "fatal error")
    Type { Id = UserType ("", ctor); Members = members })


let rec typeid obj =
  match obj with
  | Null      -> NullType
  | True | False -> BoolType
  | Int     _ -> IntType
  | Float   _ -> FloatType
  | Record  _ -> RecordType
  | Func    _ -> FuncType
  | Mutable x -> typeid x.Value
  | Instance x -> x.Type.Id
  | ClrObj  x -> ClrType (x.GetType())
  | Type    _ -> TypeType
  | Lazy    _ -> LazyType

let typeName id =
  match id with
  | NullType    -> "null"
  | BoolType    -> "bool"
  | IntType     -> "int"
  | FloatType   -> "float"
  | RecordType  -> "record"
  | FuncType    -> "function"
  | TypeType    -> "type"
  | LazyType    -> "lazy"
  | UserType (x, _) -> x
  | ClrType t   -> t.FullName

let toMutable obj =
  let mutable obj = obj
  Mutable { new IMutable with member __.Value with get() = obj and set x = obj <- x }

module DebugDump =

  let rec dump i expr =
    let rec forObj obj =
      match obj with
      | Null  -> printf "null"
      | True  -> printf "true"
      | False -> printf "false"
      | Int     x -> printf "%d" x
      | Float   x -> printf "%f" x
      | Record  x ->
        printf "{ "
        x |> Map.toSeq |> Seq.iter (fun (name, obj) -> printf "%s = " name; forObj obj; printf "; ")
        printf " }"
      | Func (UserFunc x) -> printf "(%s) -> " (x.Def.Args |> String.concat ", "); dump i x.Def.Body
      | Func (BuiltinFunc x) -> printf "(builtin-func)"
      | Mutable x -> printf "mutable "; forObj x.Value
      | Instance x -> printf "instance of %A" x.Type.Id; forObj x.Data
      | ClrObj  x -> x.ToString() |> printf "%s"
      | Type    x -> printf "(Type %s)" (typeName x.Id)
      | Lazy    x -> printf "(Lazy %A)" x

    match expr.Value with
    | Obj x -> forObj x
    | Ref x -> printf "%s" x
    | RefMember (expr, name) -> dump i expr; printf ".%s " name
    | BinaryOp (op, x1, x2) -> printf "("; dump i x1; printf " %A " op; dump i x2; printf ")"
    | UnaryOp  (op, x) -> printf "(%A" op; dump i x; printf ")"
    | Let (name, x1, x2) -> printf "\n%*s%s := " (2*i) " " name; dump (i + 1) x1; printf ";"; dump i x2
    | Combine   (x1, x2) -> printf "\n%*sdo " (2*i) " "; dump (i + 1) x1; printf ";"; dump i x2
    | Apply (f, p, _) -> printf "("; dump i f; printf " "; dump i p; printf ")";
    | FuncDef x -> printf "(%s) -> " (x.Args |> String.concat ", "); dump i x.Body
    | NewRecord x -> printf "{ "; x |> Seq.iter (fun (name, x) -> printf "\n%*s%s := " (2*i) " " name; dump (i + 1) x); printf " }"
    | NewList x -> printf "[ "; x |> Seq.iteri (fun j x -> (if j <> 0 then printf ", "); dump i x); printf " ]"
    | If (cond, x1, x2) -> printf "? "; dump i cond; printf " => "; dump i x1; printf " | "; dump i x2;
    | Substitute (x1, x2) -> dump i x1; printf " <- "; dump i x2