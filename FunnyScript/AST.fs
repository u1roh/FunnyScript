[<AutoOpen>]
module FunnyScript.AST

type Operator =
  | Plus
  | Minus
  | Mul
  | Div
  | Equal
  | NotEq
  | Not
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Is
  | Cons

type ID =
  | Name of string
  | Op   of Operator

type Env = Map<ID, Obj>

and Error =
  | IdentifierNotFound of ID
  | NotApplyable of f:Obj * arg:Obj
  | TypeMismatch of expected:TypeId * actual:TypeId
  | NotImplemented of string
  | MiscError of string
  | ExnError of exn
  | ErrorList of Error list

and Result = Result<Obj, Error>

and Obj =
  | Null
  | True
  | False
  | Int     of int
  | Float   of float
  | Record  of Map<string, Obj>
  | Func    of Func
  | List    of IFunnyList<Obj>
  | ClrObj  of obj
  | Type    of Type
  | Lazy    of Lazy<Result> // for tail-recursion

and Expr =
  | Obj of Obj
  | Ref of name:string
  | RefMember of self:Expr * name:string
  | BinaryOp of Operator * Expr * Expr
  | UnaryOp  of Operator * Expr
  | Let of name:string * value:Expr * succ:Expr
  | Combine of Expr * Expr
  | Apply of func:Expr * para:Expr
  | FuncDef of FuncDef
  | NewRecord of (string * Expr) list
  | NewList of Expr[]
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr

and FuncDef = {
    Args : string list
    Body : Expr
  }

and IBuiltinFunc =
  abstract Apply : Obj -> Result

and UserFunc = {
    Def : FuncDef
    mutable Env : Env
  }

and Func =
  | UserFunc    of UserFunc
  | BuiltinFunc of IBuiltinFunc

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
  | UserType of string
  | ClrType  of System.Type

and Type = {
    Id : TypeId
    Members : Map<string, Func>
  }

let typeid obj =
  match obj with
  | Null      -> NullType
  | True | False -> BoolType
  | Int     _ -> IntType
  | Float   _ -> FloatType
  | Record  _ -> RecordType
  | Func    _ -> FuncType
  | List    _ -> ListType
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
  | ListType    -> "list"
  | TypeType    -> "type"
  | LazyType    -> "lazy"
  | UserType x  -> x
  | ClrType t   -> t.FullName

let ofArray src = List (FunnyList.ofArray src)
let ofList  src = List (FunnyList.ofList  src)
