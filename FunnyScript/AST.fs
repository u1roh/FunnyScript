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

type ID =
  | Name of string
  | Op   of Operator

type Env = Map<ID, Obj>

and Obj =
  | Null
  | True
  | False
  | Int     of int
  | Float   of float
  | Str     of string
  | Record  of Map<string, Obj>
  | Func    of Func
  | List    of IFunnyList<Obj>
  | ClrObj  of obj
  | Type    of Type
  | Members of MemberTable
  | Lazy    of Lazy<Obj option> // for tail-recursion

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
    Arg  : string
    Body : Expr
  }

and IBuiltinFunc =
  abstract Apply : Obj -> Obj option

and UserFunc = {
    Def : FuncDef
    mutable Env : Env
  }

and Func =
  | UserFunc    of UserFunc
  | BuiltinFunc of IBuiltinFunc

and Type =
  | NullType
  | BoolType
  | IntType
  | FloatType
  | StrType
  | RecordType
  | FuncType
  | ListType
  | TypeType
  | MemberTableType
  | LazyType
  | UserType of string
  | ClrType  of System.Type

and MemberTable = Map<string, Func>

let typeof obj =
  match obj with
  | Null      -> NullType
  | True | False -> BoolType
  | Int     _ -> IntType
  | Float   _ -> FloatType
  | Str     _ -> StrType
  | Record  _ -> RecordType
  | Func    _ -> FuncType
  | List    _ -> ListType
  | ClrObj  x -> ClrType (x.GetType())
  | Type    _ -> TypeType
  | Members _ -> MemberTableType
  | Lazy    _ -> LazyType
