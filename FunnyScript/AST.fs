module FunnyScript.AST

type FuncDef = {
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

and Operator =
  | Plus
  | Minus
  | Mul
  | Div
  | Equal
  | NotEqual
  | Not

and Obj =
  | Null
  | True
  | False
  | Int     of int
  | Float   of float
  | String  of string
  | Tuple   of Obj[]
  | Record  of Map<string, Obj>
  | Func    of Func

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
  | NewTuple of Expr[]
  | NewRecord of (string * Expr)[]
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr

and ID =
  | Name of string
  | Op   of Operator

and Env = Map<ID, Obj>
