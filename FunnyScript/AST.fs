﻿[<AutoOpen>]
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
  | NotEq
  | Not
  | Less
  | LessEq
  | Greater
  | GreaterEq

and Obj =
  | Null
  | True
  | False
  | Int     of int
  | Float   of float
  | Str     of string
  | Tuple   of Obj[]
  | Record  of Map<string, Obj>
  | Func    of Func
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
  | NewTuple of Expr[]
  | NewRecord of (string * Expr) list
  | If of condition:Expr * thenExpr:Expr * elseExpr:Expr

and ID =
  | Name of string
  | Op   of Operator

and Env = Map<ID, Obj>
