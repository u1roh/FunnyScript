[<AutoOpen>]
module FunnyScript.AST

type Operator =
  | Plus
  | Minus
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

type Env = Map<ID, Obj>

and Error =
  | IdentifierNotFound of ID
  | NotApplyable of f:Obj * arg:Obj
  | TypeMismatch of expected:TypeId * actual:TypeId
  | NotImplemented of string
  | MiscError of string
  | ExnError of exn
  | ErrorList of Error list
  | NotMutable

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
  | Mutable of Ref<Obj>
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
  | Substitute of Expr * Expr

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
  | MutableType of TypeId
  | UserType of string
  | ClrType  of System.Type

and Type = {
    Id : TypeId
    Members : Map<string, Func>
  }

let rec typeid obj =
  match obj with
  | Null      -> NullType
  | True | False -> BoolType
  | Int     _ -> IntType
  | Float   _ -> FloatType
  | Record  _ -> RecordType
  | Func    _ -> FuncType
  | List    _ -> ListType
  | Mutable x -> MutableType (typeid !x)
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
      | List    x -> printf "[ "; x |> FunnyList.iter forObj; printf "]"
      | Mutable x -> printf "mutable "; forObj !x
      | ClrObj  x -> x.ToString() |> printf "%s"
      | Type    x -> printf "(Type %s)" (typeName x.Id)
      | Lazy    x -> printf "(Lazy %A)" x

    match expr with
    | Obj x -> forObj x
    | Ref x -> printf "%s" x
    | RefMember (expr, name) -> dump i expr; printf ".%s " name
    | BinaryOp (op, x1, x2) -> printf "("; dump i x1; printf " %A " op; dump i x2; printf ")"
    | UnaryOp  (op, x) -> printf "(%A" op; dump i x; printf ")"
    | Let (name, x1, x2) -> printf "\n%*s%s := " (2*i) " " name; dump (i + 1) x1; printf ";"; dump i x2
    | Combine   (x1, x2) -> printf "\n%*sdo " (2*i) " "; dump (i + 1) x1; printf ";"; dump i x2
    | Apply (f, p) -> printf "("; dump i f; printf " "; dump i p; printf ")";
    | FuncDef x -> printf "(%s) -> " (x.Args |> String.concat ", "); dump i x.Body
    | NewRecord x -> printf "{ "; x |> Seq.iter (fun (name, x) -> printf "\n%*s%s := " (2*i) " " name; dump (i + 1) x); printf " }"
    | NewList x -> printf "[ "; x |> Seq.iteri (fun j x -> (if j <> 0 then printf ", "); dump i x); printf " ]"
    | If (cond, x1, x2) -> printf "? "; dump i cond; printf " => "; dump i x1; printf " | "; dump i x2;
    | Substitute (x1, x2) -> dump i x1; printf " <- "; dump i x2