namespace FunnyScript
open System
open System.Collections

type Err =
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
    StackTrace : Position list
  } with
  static member Create err = { Err = err; StackTrace = [] }

and Result = Result<obj, ErrInfo>
and Env = Map<string, Result>

and IFuncObj =
  abstract Apply : obj * Env -> Result

and TypeId =
  | UserType of ctor:IFuncObj * members:Map<string, IFuncObj>
  | ClrType  of System.Type

and FunnyType = {
    Id : TypeId
    mutable ExtMembers : Map<string, IFuncObj>
  }

type Instance = {
    Data : obj
    Type : FunnyType
  }

