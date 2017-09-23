namespace FunnyScript
open System


type Position = {
    FilePath  : string
    LineCol1  : int * int
    LineCol2  : int * int
  } with
  override this.ToString () =
    sprintf "%s %A~%A" this.FilePath this.LineCol1 this.LineCol2

type IFunnyArray =
  inherit System.Collections.IEnumerable
  abstract Length : int
  abstract Item : int -> obj with get

type IntInterval = { min : int; max : int } with
  member this.contains n = this.min <= n && n <= this.max
  interface IFunnyArray with
    member this.Length = this.max - this.min + 1 |> max 0
    member this.Item with get i = this.min + i |> box
    member this.GetEnumerator () = (seq { this.min .. this.max } :> System.Collections.IEnumerable).GetEnumerator()


type IMutable =
  abstract Value : obj with get, set

type Record = Map<string, obj>

// 下記はまだ迷っているのでコメントアウト

//type Result<'TError> = Result<obj, 'TError>
//
//type IFuncObj<'E> =
//  abstract Apply : obj -> Result<'E>
//
//and TypeId<'E> =
//  | UserType of name:string * ctor:IFuncObj<'E> * members:Map<string, IFuncObj<'E>>
//  | ClrType  of System.Type
//
//and FunnyType<'E> = {
//    Id : TypeId<'E>
//    mutable ExtMembers : Map<string, IFuncObj<'E>>
//  }
//
//type Instance<'E> = {
//    Data : obj
//    Type : FunnyType<'E>
//  }

