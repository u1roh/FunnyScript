namespace FunnyScript
open System
open System.Collections


type Position = {
    FilePath  : string
    LineCol1  : int * int
    LineCol2  : int * int
  } with
  override this.ToString () =
    sprintf "%s %A~%A" this.FilePath this.LineCol1 this.LineCol2

type IFunnyArray =
  inherit ICollection
  abstract Item : int -> obj with get

type IntInterval = { min : int; max : int } with
  member this.contains n = this.min <= n && n <= this.max
  interface IFunnyArray with
    member this.Count = this.max - this.min + 1 |> max 0
    member this.Item with get i = this.min + i |> box
    member this.GetEnumerator () = (seq { this.min .. this.max } :> IEnumerable).GetEnumerator()
    member this.IsSynchronized = false
    member this.SyncRoot = null
    member this.CopyTo (dst, pos) = for i = this.min to this.max do dst.SetValue (i, pos + i - this.min)


type IMutable =
  abstract Value : obj with get, set

type Record = Map<string, obj>


module FunnyArray =
  let ofArray (a : Array) =
    { new IFunnyArray with
        member __.Count = a.Length
        member __.Item with get i = a.GetValue i
        member __.GetEnumerator() = a.GetEnumerator()
        member __.IsSynchronized = a.IsSynchronized
        member __.SyncRoot = a.SyncRoot
        member __.CopyTo (a, i) = a.CopyTo (a, i)
    }

  let map f (a : IFunnyArray) =
    Array.init a.Count (fun i -> f a.[i])

  let choose f a =
    a |> map f |> Array.choose id


[<AutoOpen>]
module Data =
  let (|FunnyArray|_|) (a : obj) =
    match a with
    | :? IFunnyArray as a -> Some a
    | :? Array as a -> Some (FunnyArray.ofArray a)
    | _ -> None


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

