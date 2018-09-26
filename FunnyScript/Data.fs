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


type [<AbstractClass>] Mutable() =
  abstract Value : obj with get, set
  override this.ToString() = sprintf "mutable %A" this.Value

type Record = Map<string, obj>

module FunnyArray =
  let ofCollection getItem (a : ICollection) =
    { new IFunnyArray with
        member __.Count = a.Count
        member __.Item with get i = getItem i
        member __.GetEnumerator() = a.GetEnumerator()
        member __.IsSynchronized = a.IsSynchronized
        member __.SyncRoot = a.SyncRoot
        member __.CopyTo (a, i) = a.CopyTo (a, i)
    }

  let ofArray (a : Array) =
    ofCollection a.GetValue a

  let ofObj (a : obj) =
    match a with
    | :? IFunnyArray as a -> Some a
    | :? Array as a -> Some (ofArray a)
    | :? ArrayList as a -> Some (ofCollection (fun i -> a.[i]) a)
    | _ -> None

  let toArray (a : IFunnyArray) =
    Array.init a.Count (fun i -> a.[i])

  let map f (a : IFunnyArray) =
    Array.init a.Count (fun i -> f a.[i])

  let choose f a =
    a |> map f |> Array.choose id

  let collect (f : obj -> IFunnyArray) a =
    a |> map f |> Array.collect toArray

  let filter pred a =
    let pred = a |> map pred
    let count = pred |> Seq.fold (fun n pred -> if pred then n + 1 else n) 0
    let dst = Array.create<obj> count null
    let mutable k = 0
    a |> Seq.cast<obj> |> Seq.iteri (fun i x -> if pred.[i] then dst.[k] <- x; k <- k + 1)
    dst

  let distinct (a : IFunnyArray) =
    a |> Seq.cast<obj> |> Seq.distinct |> Seq.toArray

  let append (a : IFunnyArray) (b : IFunnyArray) =
    Array.init (a.Count + b.Count) (fun i -> if i < a.Count then a.[i] else b.[i - a.Count])

[<AutoOpen>]
module Data =
  let (|FunnyArray|_|) (a : obj) = FunnyArray.ofObj a
  let (|Seq|_|) = function (:? IEnumerable as a : obj) -> a |> Seq.cast<obj> |> Some | _ -> None



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

