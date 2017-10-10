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

type Pattern =
  | Any
  | Tuple of Pattern list
  | Record of list<string * Pattern>
  | Named of string * Pattern
with
  static member Empty = Tuple[]
    
type Case (pat : Pattern) =
  new () = Case Pattern.Empty
  member __.Pattern = pat

type CaseValue = CaseValue of Case * obj

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

  let filter pred a =
    let pred = a |> map pred
    let count = pred |> Seq.fold (fun n pred -> if pred then n + 1 else n) 0
    let dst = Array.create<obj> count null
    let mutable k = 0
    a |> Seq.cast<obj> |> Seq.iteri (fun i x -> if pred.[i] then dst.[k] <- x; k <- k + 1)
    dst
    
  let append (a : IFunnyArray) (b : IFunnyArray) =
    Array.init (a.Count + b.Count) (fun i -> if i < a.Count then a.[i] else b.[i - a.Count])

[<AutoOpen>]
module Data =
  let (|FunnyArray|_|) (a : obj) =
    match a with
    | :? IFunnyArray as a -> Some a
    | :? Array as a -> Some (FunnyArray.ofArray a)
    | _ -> None


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pattern =
  let tryMatchWith (obj : obj) pattern =
    let rec execute pattern (obj : obj) matched =
      match pattern with
      | Named (name, pattern) -> matched |> execute pattern obj |> Option.map (Map.add name obj)
      | Any -> Some matched
      | Tuple items ->
        match items, obj with
        | [], null -> matched |> Some
        | _, FunnyArray a when a.Count = items.Length ->
          items
          |> List.mapi (fun i item -> item, a.[i])
          |> List.fold (fun env (item, obj) -> env |> Option.bind (execute item obj)) (Some env)
        | [item], _ -> matched |> execute item obj
        | _ -> None
      | Record items ->
        match obj with
        | :? Record as r ->
          (Some matched, items) ||> List.fold (fun env (name, item) ->
            env |> Option.bind (fun env ->
            r |> Map.tryFind name |> Option.bind (fun obj ->
              env |> execute item obj)))
        | _ -> None
    Map.empty |> execute pattern obj


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

