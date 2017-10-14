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
  | Array of Pattern list * Pattern list option
  | Record of list<string * Pattern>
  | Case of string * Pattern
  | Named of string * Pattern
with
  static member Empty = Tuple[]
    
type Case (pat : Pattern) =
  new () = Case Pattern.Empty
  member __.Pattern = pat

type CaseValue = CaseValue of Case * obj

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
    
  let append (a : IFunnyArray) (b : IFunnyArray) =
    Array.init (a.Count + b.Count) (fun i -> if i < a.Count then a.[i] else b.[i - a.Count])

[<AutoOpen>]
module Data =
  let (|FunnyArray|_|) (a : obj) = FunnyArray.ofObj a


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pattern =
  let tryMatchWith (env : string -> obj option) (obj : obj) pattern =
    let rec execute pattern (obj : obj) matched =
      let foldMatchedMap items map =
        items |> List.fold (fun matched (item, obj) -> matched |> Option.bind (execute item obj)) map
      match pattern with
      | Named (name, pattern) -> matched |> execute pattern obj |> Option.map (Map.add name obj)
      | Any -> Some matched
      | Tuple items ->
        match items, obj with
        | [], null -> matched |> Some
        | _, FunnyArray a when a.Count >= 2 && a.Count = items.Length ->
          Some matched |> foldMatchedMap (items |> List.mapi (fun i item -> item, a.[i]))
        | [item], _ -> matched |> execute item obj
        | _ -> None
      | Array (items, None) ->
        match obj with
        | FunnyArray a when a.Count = items.Length ->
          Some matched |> foldMatchedMap (items |> List.mapi (fun i item -> item, a.[i]))
        | _ -> None
      | Array (heads, Some tails) ->
        match obj with
        | FunnyArray a when a.Count >= heads.Length + tails.Length ->
          let heads = heads |> List.mapi (fun i item -> item, a.[i])
          let tails = tails |> List.rev |> List.mapi (fun i item -> item, a.[a.Count - 1 - i])
          Some matched |> foldMatchedMap heads |> foldMatchedMap tails
        | _ -> None
      | Record items ->
        match obj with
        | :? Record as r ->
          (Some matched, items) ||> List.fold (fun matched (name, item) ->
            matched |> Option.bind (fun matched ->
            r |> Map.tryFind name |> Option.bind (fun obj ->
              matched |> execute item obj)))
        | _ -> None
      | Case (case, pattern) ->
        if pattern = Pattern.Empty && env case = Some obj then Some matched else
        env case
        |> Option.bind (function :? Case as case -> Some case | _ -> None)
        |> Option.bind (fun case ->
          match obj with :? CaseValue as obj -> Some obj | _ -> None
          |> Option.bind (function CaseValue (c, obj) when c = case -> Some obj | _ -> None))
        |> Option.bind (fun obj -> matched |> execute pattern obj)
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

