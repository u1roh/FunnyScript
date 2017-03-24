namespace FunnyScript

type FunnyLength =
  | Definite of int
  | Infinity
  | Unknown

type IFunnyList<'a> =
  abstract Head : 'a
  abstract Tail : IFunnyList<'a>
  abstract IsEmpty : bool
  abstract Length : FunnyLength
  abstract Item : int -> 'a with get

module FunnyList =

  let nil<'a> =
    { new IFunnyList<'a> with
        member this.Head = failwith "FunnyList: is empty"
        member this.Tail = this
        member this.IsEmpty = true
        member this.Length = Definite 0
        member this.Item with get _ = failwith "FunnyList: is empty" }

  let cons head tail =
    { new IFunnyList<_> with
        member __.Head = head
        member __.Tail = tail
        member this.IsEmpty = false
        member __.Length =
          match tail.Length with
          | Definite len -> Definite (len + 1)
          | _ -> tail.Length
        member __.Item
          with get i = if i = 0 then head else tail.[i - 1] }

  [<Struct>]
  type private FunnyList<'a> (src : 'a list) =
    interface IFunnyList<'a> with
      member this.Head = src.Head
      member this.Tail = FunnyList src.Tail :> _
      member this.IsEmpty = src.IsEmpty
      member this.Length = Definite src.Length
      member this.Item with get i = src.[i]

  [<Struct>]
  type private FunnyArray<'a> (src : 'a[], k : int) =
    interface IFunnyList<'a> with
      member this.Head = src.[k]
      member this.Tail = if k + 1 = src.Length then nil else FunnyArray (src, k + 1) :> _
      member this.IsEmpty = k >= src.Length
      member this.Length = Definite (src.Length - k)
      member this.Item with get i = src.[i + k]

  type private FunnySeq<'a> (src : System.Collections.Generic.IEnumerator<'a>) =
    let head = src.Current
    let tail = lazy (if src.MoveNext() then FunnySeq src :> IFunnyList<_> else src.Dispose(); nil)
    interface IFunnyList<'a> with
      member this.Head = src.Current
      member this.Tail = tail.Force()
      member this.IsEmpty = false
      member this.Length = Unknown
      member this.Item
        with get i = if i = 0 then head else tail.Force().[i - 1]

  type private UnfoldList<'a> (head, state, generator) =
    interface IFunnyList<'a> with
      member this.Head = head
      member this.Tail =
        match generator state with
        | Some (head, state) -> UnfoldList (head, state, generator) :> IFunnyList<_>
        | None -> nil
      member this.IsEmpty = false
      member this.Length = Unknown
      member this.Item
        with get i = if i = 0 then head else (this :> IFunnyList<_>).Tail.[i - 1]

  let ofList src =
    if List.isEmpty src then nil else FunnyList src :> IFunnyList<_>

  let ofArray src =
    if Array.isEmpty src then nil else FunnyArray (src, 0) :> IFunnyList<_>

  let ofSeq (src : seq<_>) =
    let it = src.GetEnumerator()
    if it.MoveNext()
      then FunnySeq it :> IFunnyList<_>
      else nil

  let unfold generator state =
    match generator state with
    | Some (head, state) -> UnfoldList (head, state, generator) :> IFunnyList<_>
    | None -> nil

  let init count initializer =
    Array.init count initializer |> ofArray