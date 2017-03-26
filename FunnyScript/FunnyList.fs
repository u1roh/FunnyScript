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

  let rec private equals (list1 : IFunnyList<_>) (list2 : IFunnyList<_>) =
    match list1.IsEmpty, list2.IsEmpty with
    | true, true  -> true
    | true, false -> false
    | false, true -> false
    | _ -> list1.Head = list2.Head && equals list1.Tail list2.Tail

  let rec iter f (list : IFunnyList<_>) =
    if not list.IsEmpty then
      f list.Head
      iter f list.Tail

  let rec private iterN n f (list : IFunnyList<_>) =
    if   list.IsEmpty then true
    elif n = 0        then false
    else
      f list.Head
      iterN (n - 1) f list.Tail

  let private toString (list : IFunnyList<_>) =
    if list.IsEmpty then "[]" else
    let sb = System.Text.StringBuilder()
    sprintf "[%A" list.Head |> sb.Append |> ignore
    let completed = list.Tail |> iterN 10 (sprintf "; %A" >> sb.Append >> ignore)
    sb.Append (if completed then "]" else "; ...]") |> ignore
    sb.ToString()
    
  [<AbstractClass>]
  type private FunnyList<'a when 'a : equality>() =
    abstract Head : 'a
    abstract Tail : IFunnyList<'a>
    abstract IsEmpty : bool
    abstract Length : FunnyLength
    abstract Item : int -> 'a with get
    override this.Equals x =
      match x with
      | :? IFunnyList<'a> as x -> equals this x
      | _ -> false
    override this.GetHashCode () =
      if this.IsEmpty then 0 else this.Head.GetHashCode()
    override this.ToString () =
      toString this
    interface IFunnyList<'a> with
      member this.Head = this.Head
      member this.Tail = this.Tail
      member this.IsEmpty = this.IsEmpty
      member this.Length = this.Length
      member this.Item with get i = this.[i]
    

  let nil<'a when 'a : equality> =
    { new FunnyList<'a>() with
        member this.Head = failwith "FunnyList: is empty"
        member this.Tail = this :> _
        member this.IsEmpty = true
        member this.Length = Definite 0
        member this.Item with get _ = failwith "FunnyList: is empty" } :> IFunnyList<_>

  let cons head tail =
    { new FunnyList<_>() with
        member __.Head = head
        member __.Tail = tail
        member this.IsEmpty = false
        member __.Length =
          match tail.Length with
          | Definite len -> Definite (len + 1)
          | _ -> tail.Length
        member __.Item
          with get i = if i = 0 then head else tail.[i - 1] } :> IFunnyList<_>

  type private OfList<'a when 'a : equality> (src : 'a list) =
    inherit FunnyList<'a>()
    override this.Head = src.Head
    override this.Tail = OfList src.Tail :> _
    override this.IsEmpty = src.IsEmpty
    override this.Length = Definite src.Length
    override this.Item with get i = src.[i]

  type private FunnyArray<'a when 'a : equality> (src : 'a[], k : int) =
    inherit FunnyList<'a>()
    override this.Head = src.[k]
    override this.Tail = if k + 1 = src.Length then nil else FunnyArray (src, k + 1) :> _
    override this.IsEmpty = k >= src.Length
    override this.Length = Definite (src.Length - k)
    override this.Item with get i = src.[i + k]

  type private FunnySeq<'a when 'a : equality> (src : System.Collections.Generic.IEnumerator<'a>) =
    inherit FunnyList<'a>()
    let head = src.Current
    let tail = lazy (if src.MoveNext() then FunnySeq src :> IFunnyList<_> else src.Dispose(); nil)
    override this.Head = src.Current
    override this.Tail = tail.Force()
    override this.IsEmpty = false
    override this.Length = Unknown
    override this.Item
      with get i = if i = 0 then head else tail.Force().[i - 1]

  type private UnfoldList<'a when 'a : equality> (head, state, generator) =
    inherit FunnyList<'a>()
    override this.Head = head
    override this.Tail =
      match generator state with
      | Some (head, state) -> UnfoldList (head, state, generator) :> IFunnyList<_>
      | None -> nil
    override this.IsEmpty = false
    override this.Length = Unknown
    override this.Item
      with get i = if i = 0 then head else (this :> IFunnyList<_>).Tail.[i - 1]

  let ofList src =
    if List.isEmpty src then nil else OfList src :> IFunnyList<_>

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

  let toSeq (list : IFunnyList<_>) =
    seq {
      let mutable x = list
      while not x.IsEmpty do
        yield x.Head
        x <- x.Tail
    }
    