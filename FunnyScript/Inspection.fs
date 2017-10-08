module FunnyScript.Inspection
open System
open System.Collections.Generic
 
type Item = {
    Caption : string
    Description : string
    Items : Item seq
  } with
  member this.IsEmpty = String.IsNullOrEmpty this.Caption
  static member Empty = { Caption = ""; Description = null; Items = [] }

let rec private getTypeName (t : Type) =
  let prefix = if t.IsNested && not t.IsGenericParameter then (getTypeName t.DeclaringType) + "." else ""
  if t.IsGenericType then
    let name = let len = t.Name.IndexOf '`' in if len < 0 then t.Name else t.Name.Substring (0, len)
    let args = t.GetGenericArguments() |> Array.map getTypeName |> String.concat ", "
    sprintf "%s%s<%s>" prefix name args
  else prefix + t.Name

let rec getItems (target : obj) =
  let create name target =
    let tooltip = sprintf "%s : %s = %A" name (if target = null then "???" else getTypeName <| target.GetType()) target
    let caption =
      let caption = sprintf "%s = %A" name target
      let caption = caption.Replace ("\n", "")
      let maxlen = 40
      if caption.Length <= maxlen then caption else caption.Substring(0, maxlen-3) + "..."
    { Caption = caption; Description = tooltip; Items = getItems target }

  let isHiddenProperty (prop : Reflection.PropertyInfo) =
      prop.DeclaringType = typeof<Array> && prop.Name <> "Length" // Array 型のプロパティは Length 以外は隠す

  let ofProperties target =
    let targetType = target.GetType()
    let isList = (targetType.IsGenericType && targetType.GetGenericTypeDefinition() = typeof<int list>.GetGenericTypeDefinition())
    if isList then Seq.empty else
      targetType.GetProperties ()
      |> Seq.filter (isHiddenProperty >> not)
      |> Seq.filter (fun prop -> let getter = prop.GetMethod in getter <> null && not getter.IsStatic && getter.GetParameters().Length = 0)
      |> Seq.choose (fun prop -> try Some (prop.Name, prop.GetValue target) with _ -> None)
      |> Seq.map (fun (name, value) -> create name value)

  let ofSeqItems target =
    let maxN = 10
    let rec makeMenuItems i0 items =
      items
      |> Seq.truncate (maxN + 1)
      |> Seq.mapi (fun i item ->
        if i = maxN
          then { Caption = "..."; Description = ""; Items =  items |> Seq.skip maxN |> makeMenuItems (i0 + maxN) }
          else create (sprintf "[%d]" (i0 + i)) item)
    makeMenuItems 0 target

  if target = null then Seq.empty else
  let propItems = ofProperties target
  let enumItems =
    match target with
    | :? Collections.IEnumerable as target -> target |> Seq.cast<obj> |> ofSeqItems
    | _ -> Seq.empty
  let headerItems =
    seq {
      yield { Item.Empty with Caption = getTypeName (target.GetType()) }
      yield Item.Empty
    }
  Seq.concat [headerItems; propItems; enumItems]
