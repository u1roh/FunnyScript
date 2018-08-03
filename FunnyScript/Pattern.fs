[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FunnyScript.Pattern

let tryMatchWith (obj : obj) pattern =
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
    | Typed (ClrType t) -> if t.IsAssignableFrom (obj.GetType()) then Some matched else None
    | Typed (FunnyClass t) -> match obj with :? Instance as obj when obj.Type = t -> Some matched | _ -> None
    | Case (caseObj, pattern) ->
      if pattern = Pattern.Empty && caseObj = obj then Some matched else
      match caseObj with
        | :? Case as case -> Some case
        | _ -> None
      |> Option.bind (fun case ->
        match obj with :? CaseValue as obj -> Some obj | _ -> None
        |> Option.bind (function CaseValue (c, obj) when c = case -> Some obj | _ -> None))
      |> Option.orElseWith (fun () ->
        match caseObj with
          | :? FunnyType as t ->
            match t with
            | ClrType t when t.IsNested && Reflection.FSharpType.IsUnion t.DeclaringType && obj.GetType() = t -> 
              let _, fields = Reflection.FSharpValue.GetUnionFields (obj, t.DeclaringType)
              Some (if fields.Length = 1 then fields.[0] else box fields)
            | _ -> None
          | _ -> None)
      |> Option.bind (fun obj -> matched |> execute pattern obj)
  Map.empty |> execute pattern obj
