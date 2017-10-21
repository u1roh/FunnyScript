[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FunnyScript.Pattern

let tryMatchWith (env : string -> obj option) (obj : obj) pattern =
  let rec execute pattern (obj : obj) matched =
    let foldMatchedMap items map =
      items |> List.fold (fun matched (item, obj) -> matched |> Option.bind (execute item obj)) map
    match pattern with
    | Named (name, pattern) -> matched |> execute pattern obj |> Option.map (Map.add name obj)
    | Any -> Some matched
    | Guard pred -> if pred.Invoke obj then Some matched else None
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
      |> Option.bind (fun case ->
        match case, obj with
        | (:? Case as case), (:? CaseValue as obj) ->
          let (CaseValue (c, obj)) = obj
          if c = case then Some obj else None
        | _ -> None)
      |> Option.bind (fun obj -> matched |> execute pattern obj)

//        env case
//        |> Option.bind (function :? Case as case -> Some case | _ -> None)
//        |> Option.bind (fun case ->
//          match obj with :? CaseValue as obj -> Some obj | _ -> None
//          |> Option.bind (function CaseValue (c, obj) when c = case -> Some obj | _ -> None))
//        |> Option.bind (fun obj -> matched |> execute pattern obj)
  Map.empty |> execute pattern obj
