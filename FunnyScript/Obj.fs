module FunnyScript.Obj
open System
open System.Collections

let rec force (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind force
  | :? IMutable as x -> force x.Value
  | _ -> Ok obj

// mutable はそのままキープ
let rec forceLet (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind forceLet
  | _ -> Ok obj

let rec forceMutable (obj : obj) =
  match obj with
  | :? Lazy<Result> as x -> x.Force() |> Result.bind forceMutable
  | :? IMutable as x -> Ok x
  | _ -> error NotMutable
    
let cast<'a> (obj : obj) =
  match obj with
  | :? 'a as obj -> Ok obj
  | _ -> error (TypeMismatch (ClrType typeof<'a>, FunnyType.ofObj obj))

let toFunnyArray (obj : obj) =
  match obj with
  | FunnyArray a -> Ok a
  | _ -> error (TypeMismatch (ClrType typeof<IFunnyArray>, FunnyType.ofObj obj))

let toSeq (obj : obj) =
  match obj with
  | Seq a -> Ok a
  | _ -> error (TypeMismatch (ClrType typeof<IFunnyArray>, FunnyType.ofObj obj))

let applyCore env (f : obj) (arg : obj) =
  let err() = error (NotApplyable (f, arg))
  match f with
  | :? IFuncObj as f -> f.Apply (arg, env)
  | :? int as a ->
    match arg with
    | :? int   as b -> Ok <| box (a * b)
    | :? float as b -> Ok <| box (float a * b)
    | _ -> err()
  | :? float as a ->
    match arg with
    | :? int   as b -> Ok <| box (a * float b)
    | :? float as b -> Ok <| box (a * b)
    | _ -> err()
  | :? FunnyType as t ->
    match t with
    | ClrType t ->
      CLR.tryGetConstructor t
      |> function Some ctor -> Ok ctor | _ -> err()
      |> Result.bind (fun ctor -> ctor.Apply (arg, env))
    | FunnyClass t ->
      t.Ctor.Apply (arg, env) |> Result.bind force |> Result.map (fun x -> box { Data = x; Type = t })
  | :? Case as c ->
    c.Pattern |> Pattern.tryMatchWith (fun name -> env |> Env.tryGet name) arg
    |> function Some _ -> CaseValue (c, arg) |> box |> Ok | _ -> error Unmatched
  | x ->
    x |> CLR.tryApplyIndexer arg |> Option.defaultWith (fun () ->
      match x, arg with
      | (:? IEnumerable as x), (:? int as i) -> Ok (x |> Seq.cast<obj> |> Seq.item i)
      | _ -> error (TypeMismatch (ClrType typeof<int>, FunnyType.ofObj arg)))
  //| _ -> err()

let apply f arg = applyCore Env.empty f arg

let findMember (obj : obj) name =
  let notFound = error (IdentifierNotFound name)
  let toResult x = match x with Some x -> Ok x | _ -> notFound
  match obj with
  | :? Record as r -> r |> Map.tryFind name |> toResult
  | :? Instance as x ->
    x.Type.Members |> Map.tryFind name |> Option.map (fun f -> apply (box f) x.Data)
    |> Option.orElseWith (fun () -> x.Type.ExtMembers |> Map.tryFind name |> Option.map (fun f -> apply (box f) x))
    |> Option.defaultValue notFound
  | :? FunnyType as t ->
    match t with
    | ClrType t -> t |> CLR.tryGetStaticMember name |> toResult
    | _ -> notFound
  | o -> o |> CLR.tryGetInstanceMember name |> toResult
