namespace FunnyScript
// --- F# 4.1 で標準となる Result 型 ---

type Result<'T,'TError> =
    | Ok of 'T 
    | Error of 'TError

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

  [<CompiledName("Map")>]
  let map f inp = match inp with Error e -> Error e | Ok x -> Ok (f x)

  [<CompiledName("MapError")>]
  let mapError f inp = match inp with Error e -> Error (f e) | Ok x -> Ok x

  [<CompiledName("Bind")>]
  let bind f inp = match inp with Error e -> Error e | Ok x -> f x