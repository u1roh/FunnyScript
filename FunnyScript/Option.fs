module private FunnyScript.Option

// --- F# 4.1 で標準となる関数 ---

[<CompiledName("DefaultValue")>]
let defaultValue def option = match option with None -> def | Some v -> v

[<CompiledName("DefaultWith")>]
let defaultWith defThunk option = match option with None -> defThunk () | Some v -> v

[<CompiledName("OrElse")>]
let orElse ifNone option = match option with None -> ifNone | Some _ -> option

[<CompiledName("OrElseWith")>]
let orElseWith ifNoneThunk option = match option with None -> ifNoneThunk () | Some _ -> option

[<CompiledName("Contains")>]
let inline contains x inp = match inp with None -> false | Some v -> v = x

[<CompiledName("Map2")>]
let map2 f option1 option2 = 
    match option1, option2 with
    | Some x, Some y -> Some <| f x y
    | _ -> None

[<CompiledName("Map3")>]
let map3 f option1 option2 option3 = 
    match option1, option2, option3 with
    | Some x, Some y, Some z -> Some <| f x y z
    | _ -> None

[<CompiledName("Flatten")>]
let flatten option = match option with None -> None | Some x -> x
