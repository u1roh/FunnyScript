open FunnyScript
open System

[<EntryPoint>]
let main argv = 
  if argv.Length = 0 then
    let asm = Reflection.Assembly.GetEntryAssembly()
    let exename = IO.Path.GetFileName asm.Location
    printfn "FunnyScript ver %A" (asm.GetName().Version)
    printfn "usage: %s <filepath>" exename
  else
    match Script.Env.Default.RunFile argv.[0] with
    | Ok x -> printfn "%A" x
    | Error (Script.ParserError msg) ->
      printfn "FunnyScript: Parser Error !!"
      printfn "%s" msg
    | Error (Script.RuntimeError e) ->
      printfn "FunnyScript: Error !!"
      printfn "%A" e
  0
