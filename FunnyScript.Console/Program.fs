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
    Script.forFile argv.[0]
    |> printfn "%A"
  0
