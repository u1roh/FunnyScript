open FunnyScript
open System

[<EntryPoint>]
let main argv = 
  if argv.Length = 0 then
    "fac := n -> n == 0 ? 1 : n * fac (n - 1);" +
    "do fac 4 |> trace;"
    |> Script.runScriptStr
    |> printfn "%A"
  else
    printfn "%s:" argv.[0]
    //let script = IO.File.ReadAllText (IO.Path.Combine (@"..\..\", argv.[0]))
    let script =
      IO.File.ReadAllLines (IO.Path.Combine (@"..\..\", argv.[0]))
      |> Array.map (fun s -> let i = s.IndexOf "//" in if 0 <= i && i < s.Length then s.Substring (0, i) else s)  // コメントの除去
      |> String.Concat
    printfn "%s" script
    printfn "---"
    script
    |> Script.runScriptStr
    |> printfn "%A"
   
  Console.ReadLine() |> ignore 
  0 // 整数の終了コードを返します
