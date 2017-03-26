open FunnyScript

[<EntryPoint>]
let main argv = 
  "fac := n -> n == 0 ? 1 : n * fac (n - 1);" +
  "do fac 4 |> trace;"
  |> Script.runScriptStr
  |> printfn "%A"

  0 // 整数の終了コードを返します
