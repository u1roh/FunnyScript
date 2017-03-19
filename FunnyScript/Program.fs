open System
open System.IO
open FunnyScript

let test src =
  printfn "--------------------------------------"
  printfn "INPUT = \"%s\"" src
  let expr = Parser.parse src
  if expr.IsSome
    then printfn "Parsed Successfully!"
    else printfn "Failed to parse."

  let result = expr |> Option.bind Lib.eval
  if result.IsSome
    then printfn "OUTPUT = %A" result.Value
    else printfn "Failed to eval."

//  expr |> Option.iter (fun expr ->
//    Lib.evalCps expr (fun result ->
//      printfn "OUTPUT = %A" result))

[<EntryPoint>]
let main argv = 
//  Samples.runAll ()
//
  test "1 + 2 * (3 + 4)"
  test "trace 1"
  test "f a b"
  test "sin (3.14 / 2.0)"
  test "2.0 * sin 3.14 + 1.0"
  test "let a = 1 + 2; let b = 3 + 4; a * b"
  test "3.14 / 2.0 |> sin"
  test "let f = \\a -> a + 1; f 2"
  test "true"
  test "true ? 1 : 2"
  test "let a = 5; a < 6 ? 1 : 2"
  test "do trace 10; do trace 20;"
//  test "let g = \\n -> do trace n; g (n + 1); g 1"

  use reader = File.OpenText "test01.txt"
  reader.ReadToEnd() |> test

//  Parser.sample()
//  Parser.test()

  0
