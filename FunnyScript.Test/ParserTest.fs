module FunnyScript.Test.ParserTest

open NUnit.Framework
open FParsec.CharParsers
open FunnyScript

// テストの読みやすさと書きやすさのために、ダブルクォートとバックスラッシュを置き換え
// 「"」 -> `
// 「\」 -> $
[<Test>]
[<TestCase("``", "")>]
[<TestCase("`abc`", "abc")>]
[<TestCase("`abc$n`", "abc\n")>]
[<TestCase("`$$`", "$")>]
[<TestCase("`$`abc$``", "`abc`")>]
[<TestCase("`$t`", "\t")>]
let ``string literal`` (snipet, expected) =
  let replace (str: string) =
    str.Replace("`", "\"").Replace("$", "\\")
  let expected = replace expected
  let result = snipet |> replace |> Parser.parse "ParserTest"
  match result with
  | Ok expr ->
    let rec strip x = match x with Trace (x, _) -> strip x | _ -> x
    Assert.AreEqual(Obj expected, strip expr)
  | Error err ->
    Assert.Fail (sprintf "%A" err)

[<Test>]
let identifierTest () =
  let parse id = id |> runParserOnString Parser.pIdentifier () "" |> function Success (r, _, _) -> true | _ -> false
  parse "hoge" |> Assert.True
  parse "piyo_123" |> Assert.True
  parse "ひらがな" |> Assert.True
  parse "Σ" |> Assert.True
  parse "13abc" |> not |> Assert.True
  parse "do" |> not |> Assert.True
