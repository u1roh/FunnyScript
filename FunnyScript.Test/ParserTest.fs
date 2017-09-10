module FunnyScript.Test.ParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

open FParsec.CharParsers
open FunnyScript

let ``string literal`` () =
  let test (snipet, expected) = test {
    let replace (str: string) =
      str.Replace("`", "\"").Replace("$", "\\")
    let expected =
      match expected with
      | Obj (:? string as str) -> Obj (replace str)
      | other -> other
    let result = snipet |> replace |> Parser.parse "ParserTest"
    do!
      match result with
      | Ok expr ->
        let rec strip x = match x with Trace (x, _) -> strip x | _ -> x
        strip expr |> assertEquals expected
      | Error err ->
          fail (sprintf "%A" err)
  }
  parameterize {
    // テストの読みやすさと書きやすさのために、ダブルクォートとバックスラッシュを置き換え
    // 「"」 -> `
    // 「\」 -> $
    source [
      ("``", Obj "")
      ("`abc`", Obj "abc")
      ("`abc$n`", Obj "abc\n")
      ("`$$`", Obj "$")
      ("`$`abc$``", Obj "`abc`")
      ("`$t`", Obj "\t")
    ]
    run test
  }

let identifierTest = test {
  let parse id = id |> runParserOnString Parser.pIdentifier () "" |> function Success (r, _, _) -> true | _ -> false
  do! parse "hoge" |> assertPred
  do! parse "piyo_123" |> assertPred
  do! parse "ひらがな" |> assertPred
  do! parse "Σ" |> assertPred
  do! parse "13abc" |> not |> assertPred
  do! parse "do" |> not |> assertPred
}