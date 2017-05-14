module FunnyScript.Test.ParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

open FunnyScript

let ``string literal`` () =
  let test (snipet, expected) = test {
    let replace (str: string) =
      str.Replace("`", "\"").Replace("$", "\\")
    let expected =
      match expected with
      | Obj (ClrObj (:? string as str)) -> Obj (ClrObj (replace str))
      | other -> other
    let result = snipet |> replace |> Parser.parse
    do!
      match result with
      | Ok expr -> assertEquals expected expr
      | Error err ->
          fail (sprintf "%A" err)
  }
  parameterize {
    // テストの読みやすさと書きやすさのために、ダブルクォートとバックスラッシュを置き換え
    // 「"」 -> `
    // 「\」 -> $
    source [
      ("``", Obj (ClrObj ""))
      ("`abc`", Obj (ClrObj "abc"))
      ("`abc$n`", Obj (ClrObj "abc\n"))
      ("`$$`", Obj (ClrObj "$"))
      ("`$`abc$``", Obj (ClrObj "`abc`"))
      ("`$t`", Obj (ClrObj "\t"))
    ]
    run test
  }
