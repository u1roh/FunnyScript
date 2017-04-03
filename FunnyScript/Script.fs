module FunnyScript.Script

type Error =
  | AstError    of AST.Error
  | ParserError of Parser.Error

type Result<'a> = Result<'a, Error>

let eval expr =
  Map.empty
  |> CLR.loadSystemAssembly
  |> Builtin.load
  |> Eval.eval expr
  |> Result.bind Eval.force
  |> Result.mapError AstError

let evalCps expr =
  Map.empty
  |> CLR.loadSystemAssembly
  |> Builtin.load
  |> Eval.evalCps expr

let runScriptStr src =
  match Parser.parse src with
    | Some expr -> Ok expr
    | _ -> Parser.MiscError "Failed to parse" |> ParserError |> Error
  |> Result.bind eval
