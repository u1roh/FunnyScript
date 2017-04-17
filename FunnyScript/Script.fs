module FunnyScript.Script

type Error =
  | AstError    of AST.Error
  | ParserError of FParsec.Error.ParserError

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
  Parser.parse src
  |> Result.map (fun x -> DebugDump.dump 1 x; x)
  |> Result.mapError ParserError
  |> Result.bind eval
