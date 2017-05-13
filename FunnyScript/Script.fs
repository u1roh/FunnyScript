module FunnyScript.Script

type Error =
  | AstError    of AST.Error
  | ParserError of FParsec.Error.ParserError

let defaultEnv =
  Map.empty
  |> CLR.loadSystemAssembly
  |> Builtin.load

let eval env expr =
  env
  |> Eval.eval expr
  |> Result.bind Eval.force
  |> Result.mapError AstError

//let evalCps env expr =
//  env
//  |> Eval.evalCps expr

let forStr env src =
  Parser.parse src
  //|> Result.map (fun x -> DebugDump.dump 1 x; x)
  |> Result.mapError ParserError
  |> Result.bind (eval env)

let forLines env (lines : string seq) =
  lines
  |> Seq.map (fun s -> let i = s.IndexOf "//" in if 0 <= i && i < s.Length then s.Substring (0, i) else s)  // コメントの除去
  |> String.concat "\n"
  |> forStr env

let forReader env (reader : System.IO.TextReader) =
  seq {
    let mutable line = reader.ReadLine()
    while line <> null do
      yield line
      line <- reader.ReadLine()
  }
  |> forLines env

let forFile env path =
  use reader = System.IO.File.OpenText path
  forReader env reader
