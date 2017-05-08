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

let forStr src =
  Parser.parse src
  //|> Result.map (fun x -> DebugDump.dump 1 x; x)
  |> Result.mapError ParserError
  |> Result.bind eval

let forLines (lines : string seq) =
  lines
  |> Seq.map (fun s -> let i = s.IndexOf "//" in if 0 <= i && i < s.Length then s.Substring (0, i) else s)  // コメントの除去
  |> String.concat "\n"
  |> forStr

let forReader (reader : System.IO.TextReader) =
  seq {
    let mutable line = reader.ReadLine()
    while line <> null do
      yield line
      line <- reader.ReadLine()
  }
  |> forLines

let forFile path =
  use reader = System.IO.File.OpenText path
  forReader reader
