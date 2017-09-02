module FunnyScript.Script
open System.IO;

type Error =
  | RuntimeError  of AST.Err
  | ParserError   of string

type Env private (data : AST.Env) =
  static member Default =
    Map.empty
    |> CLR.loadSystemAssembly
    |> Builtin.load
    |> Env

  member this.LoadAssembly asm =
    data |> CLR.loadAssembly asm |> Env

  member this.Add (name, obj : obj) =
    data |> Map.add name (Ok obj) |> Env

  member this.Eval expr =
    data |> Eval.eval expr |> Result.bind Eval.force

  member this.Run (streamName, source) =
    Parser.parse streamName source
    //|> Result.map (fun x -> DebugDump.dump 1 x; x)
    |> Result.mapError ParserError
    |> Result.bind (this.Eval >> Result.mapError RuntimeError)

  member this.RunFile path =
    this.Run (path, File.ReadAllText path)


let rec getRuntimeErrorString e =
  match e with
  | StackTrace (e, expr, env) ->
    let msg = getRuntimeErrorString e
    let trace =
      match expr.Position with
      | Some pos -> sprintf "at %s (%d, %d): %A" pos.FilePath pos.Line pos.Column expr.Value
      | None -> sprintf "at %A" expr.Value
    msg + "\n" + trace
  | _ -> sprintf "RUNTIME ERROR! %A" e

let getResultString result =
  match result with
  | Ok x -> sprintf "%A" x
  | Error (ParserError  s) -> sprintf "PARSER ERROR!: %s" s
  | Error (RuntimeError e) -> getRuntimeErrorString e
