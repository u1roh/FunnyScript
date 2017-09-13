module FunnyScript.Script
open System.IO;

type Error =
  | RuntimeError  of AST.ErrInfo
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


let getRuntimeErrorString e =
  sprintf "RUNTIME ERROR! %A" e.Err
  |> List.foldBack (fun (expr, env) msg ->
    match expr with
    | Trace (expr, pos) -> msg + "\n" + sprintf "at %s: %A" (pos.ToString()) expr
    | _ -> msg) e.StackTrace

let getResultString result =
  match result with
  | Ok x -> sprintf "%A" x
  | Error (ParserError  s) -> sprintf "PARSER ERROR!: %s" s
  | Error (RuntimeError e) -> getRuntimeErrorString e
