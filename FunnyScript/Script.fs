module FunnyScript.Script
open System
open System.IO

type Error =
  | RuntimeError  of AST.ErrInfo
  | ParserError   of string

type Env private (data : AST.Env) =
  static member Default =
    Env.empty
    |> CLR.loadSystemAssembly
    |> CLR.loadAssembly typeof<Numerics.Complex>.Assembly
    |> CLR.loadAssembly typeof<unit>.Assembly
    |> Builtin.load
    |> Env

  member this.LoadAssembly asm =
    data |> CLR.loadAssembly asm |> Env

  member this.LoadModule (name, m : list<string * Expr>) =
    let result = data |> Eval.eval (NewRecord m) 
    data |> Env.add name result |> Env

  member this.LoadModule (name, moduleScript) =
    let result =
      Parser.parseModule name moduleScript
      |> Result.mapError (ParseError >> ErrInfo.Create)
      |> Result.bind (fun m -> data |> Eval.eval (NewRecord m))
    data |> Env.add name result |> Env

  member this.Add (name, obj : obj) =
    data |> Env.add name (Ok obj) |> Env

  member this.AddFunc (name, f : Func<'a>) =
    this.Add (name, FuncObj.ofFun f.Invoke |> box)

  member this.AddFunc (name, f : Func<'a, 'b>) =
    this.Add (name, FuncObj.ofFun f.Invoke |> box)

  member this.AddFunc (name, f : Func<'a, 'b, 'c>) =
    this.Add (name, FuncObj.ofFun2 (fun a b -> f.Invoke (a, b)) |> box)

  member this.AddAction (name, f : Action) =
    this.Add (name, FuncObj.ofFun f.Invoke |> box)

  member this.AddAction (name, f : Action<'a>) =
    this.Add (name, FuncObj.ofFun f.Invoke |> box)

  member this.AddAction (name, f : Action<'a, 'b>) =
    this.Add (name, FuncObj.ofFun2 (fun a b -> f.Invoke (a, b)) |> box)

  member this.Eval expr =
    data |> Eval.eval expr |> Result.bind Obj.force

  member this.Run (streamName, source) =
    Parser.parse streamName source
    //|> Result.map (fun x -> DebugDump.dump 1 x; x)
    |> Result.mapError ParserError
    |> Result.bind (this.Eval >> Result.mapError RuntimeError)

  member this.RunFile path =
    this.Run (path, File.ReadAllText path)


let getRuntimeErrorString e =
  sprintf "RUNTIME ERROR! %A" e.Err
//  |> List.foldBack (fun (expr, env) msg ->
//    match expr with
//    | Trace (expr, pos) -> msg + "\n" + sprintf "at %s: %A" (pos.ToString()) expr
//    | _ -> msg) e.StackTrace
  |> List.foldBack (fun pos msg -> msg + "\n" + sprintf "at %s" (pos.ToString()) ) e.StackTrace

let getResultString result =
  match result with
  | Ok x -> sprintf "%A" x
  | Error (ParserError  s) -> sprintf "PARSER ERROR!: %s" s
  | Error (RuntimeError e) -> getRuntimeErrorString e
