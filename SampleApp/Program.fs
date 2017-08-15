open System;
open System.Drawing;
open System.Windows.Forms;
open FunnyScript;

type ScriptRunner () =
  let env =
    Script.defaultEnv
    |> CLR.loadAssembly (typeof<System.Drawing.Graphics>.Assembly)
  let mutable script = ""
  let mutable program : Expr = { Value = Obj Null; Position = None }

  member __.Script = script

  member __.Parse str =
    script <- str
    match Parser.parse "" script with
    | Error e -> printfn "%s" e
    | Ok expr -> program <- expr
  
  member __.Run (argname, argobj) =
    let env = env |> Map.add (Name argname) (ClrObj argobj |> Ok)
    Script.eval env program |> printfn "%A"

[<EntryPoint>]
let main argv = 
  let sampleScript = """d := System.Drawing;
pen := d.Pen.new (d.Color.Black);
do g.DrawEllipse (pen, 10, 10, 200, 100); """
  let runner = ScriptRunner()
  runner.Parse sampleScript
  let mainform = new Form (Width = 600, Height = 600)
  let editor = new TextBox (Multiline = true, Dock = DockStyle.Bottom, Height = 200, ScrollBars = ScrollBars.Vertical, Font = new Font("Consolas", 11.0f))
  editor.Text <- runner.Script
  editor.KeyDown.Add (fun e ->
    if e.KeyCode = Keys.F5 then
      runner.Parse editor.Text
      mainform.Invalidate())
  mainform.Controls.Add editor
  mainform.Paint.Add (fun e -> runner.Run ("g", e.Graphics))
  Application.Run mainform
  0
