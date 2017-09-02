open System;
open System.Drawing;
open System.Windows.Forms;
open FunnyScript;

type ScriptRunner (output : string -> unit) =
  let env = Script.Env.Default.LoadAssembly typeof<System.Drawing.Graphics>.Assembly
  let mutable script = ""
  let mutable program : Expr = Obj null

  member __.Script = script

  member __.Parse str =
    script <- str
    match Parser.parse "" script with
    | Error e -> sprintf "%s" e |> output; false
    | Ok expr -> program <- expr; true
  
  member __.Run (argname, argobj) =
    let env = env.Add (argname, argobj)
    let result = env.Eval program
    output (sprintf "%A" result)
    Result.isOk result

[<EntryPoint>]
let main argv = 
  let sampleScript = """open System.Drawing;
pen := Pen.new (Color.Black);
do g.DrawEllipse (pen, 10, 10, 200, 100); """
  let filepath = IO.Path.Combine (IO.Path.GetDirectoryName Application.ExecutablePath, "sample.fny")
  let mainform = new Form (Width = 1000, Height = 600)
  mainform.SuspendLayout()
  let canvas = new Panel (Dock = DockStyle.Fill, Width = 600, Height = 400)
  let editor = new TextBox (Multiline = true, Dock = DockStyle.Fill, ScrollBars = ScrollBars.Vertical, Font = new Font("Consolas", 11.0f))
  let stdout = new TextBox (Multiline = true, Dock = DockStyle.Fill, ScrollBars = ScrollBars.Vertical, Font = new Font("Consolas", 11.0f), ReadOnly = true)
  let splitter1 = new SplitContainer (Dock = DockStyle.Fill, Orientation = Orientation.Vertical)
  let splitter2 = new SplitContainer (Dock = DockStyle.Fill, Orientation = Orientation.Horizontal)
  splitter2.Panel1.Controls.Add canvas
  splitter2.Panel2.Controls.Add stdout
  splitter1.Panel1.Controls.Add splitter2
  splitter1.Panel2.Controls.Add editor
  editor.Width <- 300
  mainform.ResumeLayout()
  mainform.PerformLayout()
  splitter2.SplitterDistance <- 80
  splitter1.SplitterDistance <- 80

  let runner = ScriptRunner (fun s -> stdout.Lines <- s.Split '\n')
  if IO.File.Exists filepath
    then IO.File.ReadAllText filepath
    else sampleScript
  |> runner.Parse |> ignore
  editor.Text <- runner.Script
  editor.KeyDown.Add (fun e ->
    if e.KeyCode = Keys.F5 then
      if runner.Parse editor.Text then
        canvas.Invalidate()
        IO.File.WriteAllText (filepath, editor.Text))
  mainform.Controls.Add splitter1
  canvas.Paint.Add (fun e -> runner.Run ("g", e.Graphics) |> ignore)
  Application.Run mainform
  0
