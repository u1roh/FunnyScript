open System;
open FunnyScript;

let script =
  """
  console := System.Console;
  console.WriteLine "funny"
  """

[<EntryPoint>]
let main argv = 
  match Parser.parse "" script with
  | Error e -> printfn "%s" e
  | Ok expr ->
    let timer = new System.Timers.Timer()
    let ev = timer.GetType().GetEvent "Elapsed"
    //ev.AddEventHandler(timer, Timers.ElapsedEventHandler(fun sender e -> printfn "."))
    //let handler = Delegate.CreateDelegate (ev.EventHandlerType, (fun sender e -> ()))
//    ev.AddEventHandler(timer, (fun sender e -> ()))
    timer.Elapsed.Add (fun e ->
      Script.eval Script.defaultEnv expr |> ignore)
    timer.Start()
  stdin.ReadLine() |> ignore
  0
