module FunnyScript.Script

let eval expr =
  Builtin.load() |> Eval.eval expr |> Option.bind Eval.force

let evalCps expr =
  Builtin.load() |> Eval.evalCps expr

let runScriptStr src =
  let expr = Parser.parse src
  if expr.IsNone then printfn "Failed to parse."

  let result = expr |> Option.bind eval
  if result.IsNone then printfn "Failed to eval."

  result
