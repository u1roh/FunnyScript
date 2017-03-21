module FunnyScript.Test.ScriptTest
open Persimmon
open FunnyScript
open FunnyScript.AST

(*
  test "sin (3.14 / 2.0)"
  test "2.0 * sin 3.14 + 1.0"
  test "3.14 / 2.0 |> sin"
  test "do trace 10; do trace 20;"
  *)

let testScript = test "scripting test" {
  do!
    "1 + 2 * (3 + 4)"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 15))
  do!
    "let a = 1 + 2; let b = 3 + 4; a * b"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 21))
  do!
    "let f = \\a -> a + 1; f 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 3))
  do!
    "true ? 1 : 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 1))
  do!
    "let a = 5; a < 6 ? 1 : 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 1))
  do!
    "let r = { let a = 10; let b = 2 + 3; }; r.b"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 5))
  do!
    "let r = { let f = \\n -> n * 2; }; r.f 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 4))
  do!
    "let r = { let f = \\n -> n * 2; let g = \\n -> f (n + 1); }; (r.g 2)"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 6))
}