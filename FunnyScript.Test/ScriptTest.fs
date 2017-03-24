module FunnyScript.Test.ScriptTest
open Persimmon
open FunnyScript

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
    "a := 1 + 2; b := 3 + 4; a * b"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 21))
  do!
    "f := a -> a + 1; f 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 3))
  do!
    "f := a -> b -> a * b; f 3 4"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 12))
  do!
    "true ? 1 : 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 1))
  do!
    "a := 5; a < 6 ? 1 : 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 1))
  do!
    "fac := n -> n == 0 ? 1 : n * fac (n - 1); fac 4"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 24))
  do!
    "r := { a := 10; b := 2 + 3; }; r.b"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 5))
  do!
    "r := { f := n -> n * 2; }; r.f 2"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 4))
  do!
    "r := { f := n -> n * 2; g := n -> f (n + 1); }; (r.g 2)"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 6))
  do!
    "f := x -> y -> x + y; 10 |> f 5"
    |> Script.runScriptStr
    |> assertEquals (Some (Int 15))
  do!
    "[1, 2, 3 + 4]"
    |> Script.runScriptStr
    |> assertEquals (Some (List (FunnyList.ofArray [| Int 1; Int 2; Int 7 |])))
}