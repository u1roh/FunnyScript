module FunnyScript.Test.ScriptTest
open Persimmon
open FunnyScript

(*
  test "sin (3.14 / 2.0)"
  test "2.0 * sin 3.14 + 1.0"
  test "3.14 / 2.0 |> sin"
  test "do trace 10; do trace 20;"
  *)

let (==>) script expected =
  script
  |> Script.runScriptStr
  |> assertEquals (Some expected)


let testScript = test "scripting test" {
  do! "1 + 2 * (3 + 4)" ==> (Int 15)
  do! "a := 1 + 2; b := 3 + 4; a * b" ==> (Int 21)
  do! "f := a -> a + 1; f 2" ==> (Int 3)
  do! "f := a -> b -> a * b; f 3 4" ==> (Int 12)
  do! "true ? 1 : 2" ==> (Int 1)
  do! "a := 5; a < 6 ? 1 : 2" ==> (Int 1)
  do! "fac := n -> n == 0 ? 1 : n * fac (n - 1); fac 4" ==> Int 24
  do! "r := { a := 10; b := 2 + 3; }; r.b" ==> Int 5
  do! "r := { f := n -> n * 2; }; r.f 2" ==> Int 4
  do! "r := { f := n -> n * 2; g := n -> f (n + 1); }; (r.g 2)" ==> Int 6
  do! "f := x -> y -> x + y; 10 |> f 5" ==> Int 15
  do! "[1, 2, 3 + 4]" ==> List (FunnyList.ofArray [| Int 1; Int 2; Int 7 |])
}

let typeTest = test "type test" {
  do! "typeof 1" ==> Type IntType
  do! "typeof 1.0" ==> Type FloatType
  do! "typeof (x -> x * x)" ==> Type FuncType
  do! "typeof true" ==> Type BoolType
  do! "typeof true == bool" ==> True
}