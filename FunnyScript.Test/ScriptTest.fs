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
  |> Script.forStr Script.defaultEnv
  |> assertEquals (Ok expected)


let testScript = test "scripting test" {
  do! "1 + 2 * (3 + 4)" ==> (Int 15)
  do! "a := 1 + 2; b := 3 + 4; a * b" ==> (Int 21)
  do! "f := a -> a + 1; f 2" ==> (Int 3)
  do! "f := a -> b -> a * b; f 3 4" ==> (Int 12)
  do! "? true => 1 | 2" ==> (Int 1)
  do! "a := 5;  ? a < 6 => 1 | 2" ==> (Int 1)
  do! "a := 5; |? a < 4 => 1 | 2" ==> (Int 2)
  do! "fac := n -> ? n == 0 => 1 | n * fac (n - 1); fac 4" ==> Int 24
  do! "r := { a := 10; b := 2 + 3; }; r.b" ==> Int 5
  do! "{ a := 10; b := 2 + 3; }.b" ==> Int 5
  do! "r := { f := n -> n * 2; }; r.f 2" ==> Int 4
  do! "r := { f := n -> n * 2; g := n -> f (n + 1); }; (r.g 2)" ==> Int 6
  do! "f := x -> y -> x + y; 10 |> f 5" ==> Int 15
  do! "a := { b := 1; c := { d := 2; e := 3; }; }; a.c.e" ==> Int 3
  do! "f := () -> 123; f()" ==> Int 123
  do! "f := (a, b, c) -> a + b + c; f (1, 2, 3)" ==> Int 6
}

let typeTest = test "type test" {
  do! "1 :? int" ==> True
  do! "1 :? float" ==> False
  do! "1.0 :? float" ==> True
  do! "(x -> x * x) :? function" ==> True
  do! "true :? bool" ==> True
}

let listTest = test "list test" {
  do! "[1, 2, 3 + 4]" ==> ofArray [| Int 1; Int 2; Int 7 |]
  do! "List.init 3 (i -> 2 * i)" ==> ofArray [| Int 0; Int 2; Int 4 |]
  do! "[].isEmpty" ==> True
  do! "[1, 2].isEmpty" ==> False
  do! "[3, 4].head" ==> Int 3
  do! "[3, 4, 5].tail" ==> ofList [Int 4; Int 5]
  do! "[3, 4, 5].length" ==> Int 3
  do! "a := [1, 3, 5]; b := 10; b :: a" ==> ofList [Int 10; Int 1; Int 3; Int 5]
  do! "a := [1, 3, 5]; b := 10; (b :: a).length" ==> Int 4
  do! "()" ==> Null
  do! "(10)" ==> Int 10
  do! "(10, 20, 30)" ==> ofList [Int 10; Int 20; Int 30]
}

let clrTest = test "CLR reflection test" {
  do! "System :? record" ==> True
  do! "System.Collections :? record" ==> True
  do! "System.Collections.Generic :? record" ==> True
  do! "System.Console :? type" ==> True
  do! "System.Object.ReferenceEquals :? function" ==> True
  do! "System.Object.ReferenceEquals [1, 1]" ==> False
  do! "System.Object.ReferenceEquals (1, 1)" ==> False
  do! "System.Object.Equals [1, 1]" ==> True
  do! "System.Object.Equals (1, 1)" ==> True
  do! "zero := System.Math.Sin 3.14; zero < 0.01" ==> True
  do! "System.Math.Abs -1" ==> Int 1
  do! "System.Math.Abs -1.0" ==> Float 1.0
  do! "System.Console.WriteLine \"[test] Hello, System.Console.WriteLine\"" ==> Null
  do! "System.Console.WriteLine (\"[test] int = {0}, float = {1}\", 123, 3.14)" ==> Null
  do! "s := Stack(); do s.Push 123; s.Peek ()" ==> Int 123
  do! "s := Stack(); do s.Push 123; s.Count" ==> Int 1
  do! "s := System.Collections.Stack.new(); s.Count" ==> Int 0
  do! "System.String.Format (\"int val = {0}\", 987)" ==> ClrObj "int val = 987"
}