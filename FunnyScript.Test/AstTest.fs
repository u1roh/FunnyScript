module FunnyScript.Test.AstTest
open Persimmon
open FunnyScript
open FunnyScript.AST

let a1 = BinaryOp (Plus, Obj (Int 1), Obj (Int 2))

let a2 =
  Let ("a", Obj (Float 1.23),
    Let ("b", Obj (Float 3.21),
      BinaryOp (Plus, Ref "a", Ref "b")))

let a3 =
  Let ("b", Obj (Int 10),
    Let ("f", FuncDef { Arg = "a"; Body = BinaryOp (Plus, Ref "a", Ref "b") },
      Apply (Ref "f", Obj (Int 3))))

let tuple =
  NewTuple ([| a1; a2; a3 |])

let record =
  NewRecord ([ ("a1", a1); ("a2", a2); ("a3", a3) ])

let refMember =
  RefMember (record, "a2")

// ------------------------


let testAST = test "FunnyScript AST test" {
  do!
    BinaryOp (Plus, Obj (Int 1), Obj (Int 2))
    |> Script.eval |> assertEquals (Some (Int 3))
  do!
    Let ("a", Obj (Int 123),
      Let ("b", Obj (Int 321),
        BinaryOp (Plus, Ref "a", Ref "b")))
    |> Script.eval |> assertEquals (Some (Int 444))
  do!
    a3
    |> Script.eval |> assertEquals (Some (Int 13))
  do!
    If (Obj True,  Obj (Int 1), Obj (Int 0))
    |> Script.eval |> assertEquals (Some (Int 1))
  do!
    If (Obj False, Obj (Int 1), Obj (Int 0))
    |> Script.eval |> assertEquals (Some (Int 0))
  do!
    If (BinaryOp (Equal, Obj (Int 3), a1), Obj (String "equal"), Obj (String "not equal"))
    |> Script.eval |> assertEquals (Some (String "equal"))
  do!
    Let ("a", Obj (Int 10),
      Combine (
        Apply (Ref "trace", Ref "a"),
        Combine (
          Let ("a", Obj (Float 3.14), Apply (Ref "trace", Ref "a")),
          Ref "a")))
    |> Script.eval |> assertEquals (Some (Int 10))
  do!
    Let ("hoge", FuncDef ({ Arg = "a"; Body = BinaryOp (Plus, Obj (Int 100), Ref "a") }),
      Apply (Ref "hoge", Obj (Int 22)))
    |> Script.eval |> assertEquals (Some (Int 122))
  do!
    let body = If (BinaryOp (Equal, Ref "n", Obj (Int 0)), Obj (Int 1), BinaryOp (Mul, Ref "n", Apply (Ref "fac", BinaryOp (Minus, Ref "n", Obj(Int 1)))))
    Let ("fac", FuncDef ({ Arg = "n"; Body = body }),
      Apply (Ref "fac", Obj (Int 4)))
    |> Script.eval |> assertEquals (Some (Int 24))
}
