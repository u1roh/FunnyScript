module FunnyScript.Test.AstTest
open FunnyScript
open NUnit.Framework

let invoke arg f = Apply (f, arg)
let binaryOp (op, x1, x2) = Ref op |> invoke x1 |> invoke x2

let a1 = binaryOp ("+", Obj (box 1), Obj (box 2))

let a2 =
  Let ("a", Obj (box 1.23),
    Let ("b", Obj (box 3.21),
      binaryOp ("+", Ref "a", Ref "b")))

let a3 =
  Let ("b", Obj (box 10),
    Let ("f", FuncDef { Args = XNamed ("a", XAny); Body = binaryOp ("+", Ref "a", Ref "b") },
      Apply (Ref "f", Obj (box 3))))

let record =
  NewRecord ([ ("a1", a1); ("a2", a2); ("a3", a3) ])

let refMember =
  RefMember (record, "a2")

// ------------------------


[<Test>]
let testAST () =
  let assertEquals expected actual =
    match Script.Env.Default.Eval actual with
    | Ok actual -> Assert.AreEqual(box expected, actual)
    | Error e -> e.ToString() |> Assert.Fail

  binaryOp ("+", Obj (box 1), Obj (box 2))
  |> assertEquals 3

  Let ("a", Obj (box 123),
    Let ("b", Obj (box 321),
      binaryOp ("+", Ref "a", Ref "b")))
  |> assertEquals 444

  a3
  |> assertEquals 13

  If (Obj (box true),  Obj (box 1), Obj (box 0))
  |> assertEquals 1

  If (Obj (box false), Obj (box 1), Obj (box 0))
  |> assertEquals 0

  If (binaryOp ("==", Obj (box 3), a1), Obj "equal", Obj "not equal")
  |> assertEquals "equal"

  Let ("hoge", FuncDef ({ Args = XNamed ("a", XAny); Body = binaryOp ("+", Obj (box 100), Ref "a") }),
    Apply (Ref "hoge", Obj (box 22)))
  |> assertEquals 122

  Let ("a", Obj [| box 123; box 321; box 456 |], Apply (Ref "a", Obj (box 1)))
  |> assertEquals 321
