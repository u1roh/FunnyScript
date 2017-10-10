module FunnyScript.Test.AstTest
open Persimmon
open FunnyScript

let invoke arg f = Apply (f, arg)
let binaryOp (op, x1, x2) = Ref op |> invoke x1 |> invoke x2

let a1 = binaryOp ("+", Obj (box 1), Obj (box 2))

let a2 =
  Let ("a", Obj (box 1.23),
    Let ("b", Obj (box 3.21),
      binaryOp ("+", Ref "a", Ref "b")))

let a3 =
  Let ("b", Obj (box 10),
    Let ("f", FuncDef { Args = Named ("a", Any); Body = binaryOp ("+", Ref "a", Ref "b") },
      Apply (Ref "f", Obj (box 3))))

let record =
  NewRecord ([ ("a1", a1); ("a2", a2); ("a3", a3) ])

let refMember =
  RefMember (record, "a2")

// ------------------------


let testAST = test "FunnyScript AST test" {
  let env = Script.Env.Default;
  let eval x = env.Eval x
  do!
    binaryOp ("+", Obj (box 1), Obj (box 2))
    |> eval |> assertEquals (Ok (box 3))
  do!
    Let ("a", Obj (box 123),
      Let ("b", Obj (box 321),
        binaryOp ("+", Ref "a", Ref "b")))
    |> eval |> assertEquals (Ok (box 444))
  do!
    a3
    |> eval |> assertEquals (Ok (box 13))
  do!
    If (Obj (box true),  Obj (box 1), Obj (box 0))
    |> eval |> assertEquals (Ok (box 1))
  do!
    If (Obj (box false), Obj (box 1), Obj (box 0))
    |> eval |> assertEquals (Ok (box 0))
  do!
    If (binaryOp ("==", Obj (box 3), a1), Obj "equal", Obj "not equal")
    |> eval |> assertEquals (Ok (box "equal"))
  do!
    Let ("hoge", FuncDef ({ Args = Named ("a", Any); Body = binaryOp ("+", Obj (box 100), Ref "a") }),
      Apply (Ref "hoge", Obj (box 22)))
    |> eval |> assertEquals (Ok (box 122))
  do!
    let body = If (binaryOp ("==", Ref "n", Obj (box 0)), Obj (box 1), binaryOp ("*", Ref "n", Apply (Ref "fac", binaryOp ("-", Ref "n", Obj(box 1)))))
    Let ("fac", FuncDef ({ Args = Named ("n", Any); Body = body }),
      Apply (Ref "fac", Obj (box 4)))
    |> eval |> assertEquals (Ok (box 24))
  do!
    Let ("a", Obj [| box 123; box 321; box 456 |], Apply (Ref "a", Obj (box 1)))
    |> eval |> assertEquals (Ok (box 321))
}
