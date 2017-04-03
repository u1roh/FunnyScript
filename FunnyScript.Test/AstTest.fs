module FunnyScript.Test.AstTest
open Persimmon
open FunnyScript

let a1 = BinaryOp (Plus, Obj (Int 1), Obj (Int 2))

let a2 =
  Let ("a", Obj (Float 1.23),
    Let ("b", Obj (Float 3.21),
      BinaryOp (Plus, Ref "a", Ref "b")))

let a3 =
  Let ("b", Obj (Int 10),
    Let ("f", FuncDef { Args = ["a"]; Body = BinaryOp (Plus, Ref "a", Ref "b") },
      Apply (Ref "f", Obj (Int 3))))

let record =
  NewRecord ([ ("a1", a1); ("a2", a2); ("a3", a3) ])

let refMember =
  RefMember (record, "a2")

// ------------------------


let testAST = test "FunnyScript AST test" {
  do!
    BinaryOp (Plus, Obj (Int 1), Obj (Int 2))
    |> Script.eval |> assertEquals (Ok (Int 3))
  do!
    Let ("a", Obj (Int 123),
      Let ("b", Obj (Int 321),
        BinaryOp (Plus, Ref "a", Ref "b")))
    |> Script.eval |> assertEquals (Ok (Int 444))
  do!
    a3
    |> Script.eval |> assertEquals (Ok (Int 13))
  do!
    If (Obj True,  Obj (Int 1), Obj (Int 0))
    |> Script.eval |> assertEquals (Ok (Int 1))
  do!
    If (Obj False, Obj (Int 1), Obj (Int 0))
    |> Script.eval |> assertEquals (Ok (Int 0))
  do!
    If (BinaryOp (Equal, Obj (Int 3), a1), Obj (ClrObj "equal"), Obj (ClrObj "not equal"))
    |> Script.eval |> assertEquals (Ok (ClrObj "equal"))
  do!
    Let ("a", Obj (Int 10),
      Combine (
        Apply (Ref "trace", Ref "a"),
        Combine (
          Let ("a", Obj (Float 3.14), Apply (Ref "trace", Ref "a")),
          Ref "a")))
    |> Script.eval |> assertEquals (Ok (Int 10))
  do!
    Let ("hoge", FuncDef ({ Args = ["a"]; Body = BinaryOp (Plus, Obj (Int 100), Ref "a") }),
      Apply (Ref "hoge", Obj (Int 22)))
    |> Script.eval |> assertEquals (Ok (Int 122))
  do!
    let body = If (BinaryOp (Equal, Ref "n", Obj (Int 0)), Obj (Int 1), BinaryOp (Mul, Ref "n", Apply (Ref "fac", BinaryOp (Minus, Ref "n", Obj(Int 1)))))
    Let ("fac", FuncDef ({ Args = ["n"]; Body = body }),
      Apply (Ref "fac", Obj (Int 4)))
    |> Script.eval |> assertEquals (Ok (Int 24))
  do!
    Let ("a", Obj (List (FunnyList.ofArray [| Int 123; Int 321; Int 456 |])), Apply (Ref "a", Obj (Int 1)))
    |> Script.eval |> assertEquals (Ok (Int 321))
}
