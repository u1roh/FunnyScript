module FunnyScript.Test.AstTest
open Persimmon
open FunnyScript

let expr x = { Value = x; Position = None }
let invoke arg f = Apply (expr f, expr arg, NormalApply)
let binaryOp (op, x1, x2) = Ref op |> invoke x1 |> invoke x2
let def (name, x1, x2) = Let (name, expr x1, expr x2)
let apply (x1, x2) = Apply (expr x1, expr x2, NormalApply)
let ifexpr (x1, x2, x3) = If (expr x1, expr x2, expr x3)
let combine (x1, x2) = Combine (expr x1, expr x2)

let a1 = binaryOp ("+", Obj (box 1), Obj (box 2))

let a2 =
  def ("a", Obj (box 1.23),
    def ("b", Obj (box 3.21),
      binaryOp ("+", Ref "a", Ref "b")))

let a3 =
  def ("b", Obj (box 10),
    def ("f", FuncDef { Args = ["a"]; Body = binaryOp ("+", Ref "a", Ref "b") |> expr },
      apply (Ref "f", Obj (box 3))))

let record =
  NewRecord ([ ("a1", expr a1); ("a2", expr a2); ("a3", expr a3) ])

let refMember =
  RefMember (expr record, "a2")

// ------------------------


let testAST = test "FunnyScript AST test" {
  let env = Script.Env.Default;
  let eval x = env.Eval (expr x)
  do!
    binaryOp ("+", Obj (box 1), Obj (box 2))
    |> eval |> assertEquals (Ok (box 3))
  do!
    def ("a", Obj (box 123),
      def ("b", Obj (box 321),
        binaryOp ("+", Ref "a", Ref "b")))
    |> eval |> assertEquals (Ok (box 444))
  do!
    a3
    |> eval |> assertEquals (Ok (box 13))
  do!
    ifexpr (Obj (box true),  Obj (box 1), Obj (box 0))
    |> eval |> assertEquals (Ok (box 1))
  do!
    ifexpr (Obj (box false), Obj (box 1), Obj (box 0))
    |> eval |> assertEquals (Ok (box 0))
  do!
    ifexpr (binaryOp ("==", Obj (box 3), a1), Obj "equal", Obj "not equal")
    |> eval |> assertEquals (Ok (box "equal"))
  do!
    def ("a", Obj (box 10),
      combine (
        apply (Ref "trace", Ref "a"),
        combine (
          def ("a", Obj (box 3.14), apply (Ref "trace", Ref "a")),
          Ref "a")))
    |> eval |> assertEquals (Ok (box 10))
  do!
    def ("hoge", FuncDef ({ Args = ["a"]; Body = binaryOp ("+", Obj (box 100), Ref "a") |> expr }),
      apply (Ref "hoge", Obj (box 22)))
    |> eval |> assertEquals (Ok (box 122))
  do!
    let body = ifexpr (binaryOp ("==", Ref "n", Obj (box 0)), Obj (box 1), binaryOp ("*", Ref "n", apply (Ref "fac", binaryOp ("-", Ref "n", Obj(box 1)))))
    def ("fac", FuncDef ({ Args = ["n"]; Body = expr body }),
      apply (Ref "fac", Obj (box 4)))
    |> eval |> assertEquals (Ok (box 24))
  do!
    def ("a", Obj [| box 123; box 321; box 456 |], apply (Ref "a", Obj (box 1)))
    |> eval |> assertEquals (Ok (box 321))
}
