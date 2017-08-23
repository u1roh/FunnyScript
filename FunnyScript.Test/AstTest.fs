module FunnyScript.Test.AstTest
open Persimmon
open FunnyScript

let expr x = { Value = x; Position = None }
let binaryOp (op, x1, x2) = BinaryOp (op, expr x1, expr x2)
let def (name, x1, x2) = Let (name, expr x1, expr x2)
let apply (x1, x2) = Apply (expr x1, expr x2, NormalApply)
let ifexpr (x1, x2, x3) = If (expr x1, expr x2, expr x3)
let combine (x1, x2) = Combine (expr x1, expr x2)

let a1 = binaryOp (Plus, Obj (Int 1), Obj (Int 2))

let a2 =
  def ("a", Obj (Float 1.23),
    def ("b", Obj (Float 3.21),
      binaryOp (Plus, Ref "a", Ref "b")))

let a3 =
  def ("b", Obj (Int 10),
    def ("f", FuncDef { Args = ["a"]; Body = binaryOp (Plus, Ref "a", Ref "b") |> expr },
      apply (Ref "f", Obj (Int 3))))

let record =
  NewRecord ([ ("a1", expr a1); ("a2", expr a2); ("a3", expr a3) ])

let refMember =
  RefMember (expr record, "a2")

// ------------------------


let testAST = test "FunnyScript AST test" {
  let env = Script.Env.Default;
  let eval x = env.Eval (expr x)
  do!
    binaryOp (Plus, Obj (Int 1), Obj (Int 2))
    |> eval |> assertEquals (Ok (Int 3))
  do!
    def ("a", Obj (Int 123),
      def ("b", Obj (Int 321),
        binaryOp (Plus, Ref "a", Ref "b")))
    |> eval |> assertEquals (Ok (Int 444))
  do!
    a3
    |> eval |> assertEquals (Ok (Int 13))
  do!
    ifexpr (Obj True,  Obj (Int 1), Obj (Int 0))
    |> eval |> assertEquals (Ok (Int 1))
  do!
    ifexpr (Obj False, Obj (Int 1), Obj (Int 0))
    |> eval |> assertEquals (Ok (Int 0))
  do!
    ifexpr (binaryOp (Equal, Obj (Int 3), a1), Obj (ClrObj "equal"), Obj (ClrObj "not equal"))
    |> eval |> assertEquals (Ok (ClrObj "equal"))
  do!
    def ("a", Obj (Int 10),
      combine (
        apply (Ref "trace", Ref "a"),
        combine (
          def ("a", Obj (Float 3.14), apply (Ref "trace", Ref "a")),
          Ref "a")))
    |> eval |> assertEquals (Ok (Int 10))
  do!
    def ("hoge", FuncDef ({ Args = ["a"]; Body = binaryOp (Plus, Obj (Int 100), Ref "a") |> expr }),
      apply (Ref "hoge", Obj (Int 22)))
    |> eval |> assertEquals (Ok (Int 122))
  do!
    let body = ifexpr (binaryOp (Equal, Ref "n", Obj (Int 0)), Obj (Int 1), binaryOp (Mul, Ref "n", apply (Ref "fac", binaryOp (Minus, Ref "n", Obj(Int 1)))))
    def ("fac", FuncDef ({ Args = ["n"]; Body = expr body }),
      apply (Ref "fac", Obj (Int 4)))
    |> eval |> assertEquals (Ok (Int 24))
  do!
    def ("a", Obj (List (FunnyList.ofArray [| Int 123; Int 321; Int 456 |])), apply (Ref "a", Obj (Int 1)))
    |> eval |> assertEquals (Ok (Int 321))
}
