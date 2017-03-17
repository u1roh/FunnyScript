module FunnyScript.Samples
open AST

let a1 = Apply (Apply (Ref "+", Obj (Int 1)), Obj (Int 2))

let a2 =
  Let ("a", Obj (Float 1.23),
    Let ("b", Obj (Float 3.21),
      Apply (Apply (Ref "+", Ref "a"), Ref "b")))

let a3 =
  Let ("b", Obj (Int 10),
    Let ("f", FuncDef { Arg = "a"; Body = Apply (Apply (Ref "+", Ref "a"), Ref "b") },
      Apply (Ref "f", Obj (Int 3))))

let plus = BinaryOp (Plus, Obj (Int 1), Obj (Int 2))

let ifExpr1 = If (Obj True,  Obj (Int 1), Obj (Int 0))
let ifExpr2 = If (Obj False, Obj (Int 1), Obj (Int 0))
let ifExpr3 = If (BinaryOp (Equal, Obj (Int 3), plus), Obj (String "equal"), Obj (String "not equal"))
  
let nestLet =
  Let ("a", Obj (Int 10),
    Combine (
      Apply (Ref "trace", Ref "a"),
      Combine (
        Let ("a", Obj (Float 3.14), Apply (Ref "trace", Ref "a")),
        Apply (Ref "trace", Ref "a"))))

let funcDef =
  Let ("hoge", FuncDef ({ Arg = "a"; Body = Apply (Ref "trace", Ref "a") }),
    Apply (Ref "hoge", Obj (Float 3.14)))

let recursiveFunc =
  let body = If (BinaryOp (Equal, Ref "n", Obj (Int 0)), Obj (Int 1), BinaryOp (Mul, Ref "n", Apply (Ref "fac", BinaryOp (Minus, Ref "n", Obj(Int 1)))))
  Let ("fac", FuncDef ({ Arg = "n"; Body = body }),
    Apply (Ref "fac", Obj (Int 4)))

let tuple =
  NewTuple ([| a1; a2; a3 |])

let record =
  NewRecord ([| ("a1", a1); ("a2", a2); ("a3", a3) |])

let refMember =
  RefMember (record, "a2")

// ------------------------

let run caption expr =
  printfn "%s = %A" caption expr
  printfn "%A" (Lib.eval expr)
  printfn "---"

let runAll () =
  run "a1" a1
  run "a2" a2
  run "a3" a3
  run "plus" plus
  run "ifExpr1" ifExpr1
  run "ifExpr2" ifExpr2
  run "ifExpr3" ifExpr3
  run "nestLet" nestLet
  run "funcDef" funcDef
  run "recursiveFunc" recursiveFunc
  run "tuple" tuple
  run "record" record
  run "refMember" refMember
