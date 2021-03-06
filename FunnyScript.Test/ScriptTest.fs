﻿module FunnyScript.Test.ScriptTest
open Persimmon
open FunnyScript
open System
open System.Collections.Generic

type TestEventArgs (data : int) =
  inherit EventArgs ()
  member __.Data = data

type EventSource() =
  let e = Event<TestEventArgs>()
  [<CLIEvent>]
  member __.Event = e.Publish
  member __.Invoke x = e.Trigger (TestEventArgs x)

let env =
  Script.Env.Default
    .LoadAssembly(Reflection.Assembly.GetExecutingAssembly())
    .Add("es", EventSource())
  
type Hoge =
  | Piyo
  | Fuga of int
  with
  static member Piyopiyo (a : int[]) = 1234
  static member Mofumofu (a : Hoge[]) =
    a |> Array.choose (function Fuga i -> Some i | _ -> None) |> Array.sum
  static member NyanNyan (a : double) = a

let wanwan a b = a + b


let (==>) (script : string) expected =
  env.Run ("ScriptTest", script)
  |> assertEquals (Ok (box expected))

let (==>!) (script : string) error =
  match env.Run ("ScriptTest", script) with
  | Error (Script.RuntimeError { Err = e }) -> e |> assertEquals error
  | Error e -> fail (sprintf "unexpected error: %A" e)
  | Ok _ -> fail "Error expected, but succceeded"

let shouldFail script =
  match env.Run ("ScriptTest", script) with
  | Ok _ -> fail "Error expected, but succceeded"
  | _ -> pass ()

let literalTest = test "literal test" {
  do! "1." ==> 1.0
  do! "-1" ==> -1
  do! "- 2" ==> -2
  do! "- -3" ==> 3
}

let commentTest = test "comment test" {
  do! "100//ほげほげ" ==> 100
  do! "200/*ぴよぴよ*/" ==> 200
  do! "/*ぶひぶひ*/300" ==> 300
  do! "System.Math.Floor/*にゃーん*/1.5" ==> 1.0

  do! """
  /*
  ほげほげ
  ぴよぴよ
  */
  123
  """ ==> 123

  do! """
  /*
  // ほげほげ */123
  """ ==> 123

  do! """
  // ほげほげ /* ぴよぴよ
  123
  """ ==> 123
}

let testScript = test "scripting test" {
  do! "1 + 2 * (3 + 4)" ==> 15
  do! "1 + 3.14" ==> 1.0 + 3.14
  do! "3.14 * 2" ==> 6.28
  do! "a := 1 + 2; b := 3 + 4; a * b" ==> 21
  do! "f := a -> a + 1; f 2" ==> 3
  do! "f := | @ + 1; f 2" ==> 3
  do! "f := |@ + 1; f 5" ==> 6  // '|' の後にスペースを空けなくてもOKに
  do! "f := a -> b -> a * b; f 3 4" ==> 12
  do! "f := | b -> @ * b; f 3 4" ==> 12
  do! "f := a -> | a * @; f 3 4" ==> 12
  do! "if (true) 1 else 2" ==> 1
  do! "a := 5; if (a < 6) 1 else 2" ==> 1
  do! "fac := rec| n -> if (n == 0) 1 else n * @ (n - 1); fac 4" ==> 24
  do! "fac := rec f -> n -> if (n == 0) 1 else n * f (n - 1); fac 4" ==> 24
  do! "r := { a := 10; b := 2 + 3; }; r.b" ==> 5
  do! "{ a := 10; b := 2 + 3; }.b" ==> 5
  do! "{ a := x := 3; x + 10; b := -2 - 4 }.a" ==> 13  // 最後のセミコロンは省略可能
  do! "r := { f := n -> n * 2; }; r.f 2" ==> 4
  do! "r := { f := n -> n * 2; g := n -> f (n + 1); }; (r.g 2)" ==> 6
  do! "f := x -> y -> x + y; 10 |> f 5" ==> 15
  do! "a := { b := 1; c := { d := 2; e := 3; }; }; a.c.e" ==> 3
  do! "f := () -> 123; f()" ==> 123
  do! "f := (a, b, c) -> a + b + c; f (1, 2, 4)" ==> 7
  do! "a := 1; -a" ==> -1
  do! "a := b := 100; b + 20; a" ==> 120
  do! "f := a := 100; x -> x + a; f 10" ==> 110

  // レコード内の再帰関数
  do! "r := { fac := rec| n -> if (n == 0) 1 else n * @ (n - 1); }; r.fac 5" ==> 120

  // Fizz Buzz
  // else if と書かなくても if を並べれば良いところが特長
  do! """
    fizzbuzz := n ->
      if (n % 3 == 0 && n % 5 == 0) "fizzbuzz"
      if (n % 3 == 0) "fizz"
      if (n % 5 == 0) "buzz"
      else n.ToString();
    [1, 2, 3, 4, 5] |> map fizzbuzz
  """ ==> [| "1"; "2"; "fizz"; "4"; "buzz" |]

  // 関数適用の文法で掛け算が出来る
  do! "2 3" ==> 6
  do! "1.2 2" ==> 2.4
  do! "2(3 + 4)" ==> 14
  do! "a := 12.3; 2a" ==> 24.6
  do! "a := 5; b := 3; a b" ==> 15

  // if 式は else が省略できる（省略された場合は Unmatched エラーを返す）
  do! "if (true) 123" ==> 123
  do! "if (false) 123" ==>! Unmatched
  do! "f := n -> if (n % 2 == 0) \"yes\"; f 4" ==> "yes"
  do! "f := n -> if (n % 2 == 0) \"yes\"; f 3" ==>! Unmatched

  // Range
  do! "~[0, 10)~.contains 0" ==> true
  do! "~[0, 10)~.min" ==> 0
  do! "~[0, 10)~.max" ==> 9
  do! "~(3, 5]~ |> map (`+` 1)" ==> [| 5; 6 |]

  // 組み込みの Y コンビネータ rec の利用
  do! "~[0, 4]~ |> map (rec| n -> if (n == 0) 1 else n * @ (n - 1))" ==> [| 1; 1; 2; 6; 24 |]
}

let typeTest = test "type test" {
//  do! "1 :? int" ==> true
  do! "1 :? System.Int32" ==> true
//  do! "1 :? float" ==> false
//  do! "1.0 :? float" ==> true
  do! "(x -> x * x) :? std.function" ==> true
//  do! "true :? bool" ==> true
  //do! "[1, 2, 3] :? array" ==> true
}

let listTest = test "list test" {
  do! "[1, 2, 3 + 4]" ==> [| 1; 2; 7 |]
  do! "array 3 (i -> 2 * i)" ==> [| 0; 2; 4 |]
  do! "[] |> isEmpty" ==> true
  do! "[1, 2] |> isEmpty" ==> false
//  do! "[3, 4].head" ==> Int 3
//  do! "[3, 4, 5].tail" ==> ofList [Int 4; Int 5]
  do! "[3, 4, 5] |> length" ==> 3
//  do! "a := [1, 3, 5]; b := 10; b :: a" ==> ofList [Int 10; Int 1; Int 3; Int 5]
//  do! "a := [1, 3, 5]; b := 10; (b :: a).length" ==> Int 4
  do! "a := [1, 2, 3]; a 0" ==> 1
  do! "a := [1, 2, 3]; a 1" ==> 2
  do! "()" ==> null
  do! "(10)" ==> 10
  do! "(10, 20, 30)" ==> [|10; 20; 30|]
  do! "[1, 2, 3] |> map (x -> 2 * x)" ==> [|2; 4; 6|]
  do! "[1, 2, 3] |> map (| 2@)" ==> [|2; 4; 6|]
  do! "[1, 2, 3] |> collect (| [@, 2@])" ==> [|1; 2; 2; 4; 3; 6|]
  do! "[1, 2, 3, 4] |> choose (x -> if (x % 2 == 0) () else x)" ==> [|1; 3|]
  do! "[1, 2, 3, 4] |> choose (| if (@ % 2 == 0) () else @)" ==> [|1; 3|]
  do! "[1, 2, 3, 4] |> choose (| if (@ % 2 == 0) 3@)" ==> [|6; 12|]  // if の条件から漏れた unmatched は除去される
  do! "[1, 2, 3, 4] |> fold 0 `+`" ==> 10
  do! "[1, 2, 3, 4] |> filter (| @ % 2 == 0)" ==> [|2; 4|]
  do! "[2, 4, 6, 8] |> forall (| @ % 2 == 0)" ==> true
  do! "[2, 4, 6, 7] |> forall (| @ % 2 == 0)" ==> false
  do! "[2, 4, 6, 7] |> exists (| @ % 2 == 0)" ==> true
  do! "[2, 4, 6, 7] |> exists (| @ % 2 == 1)" ==> true
  do! "[2, 4, 6, 8] |> forall (| @ % 2 == 1)" ==> false
  do! "[1, 2, 3] + [4, 5]" ==> [|1; 2; 3; 4; 5|]

  do! """
    n := mutable 0;
    do [1, 2, 3] |> foreach (i -> n <- n + i);
    n
  """ ==> 6

  do! "\"hello\" 1" ==> 'e'
  do! "\"hello\"[2]" ==> 'l'
}

let operatorTest = test "operator test" {
  do! "4 % 2" ==> 0
  do! "5 % 2" ==> 1
  do! "true && true" ==> true
  do! "true && false" ==> false
  do! "false && true" ==> false
  do! "4 % 2 == 0 && 6 % 3 == 0" ==> true
  do! "5 % 2 == 0 && 6 % 3 == 0" ==> false
  do! "true || false" ==> true
  do! "false || true" ==> true
  do! "false || false" ==> false
  do! "!true" ==> false
  do! "!false" ==> true

  do! """
  x := mutable 0;
  _ := true || (do x <- 1; false);
  x""" ==> 0

  do! """
  x := mutable 10;
  _ := false && (do x <- 20; false);
  x""" ==> 10

  // 文字列の連結
  do! "\"abc\" + \"123\"" ==> "abc123"

  do! """
  open System.Numerics;
  a := Complex (1.0, 2.0);
  b := Complex (3.0, 4.0);
  a + b
  """ ==> Numerics.Complex (4.0, 6.0)

  do! """
  open System.Numerics;
  a := Complex (1.0, 2.0);
  b := Complex (3.0, 4.0);
  a - b
  """ ==> Numerics.Complex (-2., -2.)

  do! """
  open System.Numerics;
  a := Complex (1.0, 2.0);
  Complex (2.0, 0.0) * a
  """ ==> Numerics.Complex (2.0, 4.0)
}

let identifierTest = test "identifier test" {
  do! "`+` 2 4" ==> 6
  do! "`I love F#` := 123; `I love F#`" ==> 123
  do! "`×` := x -> y -> x * y; `×` 3 5" ==> 15  // 掛け算の演算子なのだけどエックスと区別がつかない…
  do! "`+` := x -> y -> x - y; 7 + 3" ==> 4 // + 演算子を引き算に上書き
  do! "_ := 0; 111" ==> 111 // '_' という識別子の「定義」は出来る
  do! "_ := 0; _" |> shouldFail // '_' という識別子の「参照」は不可
  do! "@ := 0; @" |> shouldFail // '@' という識別子の「定義」は不可
  do! "222 |> |@" ==> 222 // '@' という識別子の「参照」は出来る
}


let clrTest = test "CLR reflection test" {
  do! "System :? record" ==> true
  do! "System.Collections :? record" ==> true
  do! "System.Collections.Generic :? record" ==> true
  do! "System.Console :? type" ==> true
  do! "System.Object.ReferenceEquals :? function" ==> true
  do! "System.Object.ReferenceEquals [1, 1]" ==> false
  do! "System.Object.ReferenceEquals (1, 1)" ==> false
  do! "System.Object.Equals [1, 1]" ==> true
  do! "System.Object.Equals (1, 1)" ==> true
  do! "zero := System.Math.Sin 3.14; zero < 0.01" ==> true
  do! "System.Math.Abs (-1)" ==> 1
  do! "System.Math.Abs (-1.0)" ==> 1.0
  do! "System.Console.WriteLine \"[test] Hello, System.Console.WriteLine\"" ==> null
  do! "System.Console.WriteLine (\"[test] int = {0}, float = {1}\", 123, 3.14)" ==> null
  do! "s := System.Collections.Stack(); do s.Push 123; s.Peek ()" ==> 123
  do! "s := System.Collections.Stack(); do s.Push 123; s.Count" ==> 1
  do! "s := System.Collections.Stack(); s.Count" ==> 0
  do! "System.String.Format (\"int val = {0}\", 987)" ==> "int val = 987"
  do! "open FunnyScript.Test.ScriptTest; Hoge.Piyopiyo [1, 2, 3]" ==> 1234
  do! "open FunnyScript.Test.ScriptTest; Hoge.Mofumofu [Hoge.Piyo, Hoge.Fuga 2, Hoge.Fuga 5]" ==> 2 + 5
  do! "open FunnyScript.Test.ScriptTest; Hoge.NyanNyan 1" ==> 1.0
  do! "open FunnyScript.Test.ScriptTest; wanwan 1 2" ==> 3
}

let mutableTest = test "mutable test" {
  do! "a := mutable 1; a" ==> 1
  do! "a := mutable 1; do a <- 3; a" ==> 3
  do! """
    a := mutable 1;
    f := () -> a <- 2;
    do f();
    a
    """ ==> 2
  do! """
    a := mutable 1;
    get_a := () -> a;
    do get_a() <- 2;
    a
    """ ==> 2
  do! """
    hoge := { piyo := mutable 1; };
    do hoge.piyo <- 2;
    hoge.piyo
    """ ==> 2
  do! """
    timer := System.Timers.Timer();
    do timer.Enabled <- true;
    timer.Enabled
    """ ==> true
  do! """
  a := mutable 1;
  b := mutable 2;
  do {
    a <- 3;
    b <- 4;
  }
  a + b
  """ ==> 7
  do! """
    a := mutable ();
    do a <- 1;
    a
  """ ==> 1
  do! """
    a := array 10 (_ -> mutable ());
    do a 0 <- 111;
    a 0
  """ ==> 111
  do! """
    a := [mutable ()];
    do a 0 <- 222;
    a 0
  """ ==> 222
  do! """
    open System.Collections;
    a := ArrayList();
    do a.Add 0;
    do a 0 <- 333;
    a 0
  """ ==> 333
  do! """
    a := mutable 0;
    b := a;
    do b <- 1;
    (fix a, fix b)
  """ ==> [| 1; 1 |]
  do! """
    a := mutable 0;
    b := a;
    do a <- 1;
    (fix a, fix b)
  """ ==> [| 1; 1 |]
  do! """
    a := mutable 0;
    b := fix a;
    do a <- 1;
    (fix a, b)
  """ ==> [| 1; 0 |]
}

let methodChainTest = test "method chain test" {
  do! """
    sb := System.Text.StringBuilder();
    sb.Append "a"
    |> sb -> sb.Append "b"
    |> sb -> sb.Append "c"
    |> sb -> sb.ToString()
    """ ==> "abc"
  do! """
    sb := System.Text.StringBuilder();
    sb.Append "x"
    |> .Append "y"
    |> .Append "z"
    |> .ToString()
    """ ==> "xyz"
}

let classTest = test "class test" {
  do! """
    Adder := class (n -> n) { add := this -> n -> this + n; };
    a := Adder 8;
    a.add 2
  """ ==> 10

  do! """
    Adder := class (n -> n) { add := | n -> @ + n; };
    a := Adder 7;
    a.add 5
  """ ==> 12

  do! """
    Person := class ((first, last) -> { first_name := first; last_name := last; }) {
      fullname := self ->
        System.Text.StringBuilder()
        |> .Append self.first_name
        |> .Append " "
        |> .Append self.last_name
        |> .ToString();
      fullname2 := |
        System.Text.StringBuilder()
        |> .Append @.first_name
        |> .Append " "
        |> .Append @.last_name
        |> .ToString();
    };
    charlie := Person ("Charlie", "Parker");
    charlie.fullname2
  """ ==> "Charlie Parker"
}

let errorTest = test "error test" {
  do! """
    f := a -> a + 1;
    b := f ();  // エラー発生 => 即時終了せずに b にエラー情報が入る。 
    ()  // 全体としては null を返すので b に格納されたエラー情報はスルーされる。
  """ ==> null

  do! """
    sqrt := x -> if (x < 0) error "x must be positive" else System.Math.Sqrt x;
    sqrt (-1)
  """ ==>! UserError "x must be positive"

  do! """
    f := a -> a + 1;
    b := f ();  // エラー発生 => 即時終了せずに b にエラー情報が入る。 
    b |!> e -> "caught!"
  """ ==> "caught!"

  do! """
    f := a -> a + 1;
    b := f 1;  // 正常終了
    b |!> e -> "caught!" // エラー処理は無視される
  """ ==> 2

  do! "f := | if (@ % 2 == 0) @; f 3" ==>! Unmatched

  do! """
    a := if (1 == 2) 3;
    a |!> e ->
      if (e == Err.Unmatched) "ミスマッチ" else "不明なエラー"
  """ ==> "ミスマッチ"

  do! """
  a := () 1;  // NullReference 発生
  a |!> match [
    #Err.Unmatched -> "ミスマッチ",
    #Err.NullReference -> "ぬるぽ",
    _ -> "不明なエラー"
  ]
  """ ==> "ぬるぽ"

  do! """
    b :=
      a := 0;
      do a <- 1;
      a;
    b |!> #Err.NotMutable -> 123
  """ ==> 123
}

let openTest = test "open test" {
  do! """
    r := { hoge := 123; piyo := "hello"; };
    open r;
    hoge
  """ ==> 123

  do! "open System; Math.Abs (-3.14)" ==> 3.14

  // これも出来るようにしたいが、現状ではクラスを open することは出来ない
  //do! "open System.Math; Abs (-3.14)" ==> Float 3.14

  do! """
  load "C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.5.2\\System.Numerics.dll";
  open System.Numerics;
  Complex != ()
  """ ==> true
}

let castTest = test "cast test" {
  do! "3.14 |> as System.Int32" ==> 3
  do! "0 |> as System.Double" ==> 0.0
  do! "\"123\" |> as System.Int32" ==> 123
  do! "\"3.14\" |> as System.Double" ==> 3.14
}

let pipelineTest = test "pipeline test" {
  do! "-2 |> System.Math.Abs" ==> 2
  do! "() |?> System.Math.Sqrt" ==> null
  do! "() |?> .hoge" ==> null

  do! """
    x := mutable 3;
    do (do x <- x + 1; _ -> ()) (do x <- x * 2; ());  // (3 + 1) * 2 = 8
    x
  """ ==> 8

  do! """
    x := mutable 3;
    do (do x <- x * 2; ()) |> (do x <- x + 1; _ -> ()); // 3 * 2 + 1 = 7
    x
  """ ==> 7
}

let unicodeTest = test "unicode identifier test" {
  do! "α := 1; α" ==> 1
  do! "うんこ := 100; うんこ" ==> 100
}

let patternMatchTest = test "pattern match test" {
  do! "f := (x, y) -> x + y; f 1" ==>! Unmatched

  do! """
    10 |> match [
      (x, y) -> x + y,
      x -> 2 * x
    ]
  """ ==> 20

  do! """
    10 |> match [
      (x, y) -> x + y,
      | 3@
    ]
  """ ==> 30

  do! """
    10 |> match [
      (x, y) -> x + y,
      x -> if (x % 2 == 1) 2 * x
    ]
  """ ==>! Unmatched

  do! """
    10 |> match [
      (x, y) -> x + y,
      | if (@%2 == 0) @ / 2
    ]
  """ ==> 5

  do! """
    f := match [
      (x, y) -> x + y,
      x -> if (x % 2 == 1) 2 * x
    ];
    f (3, 6)
  """ ==> 9

  do! """
    { name := "Jack"; age := 32 } |> { name := n } -> n + " the Ripper"
  """ ==> "Jack the Ripper"

  do! "{ a := { b := (1, 2); c := 3 }; d := 4 } |> { a := { b := (x, y) }} -> x + y" ==> 3
  do! "{ a := { b := (1, 2); c := 3 }; d := 4 } |> (x { a := { b := (_, _) }; d := _ }) -> x.a.b 1 + x.d" ==> 6

  do! "[] |> () -> 0" ==>! Unmatched
  do! "[] |> [] -> 0" ==> 0

  do! "[1] |> (x) -> x" ==> [| 1 |]
  do! "[1] |> [x] -> x" ==> 1

  do! "[1, 2] |> [x, y] -> x + y" ==> 3
  do! "[1, 2, 3] |> [x, ...] -> x" ==> 1
  do! "[1, 2, 3] |> [..., x] -> x" ==> 3
  do! "[1, 2, 3, 4, 5] |> [x, ..., y] -> [x, y]" ==> [| 1; 5 |]
  do! "~[0, 10)~ |> [x, ..., y] -> [x, y]" ==> [| 0; 9 |]

  do! "3.14 |> (:System.Double) -> true" ==> true
  do! "open System; 3.14 |> (:Int32)  -> true" ==>! Unmatched
  do! "open System.Collections; [1, 2, 3] |> (a:IEnumerable) -> a" ==> [|1; 2; 3|]
  do! "Hoge := class (a -> a) {}; Hoge() |> (:Hoge) -> 111" ==> 111
  do! "Hoge := class (a -> a) {}; Hoge() |> (:System.Int32) -> 111" ==>! Unmatched
  do! "(123, 4.56) |> (a : System.Int32, b : System.Double) -> a + b" ==> float 123 + 4.56
  do! "{ a := 123; b := 4.56 } |> { a := a : System.Int32; b := : System.Double } -> a" ==> 123
  do! "[1, 2.5, \"hoge\", 4, 6.3] |> [a:System.Int32, ..., b:System.Double] -> a + b" ==> 7.3
}

let extendTest = test "extend test" {
  do! """
  do extend System.Int32 { square := | @ * @; };
  (12).square
  """ ==> 144

  do! """
  Vector := class ((x, y) -> { x := x; y := y; }) { len2 := | @.x @.x + @.y @.y; };
  do extend Vector { len := | System.Math.Sqrt @.len2; };
  v := Vector (1.0, 2.0);
  v.len
  """ ==> sqrt 5.
}

let evalTest = test "eval test" {
  do! """
    a := 10;
    b := 7;
    eval "a + b"
  """ ==> 17
}

let caseTest = test "case test" {
  do! """
    CaseA := #;
    a := CaseA;
    a |> #CaseA -> 123
  """ ==> 123

  do! """
    CaseA := #; CaseB := #;
    a := CaseA;
    a |> #CaseB -> 123
  """ ==>! Unmatched

  do! """
    Add := # (_, _); Mul := # { width := _; height := _ };
    f := match [
      #Add (a, b) -> a + b,
      #Mul { width := x; height := y } -> x y
    ];
    [Add (1, 2), Mul{ width := 3; height := 4 }, Add (5, 6)] |> map f
  """ ==> [| 3; 12; 11 |]
  
  do! """
    Hoge := {
      CaseA := #;
      CaseB := # (_, _);
    };
    [Hoge.CaseA, Hoge.CaseB (2, 3)] |> map (match [
      #Hoge.CaseA -> 123,
      #Hoge.CaseB (a, b) -> a + b
    ])
  """ ==> [| 123; 5 |]

  do! """
  open FunnyScript.Test.ScriptTest;
  [ Hoge.Piyo, Hoge.Fuga 321 ] |> map (match [
    #Hoge.Piyo -> 1919,
    #Hoge.Fuga a -> a
  ])
  """ ==> [| 1919; 321 |]
  
  do! """
    [3, 1, 4, 2] |> map (match [
      #1 -> "hoge",
      #2 -> "piyo",
      #3 -> "fuga",
      #4 -> "nyan"
    ])
  """ ==> [| "fuga"; "hoge"; "nyan"; "piyo" |]
}

let genericTypeTest = test "generic type test" {
  do! "System.Collections.Generic.List System.Int32" ==> ClrType typeof<List<int>>
  do! "System.Collections.Generic.Dictionary (System.String, System.Int32)" ==> ClrType typeof<Dictionary<string, int>>

  do! """
    open System;
    open System.Collections.Generic;
    a := List Int32 ();
    do a.Add 123;
    do a.Add 456;
    a.ToArray()
  """ ==> [| 123; 456 |]

  do! """
    open System;
    open System.Collections.Generic;
    a := Dictionary (String, Int32) ();
    do a.Add ("hoge", 789);
    a "hoge"
  """ ==> 789
}

let collectionTest = test "collection test" {
  do! """
  a := Set [1, 2, 3];
  a.Count
  """ ==> 3

  do! """
  a := Map [("hoge", 123), ("piyo", 321)];
  a.Count
  """ ==> 2

  do! """
  a := Map [("hoge", 123), ("piyo", 321)];
  a := a.Add ("buzz", 111);
  a "piyo" + a "buzz"
  """ ==> 432
}


let eventTest = test "event test" {
  do! """
  flag := mutable false;
  subscr := es.Event |> foreach (_ -> flag <- true);
  do es.Invoke 0;
  flag
  """ ==> true

  do! """
  list := System.Collections.ArrayList();
  do es.Event
    |> map ((sender, e) -> 2 e.Data)
    |> foreach list.Add;
  do [1, 2, 3] |> foreach es.Invoke;
  list.ToArray()
  """ ==> [| 2; 4; 6 |]

  do! """
  list := System.Collections.ArrayList();
  do es.Event
    |> choose ((sender, e) -> if (e.Data % 2 == 0) () else e.Data)
    |> foreach list.Add;
  do [1, 2, 3, 4] |> foreach es.Invoke;
  list.ToArray()
  """ ==> [| 1; 3 |]

  do! """
  list := System.Collections.ArrayList();
  do es.Event
    |> filter ((sender, e) -> e.Data % 2 == 0)
    |> foreach ((sender, e) -> list.Add e.Data);
  do [1, 2, 3, 4] |> foreach es.Invoke;
  list.ToArray()
  """ ==> [| 2; 4 |]
}
