module FunnyScript.Test.ScriptTest
open Persimmon
open FunnyScript
open System

let env = Script.Env.Default

let (==>) (script : string) expected =
  env.Run ("ScriptTest", script)
  |> assertEquals (Ok (box expected))

let (==>!) (script : string) error =
  let rec strip e = match e with StackTrace (e, _, _) -> strip e | _ -> e
  match env.Run ("ScriptTest", script) with
  | Error (Script.RuntimeError e) -> strip e |> assertEquals error
  | Error e -> fail (sprintf "unexpected error: %A" e)
  | Ok _ -> fail "Error expected, but succceeded"

let literalTest = test "literal test" {
  do! "1." ==> 1.0
  do! "-1" ==> -1
  do! "- 2" ==> -2
  do! "- -3" ==> 3
}

let testScript = test "scripting test" {
  do! "1 + 2 * (3 + 4)" ==> 15
  do! "a := 1 + 2; b := 3 + 4; a * b" ==> 21
  do! "f := a -> a + 1; f 2" ==> 3
  do! "f := a -> b -> a * b; f 3 4" ==> 12
  do! "? true => 1 | 2" ==> 1
  do! "a := 5;  ? a < 6 => 1 | 2" ==> 1
  do! "a := 5; |? a < 4 => 1 | 2" ==> 2
  do! "fac := n -> ? n == 0 => 1 | n * fac (n - 1); fac 4" ==> 24
  do! "r := { a := 10; b := 2 + 3; }; r.b" ==> 5
  do! "{ a := 10; b := 2 + 3; }.b" ==> 5
  do! "r := { f := n -> n * 2; }; r.f 2" ==> 4
  do! "r := { f := n -> n * 2; g := n -> f (n + 1); }; (r.g 2)" ==> 6
  do! "f := x -> y -> x + y; 10 |> f 5" ==> 15
  do! "a := { b := 1; c := { d := 2; e := 3; }; }; a.c.e" ==> 3
  do! "f := () -> 123; f()" ==> 123
  do! "f := (a, b, c) -> a + b + c; f (1, 2, 4)" ==> 7
  do! "a := 1; -a" ==> -1
  do! "a := b := 100; b + 20; a" ==> 120
  do! "f := a := 100; x -> x + a; f 10" ==> 110

  // 関数適用の文法で掛け算が出来る
  do! "2 3" ==> 6
  do! "1.2 2" ==> 2.4
  do! "2(3 + 4)" ==> 14
  do! "a := 12.3; 2a" ==> 24.6
  do! "a := 5; b := 3; a b" ==> 15

  // if 式は else が省略できる（省略された場合は Unmatched エラーを返す）
  do! "? true => 123" ==> 123
  do! "? false => 123" ==>! Unmatched
  do! "f := n -> ? n % 2 == 0 => \"yes\"; f 4" ==> "yes"
  do! "f := n -> ? n % 2 == 0 => \"yes\"; f 3" ==>! Unmatched
}

let typeTest = test "type test" {
//  do! "1 :? int" ==> true
//  do! "1 :? float" ==> false
//  do! "1.0 :? float" ==> true
  do! "(x -> x * x) :? function" ==> true
//  do! "true :? bool" ==> true
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
  do! "[1, 2, 3, 4] |> choose (x -> ? x % 2 == 0 => () | x)" ==> [|1; 3|]
  do! "[1, 2, 3, 4] |> fold 0 `+`" ==> 10
  do! "[1 .. 5]" ==> [| 1; 2; 3; 4; 5 |]
  //do! "[1..5]" ==> ofList [1; 2; 3; 4; 5]
  do! "[1+2 .. 8-2]" ==> [| 3; 4; 5; 6 |]

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
}

let identifierTest = test "identifier test" {
  do! "`+` 2 4" ==> 6
  do! "`I love F#` := 123; `I love F#`" ==> 123
  do! "`×` := x -> y -> x * y; `×` 3 5" ==> 15  // 掛け算の演算子なのだけどエックスと区別がつかない…
  do! "`+` := x -> y -> x - y; 7 + 3" ==> 4 // + 演算子を引き算に上書き
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
  do! "s := Stack(); do s.Push 123; s.Peek ()" ==> 123
  do! "s := Stack(); do s.Push 123; s.Count" ==> 1
  do! "s := System.Collections.Stack.new(); s.Count" ==> 0
  do! "System.String.Format (\"int val = {0}\", 987)" ==> "int val = 987"
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
    timer := System.Timers.Timer.new();
    do timer.Enabled <- true;
    timer.Enabled
    """ ==> true
}

let methodChainTest = test "method chain test" {
  do! """
    sb := System.Text.StringBuilder.new();
    sb.Append "a"
    |> sb -> sb.Append "b"
    |> sb -> sb.Append "c"
    |> sb -> sb.ToString()
    """ ==> "abc"
  do! """
    sb := System.Text.StringBuilder.new();
    sb.Append "x"
    |> .Append "y"
    |> .Append "z"
    |> .ToString()
    """ ==> "xyz"
}

let classTest = test "class test" {
  do! """
    Adder := class (n -> n) { add := this -> n -> this + n; };
    a := Adder.new 8;
    a.add 2
  """ ==> 10

  do! """
    Person := class ((first, last) -> { first_name := first; last_name := last; }) {
      fullname := self ->
        System.Text.StringBuilder.new()
        |> .Append self.first_name
        |> .Append " "
        |> .Append self.last_name
        |> .ToString();
    };
    charlie := Person.new ("Charlie", "Parker");
    charlie.fullname
  """ ==> "Charlie Parker"
}

let errorTest = test "error test" {
  do! """
    f := a -> a + 1;
    b := f ();  // エラー発生 => 即時終了せずに b にエラー情報が入る。 
    ()  // 全体としては null を返すので b に格納されたエラー情報はスルーされる。
  """ ==> null

  do! """
    sqrt := x -> ? x < 0 => error "x must be positive" | System.Math.Sqrt x;
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
}

let castTest = test "cast test" {
  do! "Cast.int 3.14" ==> 3
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
      x -> ? x % 2 == 1 => 2 * x
    ]
  """ ==>! Unmatched

  do! """
    f := match [
      (x, y) -> x + y,
      x -> ? x % 2 == 1 => 2 * x
    ];
    f (3, 6)
  """ ==> 9
}