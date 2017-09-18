# FunnyScript

* F# で作ったスクリプト言語です。ド素人が勉強のために作った習作なので、ヘボいです。
* 開発環境は Visual Studio 2015 です。
* パーザーは FParsec を利用しています。
* ろくに勉強しないで勘で作ったので、ツッコミどころ満載だと思います。お手柔らかにお願いします。
* 関数型です（たぶん、そのつもり）。式ベースで、文はありません。
* 動的型付きのスクリプト言語です。静的型付き言語を作る能力は私にはありません＞＜
* 正格評価です。遅延評価はしません。
* これといって特筆すべき特長はありません。
* キーワード（予約語）がない言語にしてみようかなと思っていましたが、結局 do キーワードだけは存在しているという中途半端っぷりです。
* 文法を間違えると意味不明なエラーメッセージが出るのでデバッグ不能です。
* とても実用できる完成度ではありません。オモチャです。

オモチャですがとりあえず「言語を自作したことがあるか」という質問に Yes と答えても良さそうになったので僕は大変満足です、はい。
自分で実際に作ってみると「既存の言語は実によく出来ているんだなぁ」と感じることしばしばで、プログラミング言語に対する感謝と畏敬の念が育まれました。

## 文法

### Hello World
```
do System.Console.WriteLine "Hello World";
```

### コメント
```
// コメントです
```

### リテラル
```
123     // 整数値
3.14    // 浮動小数点数
"hello" // 文字列
true    // 真
false   // 偽
()      // null
(a, b)  // タプル
[a, b]  // リスト
```

~~文字列リテラルはバックスラッシュによるエスケープ処理とか未実装なのでダメダメです＞＜~~

プルリク頂いて文字列リテラルにエスケープ処理が実装されました！

### 変数束縛
```
a := 1;
b := 3.14;
c := "hello";
```

行末にセミコロンがついていると「文」っぽいですが、これは F# や OCaml などの `in` に相当する役割です。

### 関数定義（ラムダ式）
```
add10 := x -> x + 10; // x に10を足す関数
a := add10 20;  // 呼び出し

add := x -> y -> x + y; // x + y を計算する関数（カリー化）
a := add 10 20;  // 呼び出し

add := (x, y) -> x + y; // x + y を計算する関数（タプル）
a := add (10, 20);  // 呼び出し

hello := () -> System.Console.WriteLine "Hello World"  // 引数なしの関数
do hello ();  // 呼び出し
```

ラムダ式以外に関数を定義する文法はありません。

#### 簡易ラムダ式
```
add10 := x -> x + 10; // x に10を足す関数
add10 := | @ + 10; // こう書いても同じです
```
`|` が簡易ラムダ式の開始の記号です。
`@` が暗黙的に定義された引数です。

### 条件分岐
```
result := if x % 2 == 0 => "even" else "odd";

// ※ 以前は下記のように書く（ちょっと変わった）文法でしたが、普通に if キーワードを使う文法に変えました。
// result := ? x % 2 == 0 => "even" | "odd";
```

普通の if 式とちょっと違うのは、else if の else を省略できることです。

```
fizzbuzz := n ->
  if n % 3 == 0 && n % 5 == 0 => "fizzbuzz"
  if n % 3 == 0 => "fizz"
  if n % 5 == 0 => "buzz"
  else n.ToString();
```

### 再帰呼び出し
```
fac := n -> if n == 0 => 1 else n * fac (n - 1);	// 階乗計算
```

しれっと再帰呼び出ししていますが、内部はやや強引な実装になっています。

`:=` で変数束縛が生じたときに、その値が関数である場合に限って、その変数（つまり関数）に関数内部から参照できるように特別処理されています。具体的に言うと、その関数が内部からアクセスできる「環境」にその変数を後からねじ込んでいる感じです。

この言語では関数定義はラムダ式しか用意していないので、再帰呼び出しを可能にするにはこういう風にするしかないよなぁと思っているのですが…。

### 末尾再帰最適化
出来ているっぽいです。

F# の末尾再帰最適化が効くような書き方にして後はF#コンパイラにお任せしている感じです。
次のようなコードが Stack Overflow することなく延々と動きます。

```
iter := n -> do System.Console.WriteLine n; iter (n + 1);
do iter 0;
```

※ 当初は出来ていないと思っていましたが、単に Debug ビルドの設定で「末尾呼び出しの生成」がONになっていなかっただけでした（ﾄﾎﾎ


### レコード
```
a := { name := "u1roh"; age := 40; };
do System.Console.WriteLine ("name = {0}, age = {1}", a.name, a.age);
```
（↑どーも、40のおじさんです）

この応用でモジュールっぽいものが作れます。

```
Calc := {
  add := x -> y -> x + y;
  sub := x -> y -> x - y;
};
do System.Console.WriteLine ("add: {0}", Calc.add 7 9);
do System.Console.WriteLine ("sub: {0}", Calc.sub 8 2);
```

### リスト
※ 現在 head, tail は使えません。リストは廃止して配列だけにしちゃうかも。

```
a := [1, 2, 3];  // リテラル
b := List.init 3 (i -> 2 * i);  // [0, 2, 4]
c := a.head;  // 先頭要素（この場合は 1）
d := a.tail;  // 2番目以降（この場合は [2, 3])
len := a.length; // 3
a1 := a 1;  // 2（インデックスによる要素取得）
e := 10 :: a; // [10, 1, 2, 3] （cons 演算子）
```

配列とリストの区別はありません。

### パイプライン演算子
F#er としては外せない演算子です。
```
add := x -> y -> x + y;
a := 10 |> add 5; // 15
```

### 繰り返し
リストの `foreach` メソッドで書けます。
```
do [1, 3, 5, 7].foreach (x -> System.Console.WriteLine x);
```

あるいは再帰で書きます。
```
// リストに対して繰り返しを実行する関数
iter := f -> ls ->
	? ls.isEmpty => () | do f ls.head; iter f ls.tail;

// リストの全要素を表示  
do [9, 8, 7, 6] |> iter System.Console.WriteLine;
```

※ まともに使えるようにするにはこのあたりのリストの機能を強化していく必要がありますね。

### .NET Framework の呼び出し
既にサンプル中に `System.Console.WriteLine` の呼び出しは登場しています。
同様に他の関数も呼び出せます。

```
a := System.Math.Abs -1; // 1
b := System.Math.Sin 3.14
```

クラス名後に `.new` を付けるとコンストラクタが呼び出せます。

```
stack := System.Collections.Stack.new();
do stack.Push 123;
do stack.Push 456;
n := stack.Count;
```

↑ジェネリッククラスは使えないので、前時代の遺物である非ジェネリックなコレクションクラスを使います。

### メンバ参照ラムダ式（仮称）
次のような、オブジェクトを受け取ってそのオブジェクトのメンバーを呼び出す関数
```
f := x -> x.member arg1 arg2;
```
を次のように簡潔に定義できるシンタックスシュガーです。
```
f := .member arg1 arg2;
```
次のようなメソッドチェインで効果を発揮します。
```
s :=
  System.Text.StringBuilder.new()
  |> .Append "Hello "
  |> .Append "World, "
  |> .Append "Hello "
  |> .Append "FunnyScript!"
  |> .ToString();
```

### mutable 変数と代入
```
a := mutable 1; // mutable 関数でミュータブル変数を生成
do a <- 2;      // <- 演算子で代入

// ミュータブルなメンバを持つレコード
hoge := { piyo := mutable 1; };
do hoge.piyo <- 2;

// CLRオブジェクトのプロパティの代入も出来ます
timer := System.Timers.Timer.new();
do timer.Enabled <- true;
```

### クラス定義
`class` 関数を使ってクラスを定義することができます。
```
Person := class ((first, last) -> { first_name := first; last_name := last; }) {
  fullname := self ->
	System.Text.StringBuilder.new()
	|> .Append self.first_name
	|> .Append " "
	|> .Append self.last_name
	|> .ToString();
};
charlie := Person.new ("Charlie", "Parker");
do System.Console.WriteLine charlie.fullname;
```
`class` 関数は引数を2つ取ります。

第1引数はコンストラクタです。
上のコードではファーストネームとラストネームをタプルで受け取り、レコードオブジェクトを生成するコンストラクタとなっています。
なお、コンストラクタに取れる引数は1つだけです。複数の引数が必要な場合はタプルで受け取るようにします。

第2引数はメンバーテーブルとなるレコードオブジェクトです。
メンバーは必ず関数とし、その第1引数には自分自身（コンストラクタで生成したもの）を受け取ります。
上のコードでは `self` という名前で受け取っていますが、特に `self` は予約語ではなく名前は何でもよいです。

「継承」はできません。実装する予定もありません。

「簡易ラムダ式」を使うとメソッドは下記のように書くことが出来ます。
```
Person := class ((first, last) -> { first_name := first; last_name := last; }) {
  fullname := |
	System.Text.StringBuilder.new()
	|> .Append @.first_name
	|> .Append " "
	|> .Append @.last_name
	|> .ToString();
};
```
