# FunnyScript

* F# で作ったスクリプト言語です。
* 開発環境は Visual Studio 2015 です。
* パーザーは FParsec を利用しています。
* 関数型です。式ベースで、文はありません。
* 動的型付きのスクリプト言語です。
* 正格評価です。遅延評価はしません。
* .NET Framework の機能（クラスやメソッド）を呼び出せます。
* 簡単なオブジェクト指向機能（クラスの定義）を備えています。
* 例外はありません。（が、バグにより .NET の例外が出てしまうことがあるかもしれません(^^;）
* 簡単なGUIエディタを備えていて、手軽に動かせます。
* 別の .NET アプリケーションに組み込むことが出来ます。（たぶん、出来るはず…）

完成度はとても実用できるレベルではありませんが、「こいつ…動くぞ…」という程度には動く、と思います。
（という表現で雰囲気が伝われば良いのですが）

作者（私）は言語処理系については素人で、これは私が初めて作った言語です。
自分で実際に作ってみると「既存の言語は実によく出来ているんだなぁ」と感じることしばしばで、プログラミング言語に対する感謝と畏敬の念が育まれました。

## 動機

動機は、まずひとつは自分の興味・関心のためです。
一度は言語を自作してみたいという興味で作りました。
それ以外の動機は…。

* 子供が学びやすい言語を作ってみたい
* 遊び感覚で学べるという方向性ではなくて、歴史的事情や計算機のパフォーマンスのための事情などを極力廃して、素直に論理を記述できる言語
* 父親が作った言語で子供がプログラミングを学ぶっていう憧れのシチュエーション（単に親のエゴともいうｗ）
* 数学の学習とのシナジー効果が期待できそうな手軽な関数型言語を考えてみたい。
* 英語（自然言語）っぽく読める/書けるのは（特に子供には）意味がない。英単語による予約語（キーワード）に頼らない文法を考えてみたい。


## 文法

### 予約語
予約語は次の6つしかありません。

`do`, `if`, `else`, `in`, `open`, `load`

### Hello World
```
do System.Console.WriteLine "Hello World";
```

このように普通に .NET の `Console` クラスが利用できます。

### コメント
```
// コメントです
```

`/* ... */` のような範囲指定のコメントは（まだ）ありません。
別に深い意図はなくて、単にサボって作っていないだけです。

### リテラル
```
123     // 整数値
3.14    // 浮動小数点数
"hello" // 文字列
true    // 真
false   // 偽
()      // null
(a, b)  // タプル
[a, b]  // 配列
```

~~文字列リテラルはバックスラッシュによるエスケープ処理とか未実装なのでダメダメです＞＜~~

プルリク頂いて文字列リテラルにエスケープ処理が実装されました！

### 演算子
演算子を設計するに当たり、2つの指針を立てました。

* 言語によって混乱があるので、 思い切って `=` 演算子は使わないことにする
* 概ね C#/Java/C/C++ プログラマにとって違和感が少ない演算子にする

`=` という演算子は、言語によって「変数定義」「代入」「同値判定」など異なる用途に使われており、しばしば混乱を招きます。
そこで FunnyScript では `=` という演算子は使わないことにしました。
* 変数定義は `:=` で行います。
* 代入は `<-` で行います。
* 同値判定は `==` で行います。

後は概ね C#/Java/C/C++ と似たような演算子体系です。

* 同値判定は `==`, `!=`
* 比較は `<`, `>`, `<=`, `>=`
* 否定は `!`
* 論理演算は `&&`, `||`
* 四則演算等は `+`, `-`, `*`, `/`, `%`

※ ビット演算子は（まだ）定義されていません。

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

ラムダ式の利用例として、配列の要素をラムダ式で `map` するコードを示します。
（このコードにはまだ説明していない `|>` 演算子や `map` 関数が出てきていますが、雰囲気で読んで下さい。）
```
[1, 2, 3] |> map (x -> 2 * x) // [2, 4, 6]
```

### 簡易ラムダ式
上記のラムダ式も十分簡潔ですが、1変数関数に限っては更に簡潔に書ける文法も用意しました。

```
add10 := x -> x + 10; // x に10を足す関数
add10 := | @ + 10; // こう書いても同じです
```
`|` が簡易ラムダ式の開始の記号です。
`@` が暗黙的に定義された引数です。

これを使うと、先ほどの配列を map するコードは次のように書けます。
```
[1, 2, 3] |> map (| 2 * @) // [2, 4, 6]
```
引数に特に意味のある名前が不要な場合に、この簡易記法が使えます。

### 条件分岐
```
result := if x % 2 == 0 => "even" else "odd";
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
遅延評価の言語ならYコンビネータで再帰が実現できるようですが、この言語は正格評価なので。

### 末尾再帰最適化
出来ているっぽいです。

F# の末尾再帰最適化が効くような書き方にして後はF#コンパイラにお任せしている感じです。
次のようなコードが Stack Overflow することなく延々と動きます。

```
iter := n -> do System.Console.WriteLine n; iter (n + 1);
do iter 0;
```


### レコード
次のように中括弧 `{` `}` で囲むことでレコードオブジェクトが作れます。
```
a := { name := "u1roh"; age := 40; };
do System.Console.WriteLine ("name = {0}, age = {1}", a.name, a.age);
```

この応用でモジュール（関数などをグループ化したもの）も作れます。

```
Calc := {
  add := x -> y -> x + y;
  sub := x -> y -> x - y;
};
do System.Console.WriteLine ("add: {0}", Calc.add 7 9);
do System.Console.WriteLine ("sub: {0}", Calc.sub 8 2);
```

つまり FunnyScript はレコードとモジュールに文法の違いはありません。
単に `{` `}` によってグループ化出来る機能を提供するだけであり、それをレコードとして使うのもモジュールとして使うのもユーザーの自由です。

### 配列
中身は普通の .NET の配列オブジェクトです。

```
a := [1, 2, 3];  // リテラル
len := a.Length; // 3
type := a.GetType();  // System.Object[]
b := array 3 (i -> 2 * i);  // [0, 2, 4]
a1 := a 1;  // 2（インデックスによる要素取得）
c := a + b; // [1, 2, 3, 0, 2, 4] （配列の連結）
```

特にインデックスによる要素取得は特徴的です。
カギ括弧の演算子を使って `a[1]` のように表記する言語のほうが一般的かもしれません。
しかし FunnyScript ではカギ括弧を使わずに単に `a 1` と表記します。
これは関数適用と同じ表記です。
つまり、配列は「インデックス（整数）を引数に与えると要素を返す関数」として扱うことが出来ます。

例として、次のコードは `countries` がラムダ式として `map` の引数に与えられています。
```
countries := ["Japan", "China", "USA", "Russia"];
[2, 0, 3] |> map countries // ["USA", "Japan", "Russia"]
```

なお、関数型言語によくある単方向連結リストの機能は用意していません。

### パイプライン演算子
F#er としては外せない演算子です。
```
add := x -> y -> x + y;
a := 10 |> add 5; // 15
```

### 繰り返し
リストの `foreach` 関数で書けます。
```
do [1, 3, 5, 7] |> foreach System.Console.WriteLine;
```

### .NET Framework の呼び出し
#### 関数呼び出し
既にサンプル中に `System.Console.WriteLine` の呼び出しは登場しています。
同様に他の関数も呼び出せます。

```
a := System.Math.Abs (-1); // 1
b := System.Math.Sin 3.14; // 0.00159...
```

#### namespace の open
```
open System;
a := Math.Abs (-1); // 1
b := Math.Sin 3.14; // 0.00159...
```

#### クラスのインスタンス生成
```
open System.Collections;
stack := Stack();
do stack.Push 123;
do stack.Push 456;
n := stack.Count;
```
クラスがそのままコンストラクタとして扱われます。
クラスにコンストラクタ引数（上記の場合は `()`）を渡すとインスタンスが生成されます。

もちろん、次のようにコンストラクタに `()` ではない引数を渡すことも可能です。
```
s := System.Text.StringBuilder "Foo";
do s.Append "Bar";
do s.Append "Buzz";
```

#### ジェネリッククラスのインスタンス生成
```
open System;
open System.Collections.Generic;

a := Stack Int32(); // Stack<int> の生成
do a.Push 12;

b := Dictionary (String, Double) (); // Dictionary<string, double> の生成
do b.Add ("PI", 3.14159);
do b.Add ("e", 2.718);
```
次のような形式でインスタンスが生成されていることがわかります。
```
ジェネリッククラス 型引数 コンストラクタ引数
```
つまり、ジェネリッククラスは次のようにカリー化された関数であると考えることが出来ます。
```
ジェネリッククラス : 型引数 -> コンストラクタ引数 -> インスタンス
```

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
