[<AutoOpen>]
module FunnyScript.ContLib
open System

type ContCallback<'a> = 'a -> unit

type Cont<'a> =
  | Cont of (ContCallback<'a> -> unit)


module Cont =

  let inline invoke callback source =
    match source with Cont f -> f callback

  let inline bind f source =
    Cont <| fun callback ->
      source |> invoke (fun a -> f a |> invoke callback)

  let inline map f source =
    Cont <| fun callback ->
      source |> invoke (fun a -> f a |> callback)

  let inline create cont =
    Cont (fun callback -> cont callback)

  let inline ofFun f =
    Cont (fun callback -> f () |> callback)

  let inline ofValue x =
    Cont (fun callback -> callback x)

  let inline ofOption x =
    Cont (fun callback -> match x with Some x -> callback x | _ -> ())

  let inline guard condition =
    Cont (fun callback -> if condition then callback ())

  let nop = Cont (fun callback -> callback ())

type Builder () =
  member __.Bind (x, f)  = x |> Cont.bind f
  member __.ReturnFrom x = x
  member __.Return x  = Cont.ofValue x
  member __.Zero ()   = Cont.ofValue ()

  // Delay と Run は評価を遅延させるための仕掛け
  member __.Delay f   = f
  member __.Run   f   = Cont.ofValue () |> Cont.bind (fun () -> f ())

  // Combine は if による条件分岐の後に続けて処理を書きたいときに必要になる
  // - x の後に「継続」して f を実行する。その際、x の結果の値は無視する。
  // - 第二引数の f が関数なのは Delay によって処理が遅延されているから（たぶん…）
  member __.Combine (x, f) = x |> Cont.bind (fun _ -> f ())

  // use 構文が使えるようにする。
  // f x で得られる計算処理の終了時に x.Dispose () を呼べば良いはず。
  member __.Using (x: #IDisposable, f : _ -> Cont<_>) =
    Cont <| fun callback ->
      f x |> Cont.invoke (fun a -> x.Dispose(); callback a)

