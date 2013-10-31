namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Basis.Core

[<TestFixture>]
module ResultComputationExprTest =
  [<Test>]
  let ret () =
    let res = result { return 1 }
    res |> should equal (Success 1)

  [<Test>]
  let retret () =
    let res = result { return 1; return 2 }
    res |> should equal (Success 1)

  let src_retFrom = [
    TestCaseData(Failure "hoge" : Result<int, string>)
    TestCaseData(Success 10 : Result<int, string>)
  ]

  [<TestCaseSource "src_retFrom">]
  let retFrom(x: Result<int, string>) =
    let res = result { return! x }
    res |> should equal x

  let src_letBinding =
    let data (x: Result<int, string>, expected: Result<string, string>) = TestCaseData(x, expected)
    [
      data (Success 10,     Success "20")
      data (Failure "hoge", Failure "hoge")
    ]

  [<TestCaseSource "src_letBinding">]
  let letBinding(x: Result<int, string>, expected: Result<string, string>) =
    let res = result {
      let! a = x
      return a * 2 |> string
    }
    res |> should equal expected

  let src_usingBinding =
    let data (x: Result<Disposable<Result<int, string>>, string>, willDisposed: bool, expected: Result<string, string>) =
      TestCaseData(x, willDisposed, expected)

    [
      data (Failure "hoge",                                                false, Failure "hoge")
      data (Success (new Disposable<Result<int, string>>(Failure "hoge")), true,  Failure "hoge")
      data (Success (new Disposable<Result<int, string>>(Success 10)),     true,  Success "10")
      data (Success (new Disposable<Result<int, string>>(Success 20)),     true,  Success "20")
    ]

  [<TestCaseSource "src_usingBinding">]
  let usingBinding(x: Result<Disposable<Result<int, string>>, string>, willDisposed: bool, expected: Result<string, string>) =
    let disposed = ref false
    let res = result {
      use! a = x
      a.F <- (fun () -> disposed := true)
      let! b = a.Value
      return b |> string
    }
    res |> should equal expected
    !disposed |> should equal willDisposed

  let src_tryWith =
    let data (f: unit -> Result<int, string>, expected: Result<int, string>) = TestCaseData(f, expected)
    [
      data ((fun () -> Failure "hoge"),   Failure "hoge")
      data ((fun () -> Success 10),       Success 10)
      data ((fun () -> failwith "oops!"), Success -1)
    ]

  [<TestCaseSource "src_tryWith">]
  let tryWith(f: unit -> Result<int, string>, expected: Result<int, string>) =
    let res = result {
      try
        let! a = f ()
        return a
      with
        _ -> return -1
    }
    res |> should equal expected

  let src_tryFinally =
    let data (f: unit -> Result<int, string>, expected: Result<int, string>) = TestCaseData(f, expected)
    [
      (data ((fun () -> Failure "hoge"),   Failure "hoge"))
      (data ((fun () -> Success 10),       Success 10))
      (data ((fun () -> failwith "oops!"), Unchecked.defaultof<Result<int, string>>)).Throws(typeof<exn>)
    ]

  [<TestCaseSource "src_tryFinally">]
  let tryFinally(f: unit -> Result<int, string>, expected: Result<int, string>) =
    let final = ref false
    try
      let res = result {
        try
          let! a = f ()
          return a
        finally
          final := true
      }
      res |> should equal expected
      !final |> should equal true
    with
      _ ->
        !final |> should equal true
        reraise ()