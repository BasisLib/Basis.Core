namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Basis.Core

[<TestFixture>]
module ResultComputationExprTest =
  [<Test>]
  let zero () =
    let res = resultWithZero ("oops!") { () }
    res |> should equal (Failure "oops!")

  [<Test>]
  let zeroFailure () =
    let res = failureWithZero (0) { () }
    res |> should equal (Success 0)

  [<Test>]
  let ret () =
    let res = result { return 1 }
    res |> should equal (Success 1)

    let res = resultWithZero ("oops!") { return 1 }
    res |> should equal (Success 1)

  [<Test>]
  let retFailure () =
    let res = failure { return "hoge" }
    res |> should equal (Failure "hoge")

    let res = failureWithZero (0) { return "hoge" }
    res |> should equal (Failure "hoge")

  [<Test>]
  let retret () =
    let res = result { return 1; return 2 }
    res |> should equal (Success 1)

  [<Test>]
  let retretFailure () =
    let res = failure { return "hoge"; return "piyo" }
    res |> should equal (Failure "hoge")


  let src_retFrom =
    let data (x: Result<int, string>) = TestCaseData(x)
    [
      data (Failure "hoge")
      data (Success 10)
    ]

  [<TestCaseSource "src_retFrom">]
  let retFrom (x: Result<int, string>) =
    let res = result { return! x }
    res |> should equal x

  [<TestCaseSource "src_retFrom">]
  let retFromFailure (x: Result<int, string>) =
    let res = failure { return! x }
    res |> should equal x

  let src_letBinding =
    let data (x: Result<int, string>, expected: Result<string, string>) = TestCaseData(x, expected)
    [
      data (Success 10,     Success "20")
      data (Failure "hoge", Failure "hoge")
    ]

  let src_letBindingFailure =
    let data (x: Result<int, string>, expected: Result<int, string>) = TestCaseData(x, expected)
    [
      data (Success 10,     Success 10)
      data (Failure "hoge", Failure "hogehoge")
    ]

  [<TestCaseSource "src_letBinding">]
  let letBinding (x: Result<int, string>, expected: Result<string, string>) =
    let res = result {
      let! a = x
      return a * 2 |> string
    }
    res |> should equal expected

  [<TestCaseSource "src_letBindingFailure">]
  let letBindingFailure (x: Result<int, string>, expected: Result<int, string>) =
    let res = failure {
      let! a = x
      return a + a
    }
    res |> should equal expected

  let src_usingBinding =
    let data (x: Result<Disposable<Result<int, string>>, string>, willDisposed: bool, expected: Result<string, string>) =
      TestCaseData(x, willDisposed, expected)

    [
      data (Failure "hoge",                                                false, Failure "hoge")
      data (Success (new Disposable<Result<int, string>>(Failure "hoge")), true,  Failure "hoge")
      data (Success (new Disposable<Result<int, string>>(Success 10)),     true,  Success "10")
    ]

  let src_usingBindingFailure =
    let data (x: Result<int, Disposable<Result<int, string>>>, willDisposed: bool, expected: Result<int, string>) =
      TestCaseData(x, willDisposed, expected)

    [
      data (Failure (new Disposable<Result<int, string>>(Failure "hoge")), true,  Failure "hogehoge")
      data (Failure (new Disposable<Result<int, string>>(Success 10)),     true,  Success 10)
      data (Success 10,                                                    false, Success 10)
    ]

  [<TestCaseSource "src_usingBinding">]
  let usingBinding (x: Result<Disposable<Result<int, string>>, string>, willDisposed: bool, expected: Result<string, string>) =
    let disposed = ref false
    let res = result {
      use! a = x
      a.F <- (fun () -> disposed := true)
      let! b = a.Value
      return b |> string
    }
    res |> should equal expected
    !disposed |> should equal willDisposed

  [<TestCaseSource "src_usingBindingFailure">]
  let usingBindingFailure (x: Result<int, Disposable<Result<int, string>>>, willDisposed: bool, expected: Result<int, string>) =
    let disposed = ref false
    let res = failure {
      use! a = x
      a.F <- (fun () -> disposed := true)
      let! b = a.Value
      return b + b
    }
    res |> should equal expected
    !disposed |> should equal willDisposed

  let dataFor_tryWith (f: unit -> Result<int, string>, expected: Result<int, string>) = TestCaseData(f, expected)
  let src_tryWith = [
    dataFor_tryWith ((fun () -> Failure "hoge"),   Failure "hoge")
    dataFor_tryWith ((fun () -> Success 10),       Success 10)
    dataFor_tryWith ((fun () -> failwith "oops!"), Success -1)
  ]
  let src_tryWithFailure = [
    dataFor_tryWith ((fun () -> Failure "hoge"),   Failure "hoge")
    dataFor_tryWith ((fun () -> Success 10),       Success 10)
    dataFor_tryWith ((fun () -> failwith "oops!"), Failure "err")
  ]

  [<TestCaseSource "src_tryWith">]
  let tryWith (f: unit -> Result<int, string>, expected: Result<int, string>) =
    let res = result {
      try
        let! a = f ()
        return a
      with
        _ -> return -1
    }
    res |> should equal expected

  [<TestCaseSource "src_tryWithFailure">]
  let tryWithFailure (f: unit -> Result<int, string>, expected: Result<int, string>) =
    let res = failure {
      try
        let! a = f ()
        return a
      with
        _ -> return "err"
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
  let tryFinally (f: unit -> Result<int, string>, expected: Result<int, string>) =
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

  [<TestCaseSource "src_tryFinally">]
  let tryFinallyFailure (f: unit -> Result<int, string>, expected: Result<int, string>) =
    let final = ref false
    try
      let res = failure {
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

  let src_whileLoop =
    let data (x: Result<int, string>, expectedCounter: int, expected: Result<int, string>) = TestCaseData(x, expectedCounter, expected)
    [
      data (Failure "hoge",  0,  Failure "hoge")
      data (Success 1,       5,  Success 1)
      data (Success 2,       6,  Success 2)
      data (Success 10,      10, Success -1)
    ]

  [<TestCaseSource "src_whileLoop">]
  let whileLoop(x: Result<int, string>, expectedCounter: int, expected: Result<int, string>) =
    let counter = ref 0
    let res = resultWithZero ("oops!") {
      let! a = x
      while (!counter < 5) do
        counter := !counter + a
        if !counter = 10 then
          return -1
      return a
    }
    res |> should equal expected
    !counter |> should equal expectedCounter

  let src_forLoop =
    let data (x: Result<int, string>, expectedCounter: int, expected: Result<int, string>) = TestCaseData(x, expectedCounter, expected)
    [
      data (Failure "hoge", 0, Failure "hoge")
      data (Success 1,      5, Success 1)
      data (Success -1,     3, Success 0)
    ]

  [<TestCaseSource "src_forLoop">]
  let forLoop(x: Result<int, string>, expectedCounter: int, expected: Result<int, string>) =
    let counter = ref 0
    let res = resultWithZero ("oops!") {
      let! a = x
      for i in 1..5 do
        counter := i
        if a = -1 && i = 3 then
          return 0
      return a
    }
    res |> should equal expected
    !counter |> should equal expectedCounter