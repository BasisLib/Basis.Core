namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

[<TestFixture>]
module OptionComputationExprTest =
  [<Test>]
  let zero() =
    let res = option { () }
    res |> should equal None

  [<Test>]
  let ret() =
    let res = option { return 0 }
    res |> should equal (Some 0)

  [<Test>]
  let retret() =
    let res = option { return 10; return 20; }
    res |> should equal (Some 10)

  let src_retFrom = [
    TestCaseData(None)
    TestCaseData(Some 10)
  ]

  [<TestCaseSource "src_retFrom">]
  let retFrom(opt: int option) =
    let res = option { return! opt; return 0 }
    res |> should equal opt

  let src_letBinding = [
    TestCaseData(Some 10, Some "20")
    TestCaseData(None,    None)
  ]

  [<TestCaseSource "src_letBinding">]
  let letBinding(opt: int option, expected: string option) =
    let res = option {
      let! a = opt
      return a * 2 |> string
    }
    res |> should equal expected

  let src_letBindings = [
    TestCaseData(Some 10, Some 5, Some "15")
    TestCaseData(Some 10, None,   None)
    TestCaseData(None,    Some 5, None)
    TestCaseData(None,    None,   None)
  ]

  [<TestCaseSource "src_letBindings">]
  let letBindings(opt1: int option, opt2: int option, expected: string option) =
    let res = option {
      let! a = opt1
      let! b = opt2
      return a + b |> string
    }
    res |> should equal expected

  let src_usingBinding = [
    TestCaseData(None,                                       false, None)
    TestCaseData(Some (new Disposable<int option>(None)),    true,  None)
    TestCaseData(Some (new Disposable<int option>(Some 10)), true,  Some "10")
    TestCaseData(Some (new Disposable<int option>(Some 20)), true,  Some "20")
  ]

  [<TestCaseSource "src_usingBinding">]
  let usingBinding(opt: Disposable<int option> option, willDisposed: bool, expected: string option) =
    let disposed = ref false
    let res = option {
      use! a = opt
      a.F <- (fun () -> disposed := true)
      let! b = a.Value
      return b |> string
    }
    res |> should equal expected
    !disposed |> should equal willDisposed

  let src_combine = [
    TestCaseData(None,    false, None)
    TestCaseData(Some 11, false, Some 11)
    TestCaseData(Some 18, true,  Some 36)
  ]

  [<TestCaseSource "src_combine">]
  let combine(opt: int option, willEven: bool, expected: int option) =
    let isEven = ref false
    let res = option {
      let! a = opt
      if a % 2 = 0 then
        isEven := true
        return a * 2
      return a
    }
    res |> should equal expected
    !isEven |> should equal willEven

  let src_tryWith = [
    TestCaseData((fun () -> None: int option),             (None: int option))
    TestCaseData((fun () -> Some 10),                       Some 10)
    TestCaseData((fun () -> failwith "oops!": int option),  Some -1)
  ]

  [<TestCaseSource "src_tryWith">]
  let tryWith(f: unit -> int option, expected: int option) =
    let res = option {
      try
        let! a = f ()
        return a
      with
        _ -> return -1
    }
    res |> should equal expected

  let src_tryFinally = [
    TestCaseData((fun () -> None: int option),             (None: int option))
    TestCaseData((fun () -> Some 10),                       Some 10)
    TestCaseData((fun () -> failwith "oops!": int option),  null).Throws(typeof<System.Exception>)
  ]

  [<TestCaseSource "src_tryFinally">]
  let tryFinally(f: unit -> int option, expected: int option) =
    let final = ref false
    try
      let res = option {
        try
          let! a = f ()
          return a
        finally
          final := true
      }
      res |> should equal expected
      !final |> should be True
    with
      _ ->
        !final |> should be True
        reraise ()

  let src_whileLoop = [
    TestCaseData((None: int option), 0, (None: int option))
    TestCaseData( Some 1,            5,  Some 1)
    TestCaseData( Some 2,            6,  Some 2)
    TestCaseData( Some 10,           10, Some -1)
  ]

  [<TestCaseSource "src_whileLoop">]
  let whileLoop(opt: int option, expectedCounter: int, expected: int option) =
    let counter = ref 0
    let res = option {
      let! a = opt
      while (!counter < 5) do
        counter := !counter + a
        if !counter = 10 then
          return -1
      return a
    }
    res |> should equal expected
    !counter |> should equal expectedCounter

  let src_forLoop = [
    TestCaseData((None: int option), 0, (None: int option))
    TestCaseData( Some 1,            5,  Some 1)
    TestCaseData( Some -1,           3,  Some 0)
  ]

  [<TestCaseSource "src_forLoop">]
  let forLoop(opt: int option, expectedCounter: int, expected: int option) =
    let counter = ref 0
    let res = option {
      let! a = opt
      for i in 1..5 do
        counter := i
        if a = -1 && i = 3 then
          return 0
      return a
    }
    res |> should equal expected
    !counter |> should equal expectedCounter