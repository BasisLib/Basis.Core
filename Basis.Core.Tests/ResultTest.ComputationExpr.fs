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