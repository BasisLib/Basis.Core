namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

[<TestFixture>]
module ResultTest =
  let success x : Result<int, string> = Success x
  let failure x : Result<int, string> = Failure x

  let src_toOption = [
    TestCaseData(success 10,     Some 10)
    TestCaseData(success 20,     Some 20)
    TestCaseData(failure "hoge", None)
    TestCaseData(failure "piyo", None)
  ]

  let src_toOptionFailure = [
    TestCaseData(success 10,     None)
    TestCaseData(success 20,     None)
    TestCaseData(failure "hoge", Some "hoge")
    TestCaseData(failure "piyo", Some "piyo")
  ]

  [<TestCaseSource "src_toOption">]
  let toOption (x: Result<int, string>, expected: int option) =
    x
    |> Result.toOption
    |> should equal expected

  [<TestCaseSource "src_toOptionFailure">]
  let toOptionFailure (x: Result<int, string>, expected: string option) =
    x
    |> Result.toOptionFailure
    |> should equal expected