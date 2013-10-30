namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

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

  [<Test>]
  let ``fold success``() =
    check ""
      (fun (x, init) ->
        (Success x |> Result.fold (fun acc x -> x::acc) init) = (Some x |> Option.fold (fun acc x -> x::acc) init))

  [<Test>]
  let ``fold failure``() =
    check ""
      (fun (x, init) ->
        (Failure x |> Result.fold (fun acc x -> x::acc) init) = (None |> Option.fold (fun acc x -> x::acc) init))

  [<Test>]
  let ``foldFailure success``() =
    check ""
      (fun (x, init) ->
        (Success x |> Result.foldFailure (fun acc x -> x::acc) init) = (None |> Option.fold (fun acc x -> x::acc) init))

  [<Test>]
  let ``foldFailure failure``() =
    check ""
      (fun (x, init) ->
        (Failure x |> Result.foldFailure (fun acc x -> x::acc) init) = (Some x |> Option.fold (fun acc x -> x::acc) init))