namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Basis.Core
open System

[<TestFixture>]
module StrTest =
  [<Test>]
  let ``Str.split2 should be equal to String.Split``() =
    check
      (fun (strs: string[]) ->
        let str = String.Join(",", strs)
        let expected = match str.Split([|','|], 2) with [| a; b |] -> (a, b) | [| a |] -> (a, "") | _ -> ("", "")
        (str |> Str.split2 ",") = expected)

  [<TestCase("", 0, "", "")>]
  [<TestCase("hoge", -10, "", "hoge")>]
  [<TestCase("hoge", -1,  "", "hoge")>]
  [<TestCase("hoge", 0,   "", "hoge")>]
  [<TestCase("hoge", 1,   "h", "oge")>]
  [<TestCase("hoge", 2,   "ho", "ge")>]
  [<TestCase("hoge", 3,   "hog", "e")>]
  [<TestCase("hoge", 4,   "hoge", "")>]
  [<TestCase("hoge", 5,   "hoge", "")>]
  [<TestCase("hoge", 100, "hoge", "")>]
  let splitAt (str: string, pos: int, expectedFront: string, expectedBack: string) =
    str
    |> Str.splitAt pos
    |> should equal (expectedFront, expectedBack)

  [<TestCase("", 0, 0, "")>]
  [<TestCase("hoge", 1, 3, "og")>]
  [<TestCase("hoge", 1, 4, "oge")>]
  let slice (str: string, start: int, endPos: int, expected: string) =
    str
    |> Str.slice start endPos
    |> should equal expected

  [<Test>]
  let ``Str.truncate should be equal to Seq.truncate``() =
    check
      (fun (str: string, n: int) ->
        (str |> Str.truncate n) = (str |> Seq.truncate n |> Str.join ""))

  [<Test>]
  let ``Str.skipWhile should be equal to Seq.skipWhile``() =
    check
      (fun (str: string) ->
        (str |> Str.skipWhile Char.IsDigit) = (str |> Seq.skipWhile Char.IsDigit |> Str.join ""))

  [<Test>]
  let ``Str.takeWhile should be equal to Seq.takeWhile``() =
    check
      (fun (str: string) ->
        (str |> Str.takeWhile Char.IsDigit) = (str |> Seq.takeWhile Char.IsDigit |> Str.join ""))