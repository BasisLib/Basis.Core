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