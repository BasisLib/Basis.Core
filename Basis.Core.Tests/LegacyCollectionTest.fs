namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

open System.Collections.Specialized

[<TestFixture>]
module LegacyCollectionTest =
  [<Test>]
  let ``toList test``() =
    let xs = NameValueCollection()
    xs.Add("x", "1")
    xs.Add("x", "2")
    xs.Add("y", "3")

    LegacyCollection.toList xs |> should equal [ "x"; "y" ]

  [<Test>]
  let ``nameValueCollectionToMap test``() =
    let xs = NameValueCollection()
    xs.Add("x", "1")
    xs.Add("x", "2")
    xs.Add("y", "3")

    let actual = LegacyCollection.nameValueCollectionToMap xs
    Map.toList actual |> should equal [ ("x", [ "1"; "2" ]); ("y", [ "3" ]) ]