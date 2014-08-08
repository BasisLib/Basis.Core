namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core
open Basis.Core.Legacy

open System.Collections.Specialized

[<TestFixture>]
module NameValueCollectionTest =
  [<Test>]
  let ``nameValueCollectionToMap test``() =
    let xs = NameValueCollection()
    xs.Add("x", "1")
    xs.Add("x", "2")
    xs.Add("y", "3")

    let actual = NameValueCollection.toMap xs
    Map.toList actual |> should equal [ ("x", [ "1"; "2" ]); ("y", [ "3" ]) ]