namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core
open Basis.Core.Legacy

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