namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Basis.Core
open System

[<TestFixture>]
module NullableTest =
  [<TestCase(10,   "not null: 10")>]
  [<TestCase(null, "null value")>]
  let ``nullable value active pattern``(x: Nullable<int>, expected: string) =
    match x with
    | NonNullVal i -> "not null: " + (string i)
    | NullVal -> "null value"
    |> should equal expected

  [<TestCase("hoge", "not null: hoge")>]
  [<TestCase(null,   "null reference")>]
  let ``nullable reference active pattern``(x: string, expected: string) =
    match x with
    | NonNullRef s -> "not null: " + s
    | NullRef -> "null reference"
    |> should equal expected

  module ValueType =

    let src_toOption = [
      TestCaseData(null, (None: int option))
      TestCaseData(10,    Some 10)
    ]

    [<TestCaseSource "src_toOption">]
    let toOption (x: Nullable<int>, expected: int option) =
      x
      |> Nullable.ValueType.toOption
      |> should equal expected

    let src_ofOption = [
      TestCaseData((None: int option), null)
      TestCaseData( Some 10,           10)
    ]

    [<TestCaseSource "src_ofOption">]
    let ofOption (x: int option, expected: Nullable<int>) =
      Nullable.ValueType.ofOption x
      |> should equal expected

    [<Test>]
    let ``(Nullable.ValueType.ofOption >> Nullable.ValueType.toOption) do nothing``() =
      check (fun x -> ((Nullable.ValueType.ofOption >> Nullable.ValueType.toOption) x) = x)

    [<TestCase(null, true)>]
    [<TestCase(10,   false)>]
    let isNull (x: Nullable<int>, expected: bool) =
      x
      |> Nullable.ValueType.isNull
      |> should equal expected

    [<Test>]
    let hasValue () =
      check (fun x -> (x |> Nullable.ValueType.hasValue) = (x |> Nullable.ValueType.isNull |> not))

    [<TestCase(true,  true)>]
    [<TestCase(false, true)>]
    [<TestCase(null,  false)>]
    let hasValue2 (x: Nullable<bool>, expected: bool) =
      x
      |> Nullable.ValueType.hasValue
      |> should equal expected

    [<TestCase(true,  true)>]
    [<TestCase(false, false)>]
    [<TestCase(null,  false)>]
    let isTrue (x: Nullable<bool>, expected: bool) =
      x
      |> Nullable.ValueType.isTrue
      |> should equal expected

    [<TestCase(true,  false)>]
    [<TestCase(false, true)>]
    [<TestCase(null,  true)>]
    let isNotTrue (x: Nullable<bool>, expected: bool) =
      x
      |> Nullable.ValueType.isNotTrue
      |> should equal expected

  module ReferenceType =

    let src_toOption = [
      TestCaseData(null,  (None: string option))
      TestCaseData("hoge", Some "hoge")
    ]

    [<TestCaseSource "src_toOption">]
    let toOption (x: string, expected: string option) =
      x
      |> Nullable.ReferenceType.toOption
      |> should equal expected

    let src_ofOption = [
      TestCaseData((None: string option), null)
      TestCaseData( Some "hoge",          "hoge")
    ]

    [<TestCaseSource "src_ofOption">]
    let ofOption (x: string option, expected: string) =
      Nullable.ReferenceType.ofOption x
      |> should equal expected

    [<Test>]
    let ``(Nullable.ReferenceType.ofOption >> Nullable.ReferenceType.toOption) do nothing``() =
      check (fun x -> ((Nullable.ReferenceType.ofOption >> Nullable.ReferenceType.toOption) x) = x)

    [<TestCase(null,   true)>]
    [<TestCase("hoge", false)>]
    let isNull (x: string, expected: bool) =
      x
      |> Nullable.ReferenceType.isNull
      |> should equal expected

    [<Test>]
    let hasValue () =
      check (fun x -> (x |> Nullable.ReferenceType.hasValue) = (x |> Nullable.ReferenceType.isNull |> not))