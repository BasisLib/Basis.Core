namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open System.Xml.Linq
open Basis.Core.Xml.NamespaceLess

[<TestFixture>]
module XmlTest =
  [<Test>]
  let ``can get empty xml declaration``() =
    Xml.Declaration.empty.ToString()
    |> should equal "<?xml?>"

  [<Test>]
  let ``can get utf-8 xml declaration``() =
    Xml.Declaration.utf8.ToString()
    |> should equal "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

  [<Test>]
  let ``can get utf-8 xml declaration (1.1)``() =
    Xml.Declaration.utf8_1_1.ToString()
    |> should equal "<?xml version=\"1.1\" encoding=\"utf-8\"?>"

[<TestFixture>]
module XAttributeTest =
  [<Test>]
  let ``can make XAttribute``() =
    Xml.attr "name" "value"
    |> XAttribute.toStr
    |> should equal (XAttribute(Xml.xname "name", "value").ToString())

  [<Test>]
  let ``can get value``() =
    Xml.attr "name" "value"
    |> XAttribute.value
    |> should equal "value"

[<TestFixture>]
module XElementTest =
  [<Test>]
  let ``can make XElement``() =
    Xml.elem "name" [ "value" ]
    |> XElement.toStr
    |> should equal (XElement(Xml.xname "name", [| "value" |]).ToString())

  [<TestCase([||],          true)>]
  [<TestCase([| "value" |], false)>]
  let ``can check empty``(values: obj[], expected: bool) =
    Xml.elem "name" (values |> Array.toList)
    |> XElement.isEmpty
    |> should equal expected

  let xelem =
    Xml.elem "root" [
      Xml.attr "attr" "value"
      Xml.elem "child" []
      Xml.elem "child" ["hoge"; "piyo"]
      Xml.elem "child" ["foo"; "bar"]
    ]

  [<TestCase("child", 3)>]
  [<TestCase("hoge",  0)>]
  let ``can get elems``(name: string, expectedCount: int) =
    xelem |> XElement.getElems name
    |> Seq.length
    |> should equal expectedCount

  [<TestCase("child", true)>]
  [<TestCase("hoge",  false)>]
  let ``can get elem as option``(name: string, expectedIsSome: bool) =
    xelem |> XElement.tryGetElem name
    |> Option.isSome
    |> should equal expectedIsSome

  [<TestCase("attr", true)>]
  [<TestCase("hoge", false)>]
  let ``can get attribute value as option``(name: string, expectedIsSome: bool) =
    xelem |> XElement.tryGetAttr name
    |> Option.isSome
    |> should equal expectedIsSome
