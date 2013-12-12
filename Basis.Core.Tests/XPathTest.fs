module XPathTest

open NUnit.Framework
open FsUnit

open System.Xml.Linq
open Basis.Core.Xml.NamespaceLess

[<TestFixture>]
module XPathEvaluateTest =
  [<Test>]
  let ``evaluate string``() =
    let xpath = """concat("a", "b")"""
    let xml = XDocument.parse """<xml />"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal (XPath.Str "ab")

  [<Test>]
  let ``evaluate bool``() =
    let xpath = """starts-with("aaa", "b")"""
    let xml = XDocument.parse """<xml />"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal (XPath.Bool false)

  [<Test>]
  let ``evaluate number``() =
    let xpath = """string-length("aaa")"""
    let xml = XDocument.parse """<xml />"""
    let actual = XPath.evaluate xpath xml
    actual |> should (equalWithin 0.1) (XPath.Num 3.0)

  [<Test>]
  let ``evaluate element seq``() =
    let xpath = """/xml/a"""
    let xml = XDocument.parse """<xml><a /><a /><a /></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal (XPath.ElemSeq [ Xml.elem "a" []; Xml.elem "a" []; Xml.elem "a" [] ])

  [<Test>]
  let ``evaluate empty element seq``() =
    let xpath = """/xml/b"""
    let xml = XDocument.parse """<xml><a /><a /><a /></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal XPath.EmptySeq

  [<Test>]
  let ``evaluate attribute seq``() =
    let xpath = """/xml/a/@attr"""
    let xml = XDocument.parse """<xml><a attr="a"/><a attr="b"/><a attr="c"/></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal (XPath.AttrSeq [ Xml.attr "attr" "a"; Xml.attr "attr" "b"; Xml.attr "attr" "c" ])

  [<Test>]
  let ``evaluate empty attribute seq``() =
    let xpath = """/xml/b/@attr"""
    let xml = XDocument.parse """<xml><a attr="a"/><a attr="b"/><a attr="c"/></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal XPath.EmptySeq

  let text (t: string) = XText(t)

  [<Test>]
  let ``evaluate text seq``() =
    let xpath = """/xml/a/text()"""
    let xml = XDocument.parse """<xml><a>a</a><a>b</a><a>c</a></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal (XPath.TextSeq [ text "a"; text "b"; text "c" ])

  [<Test>]
  let ``evaluate empty text seq``() =
    let xpath = """/xml/b/text()"""
    let xml = XDocument.parse """<xml><a /><a /><a /></xml>"""
    let actual = XPath.evaluate xpath xml
    actual |> should equal XPath.EmptySeq