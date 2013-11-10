namespace Basis.Core.Xml.NamespaceLess

open System.Xml.Linq
open Basis.Core

module Xml =
  type Version = Ver1_0 | Ver1_1
  type Declaration = {
    Version: Version option
    Encoding: System.Text.Encoding option
    Standalone: bool option
  }
  with
    member this.ToXDeclaration() =
      let ver = match this.Version with Some Ver1_0 -> "1.0" | Some Ver1_1 -> "1.1" | None -> null
      let enc = match this.Encoding with Some null | None -> null | Some enc -> enc.WebName
      let standalone = match this.Standalone with None -> null | Some b -> string b
      XDeclaration(ver, enc, standalone)
    override this.ToString() =
      this.ToXDeclaration().ToString()

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Declaration =
    let empty = { Version = None; Encoding = None; Standalone = None }
    let utf8 = { empty with Encoding = Some System.Text.Encoding.UTF8 }
    let utf8_1_1 = { utf8 with Version = Some Ver1_1 }

  let xname name = XName.Get(name)
  let attr name value = XAttribute(xname name, value)
  let elem name children = XElement(xname name, children |> List.toArray)
  let decl (decl: Declaration) = decl.ToXDeclaration()
  let xdoc decl (root: XElement) = XDocument(decl, [| box root |])

open Xml

module XAttribute =
  let value (attr: XAttribute) = attr.Value

module XContainer =
  let getElems name (xml: #XContainer) = xml.Elements(xname name)

  let tryGetElem name (xml: #XContainer) =
    match xml.Element(xname name) with
    | null -> None
    | elem -> Some elem
  let getElem name (xml: #XContainer) =
    match xml.Element(xname name) with
    | null -> failwithf "'%s' is not found" name
    | elem -> elem

module XElement =
  let isEmpty (xml: XElement) = xml.IsEmpty
  let getElems name (xml: XElement) = xml.Elements(xname name)
  let tryGetElem name (xml: XElement) = XContainer.tryGetElem name xml
  let getElem name (xml: XElement) = XContainer.getElem name xml

  let tryGetAttr name (xml: XElement) =
    match xml.Attribute(xname name) with
    | null -> None
    | attr -> Some attr

  let tryParse str = try Some (XElement.Parse(str)) with _ -> None
  let parse str = XElement.Parse(str)
  let loadStream (stream: System.IO.Stream) = XElement.Load(stream)
  let loadReader (reader: System.IO.TextReader) = XElement.Load(reader)
  let loadXmlReader (reader: System.Xml.XmlReader) = XElement.Load(reader)

module XDocument =
  let decl (xml: XDocument) = xml.Declaration
  let root (xml: XDocument) = xml.Root

  let tryParse str = try Some (XDocument.Parse(str)) with _ -> None
  let parse str = XDocument.Parse(str)
  let loadStream (stream: System.IO.Stream) = XDocument.Load(stream)
  let loadReader (reader: System.IO.TextReader) = XDocument.Load(reader)
  let loadXmlReader (reader: System.Xml.XmlReader) = XDocument.Load(reader)