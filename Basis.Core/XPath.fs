namespace Basis.Core.Xml.NamespaceLess

open System.Xml.Linq
open System.Xml.XPath

module XPath =
  let trySelectElement xpath (node: #XNode) =
    match node.XPathSelectElement(xpath) with
    | null -> None
    | notNull -> Some notNull
  let selectElement xpath (node: #XNode) =
    match node.XPathSelectElement(xpath) with
    | null -> failwithf "'%s' is not found" xpath
    | notNull -> notNull
  let selectElements xpath (node: #XNode) = node.XPathSelectElements(xpath)

  type EvaluateResult =
    | Bool of bool
    | Num of float
    | Str of string
    | EmptySeq
    | AttrSeq of XAttribute seq
    | ElemSeq of XElement seq

  let evaluate xpath (node: #XNode) =
    match node.XPathEvaluate(xpath) with
    | null -> failwith "oops! XPathEvaluate returns null!"
    | :? bool as b -> Bool b
    | :? float as f -> Num f
    | :? string as s -> Str s
    | :? System.Collections.IEnumerable as xs when xs |> Seq.cast<obj> |> Seq.isEmpty -> EmptySeq
    | :? seq<XAttribute> as attrs -> AttrSeq attrs
    | :? seq<XElement> as elems -> ElemSeq elems
    | other -> failwithf "oops! XPathEvaluate returns %s!" (other.GetType().FullName)
