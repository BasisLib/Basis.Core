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

  [<CustomEquality; NoComparison>]
  type EvaluateResult =
    | Bool of bool
    | Num of float
    | Str of string
    | EmptySeq
    | AttrSeq of XAttribute seq
    | ElemSeq of XElement seq
    | TextSeq of XText seq
  with
    override this.Equals(obj) =
      match obj with
      | :? EvaluateResult as other ->
        match this, other with
        | ElemSeq left, ElemSeq right ->
          if Seq.length left <> Seq.length right then
            false
          else
            Seq.zip left right |> Seq.forall (fun (l, r) -> XElement.DeepEquals(l, r))
        | AttrSeq left, AttrSeq right ->
          if Seq.length left <> Seq.length right then
            false
          else
            Seq.zip left right |> Seq.forall (fun (l, r) -> l.Name = r.Name && l.Value = r.Value)
        | TextSeq left, TextSeq right ->
          if Seq.length left <> Seq.length right then
            false
          else
            Seq.zip left right |> Seq.forall (fun (l, r) -> XElement.DeepEquals(l, r))
        | Bool left, Bool right -> left = right
        | Num left, Num right -> left = right
        | Str left, Str right -> left = right
        | EmptySeq, EmptySeq -> true
        | _ -> false
      | _ -> false
    override this.GetHashCode() =
      match this with
      | AttrSeq _ -> 1
      | ElemSeq _ -> 2
      | TextSeq _ -> 3
      | _ -> hash this

  let evaluate xpath (node: #XNode) =
    match node.XPathEvaluate(xpath) with
    | null -> failwith "oops! XPathEvaluate returns null!"
    | :? bool as b -> Bool b
    | :? float as f -> Num f
    | :? string as s -> Str s
    | :? seq<obj> as xs when xs |> Seq.isEmpty -> EmptySeq
    | :? seq<obj> as xs ->
      match Seq.head xs with
      | :? XElement -> ElemSeq (Seq.cast<XElement> xs)
      | :? XAttribute -> AttrSeq (Seq.cast<XAttribute> xs)
      | :? XText -> TextSeq (Seq.cast<XText> xs)
      | _ -> failwithf "oops! XPathEvaluate returns %s!" (xs.GetType().FullName)
    | other -> failwithf "oops! XPathEvaluate returns %s!" (other.GetType().FullName)
