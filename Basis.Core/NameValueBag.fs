namespace Basis.Core.Collections

open System
open System.Collections.Specialized

type KeyNotFoundException = System.Collections.Generic.KeyNotFoundException

[<Sealed>]
type NameValueBag (nvs: NameValueCollection) =
  let nameWithValue (name: string) = (name, nvs.GetValues(name) |> Array.toList)
  static member private SeqToNameValueCollection(xs) =
    let nvs = NameValueCollection()
    for name, values in xs do
      for value in values do
        if value <> null then
          nvs.Add(name, value)
    nvs
  new(xs: (string * string list) seq) = NameValueBag(NameValueBag.SeqToNameValueCollection(xs))
  new(other: NameValueBag) = NameValueBag(other.CloneRawData(): NameValueCollection)
  member private this.Nvs = nvs
  member internal this.CloneRawData() = NameValueCollection(nvs)
  member internal this.Clone(act) =
    let newRawData = NameValueCollection(nvs)
    act newRawData
    NameValueBag(newRawData)
  member this.ToEnumerable() = nvs.AllKeys |> Array.map nameWithValue :> seq<_>
  member this.ToArray() = nvs.AllKeys |> Array.map nameWithValue
  member this.ToFSharpList() = nvs.AllKeys |> Array.map nameWithValue |> Array.toList
  member this.IsEmpty = nvs.HasKeys
  member this.AllNames = nvs.AllKeys |> Array.toList
  member internal this.GetValues(name: string) = nvs.GetValues(name) |> Array.toList
  member this.AddValue(name, value) =
    if name = null then nullArg "name"
    if value = null then nullArg "value"
    this.Clone(fun nvs -> nvs.Add(name, value))
  member this.Add(name, values) =
    this.Clone(fun nvs -> values |> List.iter (fun v -> if v <> null then nvs.Add(name, v)))
  member this.ContainsName(name: string) = nvs.AllKeys |> Array.exists ((=)name)
  member this.Exists(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map nameWithValue
    |> Seq.exists pred
  member this.Forall(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map nameWithValue
    |> Seq.forall pred
  member this.Filter(pred: (string * string list) -> bool) =
    let newRawData = this.CloneRawData()
    let removeKeys = newRawData.AllKeys |> Seq.filter (nameWithValue >> pred >> not)
    removeKeys |> Seq.iter (newRawData.Remove)
    NameValueBag(newRawData)
  member this.Partition(pred: (string * string list) -> bool) =
    let trues = this.CloneRawData()
    let falses = this.CloneRawData()
    for name in nvs.AllKeys do
      if pred (nameWithValue name)
        then falses.Remove(name)
        else trues.Remove(name)
    (NameValueBag(trues), NameValueBag(falses))
  member this.TryGetAsSingleString(name: string) =
    match this.Nvs.Get(name) with null -> None | other -> Some other
  member this.TryGet(name: string) =
    nvs.AllKeys |> Array.tryFind ((=)name)
    |> Option.map (fun searched -> this.GetValues(searched))
  member this.TryFind(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map nameWithValue
    |> Seq.tryFind pred
    |> Option.map snd
  member this.TryFindName(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map nameWithValue
    |> Seq.tryPick (fun ((name, _) as kv) -> if pred kv then Some name else None)
  member this.TryPick(chooser: (string * string list) -> 'U option) =
    nvs.AllKeys
    |> Array.tryPick (nameWithValue >> chooser)
  member this.Fold(f: 'TState -> string * string list -> 'TState, init: 'TState) =
    nvs.AllKeys
    |> Array.fold (fun acc name -> f acc (nameWithValue name)) init
  member this.FoldBack(f: string * string list -> 'TState -> 'TState, init: 'TState) =
    Array.foldBack (fun name rest -> f (nameWithValue name) rest) nvs.AllKeys init
  member this.Map(f: string * string list -> string list) =
    nvs.AllKeys
    |> Seq.map (nameWithValue >> (fun (n, vs) -> (n, f (n, vs))))
    |> (fun xs -> NameValueBag(xs))
  member this.Iter(act: string * string list -> unit) =
    nvs.AllKeys
    |> Array.iter (nameWithValue >> act)

  override this.Equals(obj) =
    match obj with
    | :? NameValueBag as other ->
        let nvs1, nvs2 = this.Nvs, other.Nvs
        if nvs1.AllKeys = nvs2.AllKeys
          then (nvs1.AllKeys, nvs2.AllKeys) ||> Array.forall2 (fun n1 n2 -> nvs1.GetValues(n1) = nvs2.GetValues(n2))
          else false
    | _ -> false
  override this.GetHashCode() =
    nvs.GetHashCode()
  override this.ToString() =
    let buf = Text.StringBuilder()
    buf.Append("NameValueBag {") |> ignore
    buf.Append(
      String.Join(
        "; ",
        nvs.AllKeys |> Seq.map (fun name -> name + ": [" + String.Join("; ", match nvs.GetValues(name) with null -> [||] | other -> other) + "]"))
    ) |> ignore
    buf.Append("}") |> ignore
    buf.ToString()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameValueBag =
  [<CompiledName "Empty">]
  let empty = NameValueBag(NameValueCollection())

  [<CompiledName "IsEmpty">]
  let isEmpty (bag: NameValueBag) = bag.IsEmpty

  [<CompiledName "ToEnumerable">]
  let toSeq (bag: NameValueBag) = bag.ToEnumerable()

  [<CompiledName "ToArray">]
  let toArray (bag: NameValueBag) = bag.ToArray()

  [<CompiledName "ToFSharpList">]
  let toList (bag: NameValueBag) = bag.ToFSharpList()

  [<CompiledName "OfEnumerable">]
  let ofSeq (xs: (string * string list) seq) = NameValueBag(xs)

  [<CompiledName "AddValue">]
  let addValue (name, value) (bag: NameValueBag) = bag.AddValue(name, value)

  [<CompiledName "Add">]
  let add (name, values) (bag: NameValueBag) = bag.Add(name, values)

  [<CompiledName "Remove">]
  let remove name (bag: NameValueBag) = bag.Clone(fun nvs -> nvs.Remove(name))

  [<CompiledName "ContainsName">]
  let containsName name (bag: NameValueBag) = bag.ContainsName(name)

  [<CompiledName "Exists">]
  let exists pred (bag: NameValueBag) = bag.Exists(pred)

  [<CompiledName "Forall">]
  let forall pred (bag: NameValueBag) = bag.Forall(pred)

  [<CompiledName "Filter">]
  let filter pred (bag: NameValueBag) = bag.Filter(pred)

  [<CompiledName "Partition">]
  let partition pred (bag: NameValueBag) = bag.Partition(pred)

  [<CompiledName "TryGet">]
  let tryGet name (bag: NameValueBag) = bag.TryGet(name)

  [<CompiledName "Get">]
  let get name (bag: NameValueBag) = match bag.TryGet(name) with Some values -> values | None -> raise (KeyNotFoundException())

  [<CompiledName "TryGetAsSingleString">]
  let tryGetAsSingleString name (bag: NameValueBag) = bag.TryGetAsSingleString(name)

  [<CompiledName "GetAsSingleString">]
  let getAsSingleString name (bag: NameValueBag) = match bag.TryGetAsSingleString(name) with Some str -> str | None -> raise (KeyNotFoundException())

  [<CompiledName "TryFind">]
  let tryFind pred (bag: NameValueBag) = bag.TryFind(pred)

  [<CompiledName "Find">]
  let find pred (bag: NameValueBag) = match bag.TryFind(pred) with Some values -> values | None -> raise (KeyNotFoundException())

  [<CompiledName "TryFindName">]
  let tryFindName pred (bag: NameValueBag) = bag.TryFindName(pred)

  [<CompiledName "FindName">]
  let findName pred (bag: NameValueBag) = match bag.TryFindName(pred) with Some name -> name | None -> raise (KeyNotFoundException())

  [<CompiledName "TryPick">]
  let tryPick chooser (bag: NameValueBag) = bag.TryPick(chooser)

  [<CompiledName "Pick">]
  let pick chooser (bag: NameValueBag) = match bag.TryPick(chooser) with Some res -> res | None -> raise (KeyNotFoundException())

  [<CompiledName "Fold">]
  let fold f init (bag: NameValueBag) = bag.Fold(f, init)

  [<CompiledName "FoldBack">]
  let foldBack f (bag: NameValueBag) init = bag.FoldBack(f, init)

  [<CompiledName "Map">]
  let map f (bag: NameValueBag) = bag.Map(f)

  [<CompiledName "Iter">]
  let iter act (bag: NameValueBag) = bag.Iter(act)
