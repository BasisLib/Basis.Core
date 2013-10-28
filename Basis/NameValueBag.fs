namespace Basis.Collections

open System
open System.Collections.Specialized

type NameValueBag (nvs: NameValueCollection) =
  member private this.Nvs = nvs
  member internal this.CloneRawData() = NameValueCollection(nvs)
  member this.AllKeys = nvs.AllKeys |> Array.toList
  member this.GetValues(key: string) = nvs.GetValues(key) |> Array.toList
  member this.ContainsName(name: string) = nvs.AllKeys |> Array.exists ((=)name)
  member this.Exists(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map (fun name -> (name, this.GetValues(name)))
    |> Seq.exists pred
  member this.Filter(pred: (string * string list) -> bool) =
    let newRawData = this.CloneRawData()
    let removeKeys = newRawData.AllKeys |> Seq.choose (fun name -> if pred (name, this.GetValues(name)) then Some name else None)
    removeKeys |> Seq.iter (newRawData.Remove)
    NameValueBag(newRawData)
  member this.Find(name: string) =
    let searched = nvs.AllKeys |> Array.find ((=)name)
    this.GetValues(searched)
  member this.FindName(pred: (string * string list) -> bool) =
    nvs.AllKeys
    |> Seq.map (fun name -> (name, this.GetValues(name)))
    |> Seq.pick (fun ((name, _) as kv) -> if pred kv then Some name else None)

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
      String.Join("; ", nvs.AllKeys |> Seq.map (fun name -> name + ": [" + String.Join("; ", nvs.GetValues(name)) + "]"))
    ) |> ignore
    buf.Append("}") |> ignore
    buf.ToString()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameValueBag =
  let empty = NameValueBag(NameValueCollection())

  let toSeq (bag: NameValueBag) =
    bag.AllKeys
    |> Seq.map (fun key -> (key, bag.GetValues(key)))

  let ofSeq (xs: (string * string list) seq) =
    let nvs = NameValueCollection()
    xs |> Seq.iter (fun (key, values) -> values |> Seq.iter (fun v -> nvs.Add(key, v)))
    NameValueBag(nvs)

  let add (name, value) (bag: NameValueBag) =
    let rawData = bag.CloneRawData()
    rawData.Add(name, value)
    NameValueBag(rawData)

  let addValues (name, values) (bag: NameValueBag) =
    let rawData = bag.CloneRawData()
    values |> List.iter (fun v -> rawData.Add(name, v))
    NameValueBag(rawData)

  let containsName name (bag: NameValueBag) = bag.ContainsName(name)

  let exists pred (bag: NameValueBag) = bag.Exists(pred)

  let filter pred (bag: NameValueBag) = bag.Filter(pred)

  let find name (bag: NameValueBag) = bag.Find(name)

  let findName pred (bag: NameValueBag) = bag.FindName(pred)