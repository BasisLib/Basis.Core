namespace Basis.Core.Legacy

open System.Collections.Specialized

module NameValueCollection =
  let toMap (x: NameValueCollection) : Map<string, string list> =
    x.AllKeys
    |> Array.map (fun key -> key, x.GetValues(key) |> Array.toList)
    |> Map.ofArray