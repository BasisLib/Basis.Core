namespace Basis.Core.Legacy

module LegacyCollection =
  open System.Collections

  let inline toSeq (xs: ^TCollection) : 'T seq when ^TCollection : (member get_Item : int -> 'T) and ^TCollection :> ICollection =
    seq {
      let e = xs.GetEnumerator()
      while e.MoveNext() do
        yield e.Current :?> 'T
    }

  let inline toList (xs: ^TCollection) : 'T list when ^TCollection : (member get_Item : int -> 'T) and ^TCollection :> ICollection =
    [
      let e = xs.GetEnumerator()
      while e.MoveNext() do
        yield e.Current :?> 'T
    ]

  let inline toArray (xs: ^TCollection) : 'T [] when ^TCollection : (member get_Item : int -> 'T) and ^TCollection :> ICollection =
    [|
      let e = xs.GetEnumerator()
      while e.MoveNext() do
        yield e.Current :?> 'T
    |]