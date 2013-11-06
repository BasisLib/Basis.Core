namespace Basis.Core

open System

[<AutoOpen>]
module NullableActivePatterns =
  let (|NullVal|NonNullVal|) (x: Nullable<_>) = if x.HasValue then NonNullVal x.Value else NullVal
  let (|NullRef|NonNullRef|) (x: 'T when 'T : null) = if obj.Equals(x, null) then NullRef else NonNullRef x

[<RequireQualifiedAccess>]
module Nullable =
  [<RequireQualifiedAccess>]
  module ValueType =
    let toOption (x: Nullable<_>) =
      if x.HasValue then Some x.Value else None

    let ofOption = function
    | Some x -> Nullable x
    | None -> Nullable<_>()

    let isNull (x: Nullable<_>) = not x.HasValue
    let hasValue (x: Nullable<_>) = x.HasValue

    let isTrue (b: Nullable<bool>) = b.HasValue && b.Value
    let isNotTrue (b: Nullable<bool>) = not (isTrue b)

  [<RequireQualifiedAccess>]
  module ReferenceType =
    let toOption (x: 'T when 'T : null) =
      if obj.Equals(x, null) then None else Some x

    let ofOption = function
    | Some x -> x
    | None -> null

    let isNull (x: 'T when 'T : null) = obj.Equals(x, null)
    let hasValue (x: 'T when 'T : null) = not (obj.Equals(x, null))