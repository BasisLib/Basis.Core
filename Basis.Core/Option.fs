namespace Basis.Core

open System

module Option =
  [<CompiledName "GetOr">]
  let getOr defaultValue = function
  | Some v -> v
  | None -> defaultValue

  [<CompiledName "GetOrElse">]
  let getOrElse defaultValueSource = function
  | Some v -> v
  | None -> defaultValueSource ()

  [<CompiledName "GetOrElse">]
  let getOr' defaultLazyValue = function
  | Some v -> v
  | None -> defaultLazyValue |> Lazy.value
