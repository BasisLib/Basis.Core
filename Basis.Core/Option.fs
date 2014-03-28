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

  type OptionBuilder internal () =
    member thix.Return(x) = Some x
    member this.ReturnFrom(x: _ option) = x
    member this.Bind(x, f) = Option.bind f x

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()