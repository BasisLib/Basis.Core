namespace Basis.Core

open System

module Option =
  type OptionBuilder internal () =
    member this.Return(x) = Some x
    member this.ReturnFrom(x: _ option) = x
    member this.Bind(x, f) = Option.bind f x

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()