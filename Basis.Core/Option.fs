﻿namespace Basis.Core

open System

module Option =
  type OptionBuilder internal () =
    member this.Zero() = None
    member this.Return(x) = Some x
    member this.ReturnFrom(x: _ option) = x
    member this.Bind(x, f) = Option.bind f x
    member this.Using(x: #IDisposable, f) =
      try (f x): _ option
      finally if (box x) <> null then x.Dispose()

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()