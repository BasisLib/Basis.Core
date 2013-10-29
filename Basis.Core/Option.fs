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
    member this.Combine(x: _ option, rest: unit -> _ option) = if x.IsSome then x else rest ()
    member this.TryWith(f, h) = try (f ()): _ option with e -> h e
    member this.TryFinally(f, g) = try (f ()): _ option finally g ()
    member this.Delay(f: unit -> _ option) = f
    member this.Run(f) = f ()

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()