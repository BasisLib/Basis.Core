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
    member this.Zero() = None
    member this.Return(x) = Some x
    member this.ReturnFrom(x: _ option) = x
    member this.Bind(x, f) = Option.bind f x
    member this.Using(x: #IDisposable, f) =
      try (f x): _ option
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine(x: _ option, rest: unit -> _ option) = if x.IsSome then x else rest ()
    member this.TryWith(f, h) = try (f ()): _ option with e -> h e
    member this.TryFinally(f, g) = try (f ()): _ option finally g ()
    member this.While(guard, f) =
      if not (guard ()) then None
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
    member this.Delay(f: unit -> _ option) = f
    member this.Run(f) = f ()

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()