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

  open ComputationExpr

  type OptionBuilder internal () =
    member this.Zero() = None, Continue
    member this.Return(x) = Some x, Break
    member this.ReturnFrom(x: _ option) = x, Break
    member this.Bind(x, f: _ -> _ option * FlowControl) = (Option.bind (f >> fst) x, Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> _ option * FlowControl) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: _ option, cont), rest: unit -> _ option * FlowControl) =
      match cont with
      | Break -> x, Break
      | Continue -> if x.IsSome then x, Break else rest ()
    member this.TryWith(f, h) = try f () with e -> h e
    member this.TryFinally(f, g) = try f () finally g ()
    member this.While(guard, f) =
      if not (guard ()) then this.Zero()
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
    member this.Delay(f: unit -> _ option * FlowControl) = f
    member this.Run(f) = f () |> fst

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()