namespace Basis.Core

open System

type Result<'TSuccess, 'TFailure> =
  | Success of 'TSuccess
  | Failure of 'TFailure
with
  member this.ToOption() = match this with Success s -> Some s | _ -> None
  member this.ToOptionFailure() = match this with Failure f -> Some f | _ -> None
  member this.Fold(f, init) = match this with Success s -> f init s | _ -> init
  member this.FoldFailure(f, init) = match this with Failure e -> f init e | _ -> init
  member this.Bind(f) = match this with Success s -> f s | Failure e -> Failure e
  member this.BindFailure(f) = match this with Success s -> Success s | Failure e -> f e
  member this.Exists(pred) = match this with Success s -> pred s | Failure _ -> false
  member this.ExistsFailure(pred) = match this with Failure e -> pred e | Success _ -> false
  member this.Forall(pred) = match this with Success s -> pred s | Failure _ -> true
  member this.ForallFailure(pred) = match this with Failure e -> pred e | Success _ -> true
  member this.Get() = match this with Success s -> s | Failure _ -> invalidOp "has no success value"
  member this.GetFailure() = match this with Failure e -> e | Success _ -> invalidOp "has not failure value"
  member this.Iter(act) = match this with Success s -> act s | Failure _ -> ()
  member this.IterFailure(act) = match this with Failure e -> act e | Success _ -> ()
  member this.Map(f) = match this with Success s -> Success (f s) | Failure e -> Failure e
  member this.MapFailure(f) = match this with Failure e -> Failure (f e) | Success s -> Success s
  override this.ToString() = sprintf "%A" this

module Result =
  [<CompiledName "ToOption">]
  let toOption (result: Result<_, _>) = result.ToOption()

  [<CompiledName "ToOptionFailure">]
  let toOptionFailure (result: Result<_, _>) = result.ToOptionFailure()

  [<CompiledName "Fold">]
  let fold f init (result: Result<_, _>) = result.Fold(f, init)

  [<CompiledName "FoldFailure">]
  let foldFailure f init (result: Result<_, _>) = result.FoldFailure(f, init)

  [<CompiledName "Bind">]
  let bind f (result: Result<_, _>) = result.Bind(f)

  [<CompiledName "BindFailure">]
  let bindFailure f (result: Result<_, _>) = result.BindFailure(f)

  [<CompiledName "Exists">]
  let exists pred (result: Result<_, _>) = result.Exists(pred)

  [<CompiledName "ExistsFailure">]
  let existsFailure pred (result: Result<_, _>) = result.ExistsFailure(pred)

  [<CompiledName "Forall">]
  let forall pred (result: Result<_, _>) = result.Forall(pred)

  [<CompiledName "ForallFailure">]
  let forallFailure pred (result: Result<_, _>) = result.ForallFailure(pred)

  [<CompiledName "Get">]
  let get (result: Result<_, _>) = try result.Get() with _ -> invalidArg "result" "has no success value"

  [<CompiledName "GetFailure">]
  let getFailure (result: Result<_, _>) = try result.GetFailure() with _ -> invalidArg "result" "has no failure value"

  [<CompiledName "GetOr">]
  let getOr defaultValue = function Success v -> v | Failure _ -> defaultValue

  [<CompiledName "GetFailureOr">]
  let getFailureOr defaultValue = function Failure v -> v | Success _ -> defaultValue

  [<CompiledName "GetOrElse">]
  let getOrElse defaultValueSource = function Success v -> v | Failure _ -> defaultValueSource ()

  [<CompiledName "GetFailureOrElse">]
  let getFailueOrElse defaultValueSource = function Failure v -> v | Success _ -> defaultValueSource ()

  [<CompiledName "GetOrElse">]
  let getOr' defaultLazyValue = function Success v -> v | Failure _ -> defaultLazyValue |> Lazy.value

  [<CompiledName "GetFailureOrElse">]
  let getFailureOr' defaultLazyValue = function Failure v -> v | Success _ -> defaultLazyValue |> Lazy.value

  [<CompiledName "IsSuccess">]
  let isSuccess = function Success _ -> true | Failure _ -> false

  [<CompiledName "IsFailure">]
  let isFailure = function Failure _ -> true | Success _ -> false

  [<CompiledName "Iter">]
  let iter act (result: Result<_, _>) = result.Iter(act)

  [<CompiledName "IterFailure">]
  let iterFailure act (result: Result<_, _>) = result.IterFailure(act)

  [<CompiledName "Map">]
  let map f (result: Result<_, _>) = result.Map(f)

  [<CompiledName "MapFailure">]
  let mapFailure f (result: Result<_, _>) = result.MapFailure(f)

  open System.ComponentModel

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  module Impl =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type ExprBreakType = Break | Continue

  open Impl

  type ResultBuilder internal () =
    member this.Return(x) = (Success x), Break
    member this.ReturnFrom(x: Result<_, _>) = x, Break
    member this.Bind(x, f: _ -> Result<_,_> * ExprBreakType) = (bind (f >> fst) x, Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> Result<_,_> * ExprBreakType) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: Result<_, _>, typ), rest: unit -> Result<_,_> * ExprBreakType) =
      match typ with
      | Break -> x, Break
      | Continue -> if isSuccess x then x, Break else rest ()
    member this.TryWith(f, h) = try (f ()) with e -> h e
    member this.TryFinally(f, g) = try (f ()) finally g ()
    member this.Delay(f: unit -> (Result<_, _> * ExprBreakType)) = f
    member this.Run(f) = f () |> fst

  type ResultWithZeroBuilder<'TZero> internal (zeroValue: 'TZero) =
    inherit ResultBuilder()
    member this.Zero () = Failure zeroValue, Continue
    member this.While(guard, f) =
      if not (guard ()) then this.Zero()
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))

  type FailureBuilder internal () =
    member this.Return(x) = Failure x, Break
    member this.ReturnFrom(x: Result<_, _>) = x, Break
    member this.Bind(x, f: _ -> Result<_,_> * ExprBreakType) = (bindFailure (f >> fst) x, Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> Result<_,_> * ExprBreakType) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: Result<_, _>, typ), rest) =
      match typ with
      | Break -> x, Break
      | Continue -> if isFailure x then x, Break else rest ()
    member this.TryWith(f, h) = try (f ()) with e -> h e
    member this.TryFinally(f, g) = try (f ()) finally g ()
    member this.Delay(f: unit -> Result<_, _> * ExprBreakType) = f
    member this.Run(f) = f () |> fst

  type FailureWithZeroBuilder<'TZero> internal (zeroValue: 'TZero) =
    inherit FailureBuilder()
    member this.Zero () = Success zeroValue, Continue
    member this.While(guard, f) =
      if not (guard ()) then this.Zero()
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))

[<AutoOpen>]
module ResultDefaultOps =
  let result = Result.ResultBuilder()
  let resultWithZero failureValue = Result.ResultWithZeroBuilder(failureValue)
  let failure = Result.FailureBuilder()
  let failureWithZero successValue = Result.FailureWithZeroBuilder(successValue)