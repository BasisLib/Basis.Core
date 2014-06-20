module Basis.Core.ComputationExpr

open System

type FlowControl = Break | Continue

module State =
    
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

  let option = OptionBuilder()

  type ResultBuilder internal () =
    member this.Return(x) = (Success x), Break
    member this.ReturnFrom(x: Result<_, _>) = x, Break
    member this.Bind(x, f: _ -> Result<_,_> * FlowControl) =
      (Result.bind (f >> fst) x, Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> Result<_,_> * FlowControl) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: Result<_, _>, cont), rest: unit -> Result<_,_> * FlowControl) =
      match cont with
      | Break -> x, Break
      | Continue -> if Result.isSuccess x then x, Break else rest ()
    member this.TryWith(f, h) = try (f ()) with e -> h e
    member this.TryFinally(f, g) = try (f ()) finally g ()
    member this.Delay(f: unit -> (Result<_, _> * FlowControl)) = f
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
    member this.Bind(x, f: _ -> Result<_,_> * FlowControl) =
      (Result.bindFailure (f >> fst) x, Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> Result<_,_> * FlowControl) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: Result<_, _>, cont), rest) =
      match cont with
      | Break -> x, Break
      | Continue -> if Result.isFailure x then x, Break else rest ()
    member this.TryWith(f, h) = try (f ()) with e -> h e
    member this.TryFinally(f, g) = try (f ()) finally g ()
    member this.Delay(f: unit -> Result<_, _> * FlowControl) = f
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

  let result = ResultBuilder()
  let resultWithZero failureValue = ResultWithZeroBuilder(failureValue)
  let failure = FailureBuilder()
  let failureWithZero successValue = FailureWithZeroBuilder(successValue)
