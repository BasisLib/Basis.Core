namespace Basis.Core

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