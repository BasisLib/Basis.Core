namespace Basis.Core

type Result<'TSuccess, 'TFailure> =
  | Success of 'TSuccess
  | Failure of 'TFailure
with
  member this.ToOption() = match this with Success s -> Some s | _ -> None
  member this.ToOptionFailure() = match this with Failure f -> Some f | _ -> None
  override this.ToString() = sprintf "%A" this

module Result =
  [<CompiledName "ToOption">]
  let toOption (result: Result<_, _>) = result.ToOption()

  [<CompiledName "ToOptionFailure">]
  let toOptionFailure (result: Result<_, _>) = result.ToOptionFailure()