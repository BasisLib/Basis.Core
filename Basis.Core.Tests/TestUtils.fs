namespace Basis.Core.Tests

type Disposable<'T>(x: 'T) =
  let mutable f: unit -> unit = fun () -> ()
  member this.Value = x
  member this.F with set v = f <- v
  interface System.IDisposable with
    member this.Dispose() =
      f ()