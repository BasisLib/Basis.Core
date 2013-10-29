namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

[<TestFixture>]
module OptionTest =
  [<Test>]
  let zero() =
    let res = option { () }
    res |> should equal None

  [<Test>]
  let ret() =
    let res = option { return 0 }
    res |> should equal (Some 0)

  let src_retFrom = seq {
    yield TestCaseData(None)
    yield TestCaseData(Some 10)
  }

  [<TestCaseSource "src_retFrom">]
  let retFrom(opt: int option) =
    let res = option { return! opt }
    res |> should equal opt

  let src_letBinding = seq {
    yield TestCaseData(Some 10, Some "20")
    yield TestCaseData(None,    None)
  }

  [<TestCaseSource "src_letBinding">]
  let letBinding(opt: int option, expected: string option) =
    let res = option {
      let! a = opt
      return a * 2 |> string
    }
    res |> should equal expected

  let src_letBindings = seq {
    yield TestCaseData(Some 10, Some 5, Some "15")
    yield TestCaseData(Some 10, None,   None)
    yield TestCaseData(None,    Some 5, None)
    yield TestCaseData(None,    None,   None)
  }

  [<TestCaseSource "src_letBindings">]
  let letBindings(opt1: int option, opt2: int option, expected: string option) =
    let res = option {
      let! a = opt1
      let! b = opt2
      return a + b |> string
    }
    res |> should equal expected

  type Disposable(opt: int option) =
    let mutable f: unit -> unit = fun () -> ()
    member this.Opt = opt
    member this.F with set v = f <- v
    interface System.IDisposable with
      member this.Dispose() =
        f ()

  let src_usingBinding = seq {
    yield TestCaseData(None,                           false, None)
    yield TestCaseData(Some (new Disposable(None)),    true,  None)
    yield TestCaseData(Some (new Disposable(Some 10)), true,  Some "10")
    yield TestCaseData(Some (new Disposable(Some 20)), true,  Some "20")
  }

  [<TestCaseSource "src_usingBinding">]
  let usingBinding(opt: Disposable option, willDisposed: bool, expected: string option) =
    let disposed = ref false
    let res = option {
      use! a = opt
      a.F <- (fun () -> disposed := true)
      let! b = a.Opt
      return b |> string
    }
    res |> should equal expected
    !disposed |> should equal willDisposed

  let src_combine = seq {
    yield TestCaseData(None,    false, None)
    yield TestCaseData(Some 11, false, Some 11)
    yield TestCaseData(Some 18, true,  Some 18)
  }

  [<TestCaseSource "src_combine">]
  let combine(opt: int option, willEven: bool, expected: int option) =
    let isEven = ref false
    let res = option {
      let! a = opt
      if a % 2 = 0 then
        isEven := true
      return a
    }
    res |> should equal expected
    !isEven |> should equal willEven