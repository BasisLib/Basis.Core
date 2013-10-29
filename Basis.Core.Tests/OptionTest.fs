namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

[<TestFixture>]
module OptionTest =
  [<Test>]
  let ret() =
    let res = option { return 0 }
    res |> should equal (Some 0)

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