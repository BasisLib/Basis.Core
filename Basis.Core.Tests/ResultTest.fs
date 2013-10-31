namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

open Basis.Core

[<TestFixture>]
module ResultTest =
  let success x : Result<int, string> = Success x
  let failure x : Result<int, string> = Failure x

  let src_toOption = [
    TestCaseData(success 10,     Some 10)
    TestCaseData(success 20,     Some 20)
    TestCaseData(failure "hoge", None)
    TestCaseData(failure "piyo", None)
  ]

  let src_toOptionFailure = [
    TestCaseData(success 10,     None)
    TestCaseData(success 20,     None)
    TestCaseData(failure "hoge", Some "hoge")
    TestCaseData(failure "piyo", Some "piyo")
  ]

  [<TestCaseSource "src_toOption">]
  let toOption (x: Result<int, string>, expected: int option) =
    x
    |> Result.toOption
    |> should equal expected

  [<TestCaseSource "src_toOptionFailure">]
  let toOptionFailure (x: Result<int, string>, expected: string option) =
    x
    |> Result.toOptionFailure
    |> should equal expected

  let check test = check "" test

  let checkEq1 resultF optionF (optionOfSuccess, optionOfFailure) =
    let eqSuccess x =
      try
        let res = Success x |> resultF
        (try
          res = (optionOfSuccess x |> optionF)
         with _ -> false)
      with e1 ->
        (try optionOfSuccess x |> optionF |> ignore; false
         with e2 -> e1.GetType() = e2.GetType())
    let eqFailure x =
      try
        let res = Failure x |> resultF
        (try
          res = (optionOfFailure x |> optionF)
         with _ -> false)
      with e1 ->
        (try optionOfFailure x |> optionF |> ignore; false
         with e2 -> e1.GetType() = e2.GetType())
        
    check (fun x -> eqSuccess x && eqFailure x)

  let checkEq2 resultF optionF (optionOfSuccess, optionOfFailure) =
    check (fun (x, y) -> (Success x |> resultF y) = (optionOfSuccess x |> optionF y) &&
                         (Failure x |> resultF y) = (optionOfFailure x |> optionF y))

  let some_none = (Some, (fun _ -> None))
  let none_some = ((fun _ -> None), Some)

  [<Test>]
  let ``Result.fold should be equal to Option.fold``() =
    checkEq2 (Result.fold (fun acc x -> x::acc))
             (Option.fold (fun acc x -> x::acc))
             some_none

  [<Test>]
  let ``Result.foldFailure should be equal to Option.fold``() =
    checkEq2 (Result.foldFailure (fun acc x -> x::acc))
             (Option.fold (fun acc x -> x::acc))
             none_some

  [<Test>]
  let ``Result.bind should be equal to Option.bind``() =
    checkEq1 (Result.bind (fun v -> Success (v + 1)) >> Result.toOption)
             (Option.bind (fun v -> Some (v + 1)))
             some_none

  [<Test>]
  let ``Result.bindFailure should be equal to Option.bind``() =
    checkEq1 (Result.bindFailure (fun v -> Failure (v + 1)) >> Result.toOptionFailure)
             (Option.bind (fun v -> Some (v + 1)))
             none_some

  [<Test>]
  let ``Result.exists should equal to Option.exists``() =
    checkEq1 (Result.exists (fun v -> v > 10))
             (Option.exists (fun v -> v > 10))
             some_none

  [<Test>]
  let ``Result.existsFailure should equal to Option.exists``() =
    checkEq1 (Result.existsFailure (fun v -> v > 10))
             (Option.exists (fun v -> v > 10))
             none_some

  [<Test>]
  let ``Result.forall should equal to Option.forall``() =
    checkEq1 (Result.forall (fun v -> v > 10))
             (Option.forall (fun v -> v > 10))
             some_none

  [<Test>]
  let ``Result.forallFailure should equal to Option.forall``() =
    checkEq1 (Result.forallFailure (fun v -> v > 10))
             (Option.forall (fun v -> v > 10))
             none_some

  [<Test>]
  let ``Result.get should equal to Option.get``() =
    checkEq1 (Result.get) (Option.get) some_none

  [<Test>]
  let ``Result.getFailure should equal to Option.get``() =
    checkEq1 (Result.getFailure) (Option.get) none_some