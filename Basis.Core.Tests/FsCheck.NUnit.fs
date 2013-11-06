module FsCheck.NUnit

open FsCheck
open NUnit.Framework

let runner =
  { new IRunner with
      member x.OnStartFixture t = ()
      member x.OnArguments(ntets: int, args: obj list, every: int -> obj list -> string) = ()
      member x.OnShrink(args, everyShrink) = ()
      member x.OnFinished(name, result) =
        match result with
        | TestResult.True _ -> ()
        | _ -> Assert.Fail(Runner.onFinishedToString name result) }

let config = { Config.Default with Runner = runner }

let check testable =
  Check.One ("", config, testable)

open System

type NullableGen =
  static member Nullable () =
    Arb.fromGen (gen {
      let! x = Arb.generate<_ option>
      return
        match x with Some x -> Nullable x | None -> Nullable<_>()
    })

do Arb.register<NullableGen>() |> ignore
