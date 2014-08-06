namespace Basis.Core.Tests

open Basis.Core

open NUnit.Framework
open FsUnit


module RegularExpressionsTest =
  [<TestFixture>]
  module ActivePatternsTest =
    open Basis.Core.RegularExpressions
    open Basis.Core.RegularExpressions.ActivePatterns

    [<TestCase("123", true)>]
    [<TestCase("abc", false)>]
    let ``|Matched|_| pattern`` input expected =
      let actual =
        match input with
        | Matched @"\d+" m -> true
        | _ -> false
      actual |> should equal expected

    [<TestCase("123", false)>]
    [<TestCase("abc", true)>]
    let ``|NotMatched|_| pattern `` input expected =
      let actual =
        match input with
        | NotMatched @"\d+" m -> true
        | _ -> false
      actual |> should equal expected

    [<TestCase("123", true)>]
    [<TestCase("abc", false)>]
    let ``|RegexSucess|RegexFailure| pattern`` input expected =
      let actual =
        let r = Regex.create ""
        match Str.Regex.match' @"\d+" input with
        | RegexSuccess _ -> true
        | RegexFailure -> false
      actual |> should equal expected

    [<Test>]
    let ``|Nth| pattern`` () =
      match "a5" with
      | Matched @"(\w)(\d)" (GroupCollection(Nth 1 (Value v))) ->
        v |> should equal "a"
      | _ -> Assert.Fail()

    [<Test>]
    let ``|Name| pattern`` () =
      match "a5" with
      | Matched @"(?<name1>\w)(?<name2>\d)" (GroupCollection(Name "name2" (Value v))) ->
        v |> should equal "5"
      | _ -> Assert.Fail()

    [<Test>]
    let ``|Values| pattern`` () =
      match "abc" with
      | Matched @"(\w)(\w)(\w)" (Values [ "a"; b ;"c" ]) ->
        b |> should equal "b"
      | _ -> Assert.Fail()