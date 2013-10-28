namespace Basis.Tests

open NUnit.Framework
open FsUnit

open Basis.Core.Collections

[<TestFixture>]
module NameValueBagTest =
  [<Test>]
  let empty() =
    NameValueBag.empty
    |> NameValueBag.toSeq
    |> should be Empty

  let testCaseFor_ofSeq (nvs: (string * string list) seq, expected: (string * string list) seq -> (string * string list) seq) = TestCaseData(nvs, expected nvs |> Seq.toList)
  let src_ofSeq = seq {
    yield testCaseFor_ofSeq(Seq.empty, id)
    yield testCaseFor_ofSeq(seq { yield ("key", []) }, fun _ -> Seq.empty)
    yield testCaseFor_ofSeq(seq { yield ("key", ["value"]) }, id)
    yield testCaseFor_ofSeq(seq { yield ("key", ["value1"; "value2"]) }, id)
    yield testCaseFor_ofSeq(seq { yield ("key1", ["value1"]); yield ("key2", ["value2"]) }, id)
    yield testCaseFor_ofSeq(seq { yield ("key1", []); yield ("key2", ["value2"]); yield ("key1", ["value1"]) }, fun _ -> seq { yield ("key2", ["value2"]); yield ("key1", ["value1"]) })
  }

  [<TestCaseSource "src_ofSeq">]
  let ofSeq(nvs: (string * string list) seq, expected: (string * string list) list) =
    NameValueBag.ofSeq nvs
    |> NameValueBag.toSeq
    |> Seq.toList
    |> should equal expected

  let emptyElemBag = NameValueBag.empty
  let oneElemBag = NameValueBag.ofSeq [("name", ["value"])]
  let twoElemBag = NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"])]

  let src_add = seq {
    yield TestCaseData(emptyElemBag, "name", "value",   NameValueBag.ofSeq [("name", ["value"])])
    yield TestCaseData(emptyElemBag, "name", "other",   NameValueBag.ofSeq [("name", ["other"])])
    yield TestCaseData(oneElemBag,   "name", "value",   NameValueBag.ofSeq [("name", ["value"; "value"])])
    yield TestCaseData(oneElemBag,   "name", "value2",  NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield TestCaseData(oneElemBag,   "name2", "value",  NameValueBag.ofSeq [("name", ["value"]); ("name2", ["value"])])
    yield TestCaseData(twoElemBag,   "name1", "value",  NameValueBag.ofSeq [("name1", ["value"; "value"]); ("name2", ["value1"; "value2"])])
    yield TestCaseData(twoElemBag,   "name1", "value2", NameValueBag.ofSeq [("name1", ["value"; "value2"]); ("name2", ["value1"; "value2"])])
  }

  [<TestCaseSource "src_add">]
  let add(initBag: NameValueBag, addingName: string, addingValue: string, expected: NameValueBag) =
    initBag
    |> NameValueBag.add (addingName, addingValue)
    |> should equal expected

  let testCaseFor_addValues(initBag, name, values: string list, expected) = TestCaseData(initBag, name, values, expected)

  let src_addValues = seq {
    yield testCaseFor_addValues(emptyElemBag, "name",  [],         emptyElemBag)
    yield testCaseFor_addValues(oneElemBag,   "name",  [],         oneElemBag)
    yield testCaseFor_addValues(twoElemBag,   "name",  [],         twoElemBag)
    yield testCaseFor_addValues(emptyElemBag, "name",  ["value"],  NameValueBag.ofSeq [("name", ["value"])])
    yield testCaseFor_addValues(oneElemBag,   "name",  ["value2"], NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield testCaseFor_addValues(twoElemBag,   "name",  ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("name", ["value"])])
    yield testCaseFor_addValues(twoElemBag,   "name1", ["value2"], NameValueBag.ofSeq [("name1", ["value"; "value2"]); ("name2", ["value1"; "value2"])])
    yield testCaseFor_addValues(emptyElemBag, "other", ["value"],  NameValueBag.ofSeq [("other", ["value"])])
    yield testCaseFor_addValues(oneElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name", ["value"]); ("other", ["value"])])
    yield testCaseFor_addValues(twoElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"])])
    yield testCaseFor_addValues(twoElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"])])
    yield testCaseFor_addValues(emptyElemBag, "name",  ["value"; "value2"],  NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield testCaseFor_addValues(oneElemBag,   "name",  ["value2"; "value3"], NameValueBag.ofSeq [("name", ["value"; "value2"; "value3"])])
    yield testCaseFor_addValues(twoElemBag,   "name",  ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("name", ["value"; "value2"])])
    yield testCaseFor_addValues(twoElemBag,   "name1", ["value2"; "value3"], NameValueBag.ofSeq [("name1", ["value"; "value2"; "value3"]); ("name2", ["value1"; "value2"])])
    yield testCaseFor_addValues(emptyElemBag, "other", ["value"; "value2"],  NameValueBag.ofSeq [("other", ["value"; "value2"])])
    yield testCaseFor_addValues(oneElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name", ["value"]); ("other", ["value"; "value2"])])
    yield testCaseFor_addValues(twoElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"; "value2"])])
    yield testCaseFor_addValues(twoElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"; "value2"])])
  }

  [<TestCaseSource "src_addValues">]
  let addValues(initBag: NameValueBag, addingName: string, addingValues: string list, expected: NameValueBag) =
    initBag
    |> NameValueBag.addValues (addingName, addingValues)
    |> should equal expected

  let src_containsName = seq {
    yield TestCaseData(emptyElemBag, "name", false)
    yield TestCaseData(oneElemBag, "name", true)
    yield TestCaseData(oneElemBag, "other", false)
  }

  [<TestCaseSource "src_containsName">]
  let containsName(bag: NameValueBag, name: string, expected: bool) =
    bag
    |> NameValueBag.containsName name
    |> should equal expected

  let testCaseFor_exists(bag, pred: (string * string list) -> bool, expected) = TestCaseData(bag, pred, expected)

  let src_exists = seq {
    yield testCaseFor_exists(emptyElemBag, (fun _ -> true),  false)
    yield testCaseFor_exists(emptyElemBag, (fun _ -> false), false)
    yield testCaseFor_exists(oneElemBag,   (fun _ -> true),  true)
    yield testCaseFor_exists(oneElemBag,   (fun _ -> false), false)
    yield testCaseFor_exists(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), false)
    yield testCaseFor_exists(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), true)  // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_exists(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), false)
  }

  [<TestCaseSource "src_exists">]
  let exists(bag: NameValueBag, pred: (string * string list) -> bool, expected: bool) =
    bag
    |> NameValueBag.exists pred
    |> should equal expected

  let testCaseFor_filter(bag, pred: (string * string list) -> bool, expected) = TestCaseData(bag, pred, expected)

  let src_filter = seq {
    yield testCaseFor_filter(emptyElemBag, (fun _ -> true),  emptyElemBag)
    yield testCaseFor_filter(emptyElemBag, (fun _ -> false), emptyElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun _ -> true),  emptyElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun _ -> false), oneElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), oneElemBag)
    yield testCaseFor_filter(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), NameValueBag.ofSeq [("name1", ["value"])]) // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_filter(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), twoElemBag)
  }

  [<TestCaseSource "src_filter">]
  let filter(bag: NameValueBag, pred: (string * string list) -> bool, expected: NameValueBag) =
    bag
    |> NameValueBag.filter pred
    |> should equal expected

  type KeyNotFoundException = System.Collections.Generic.KeyNotFoundException

  let src_find = seq {
    yield TestCaseData(emptyElemBag, "name",  null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(oneElemBag,   "name",  ["value"])
    yield TestCaseData(oneElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(twoElemBag,   "name1", ["value"])
    yield TestCaseData(twoElemBag,   "name2", ["value1"; "value2"])
    yield TestCaseData(twoElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
  }

  [<TestCaseSource "src_find">]
  let find(bag: NameValueBag, name: string, expected: string list) =
    bag
    |> NameValueBag.find name
    |> should equal expected

  let testCaseFor_findName(bag, pred: (string * string list) -> bool, expected: string) =
    let data = TestCaseData(bag, pred, expected)
    if expected = null then data.Throws(typeof<KeyNotFoundException>) else  data

  let src_findName = seq {
    yield testCaseFor_findName(emptyElemBag, (fun _ -> true),  null)
    yield testCaseFor_findName(emptyElemBag, (fun _ -> false), null)
    yield testCaseFor_findName(oneElemBag,   (fun _ -> true),  "name")
    yield testCaseFor_findName(oneElemBag,   (fun _ -> false), null)
    yield testCaseFor_findName(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), null)
    yield testCaseFor_findName(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), "name2") // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_findName(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), null)
  }

  [<TestCaseSource "src_findName">]
  let findName(bag: NameValueBag, pred: (string * string list) -> bool, expected: string) =
    bag
    |> NameValueBag.findName pred
    |> should equal expected