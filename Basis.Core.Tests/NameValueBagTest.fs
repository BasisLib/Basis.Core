namespace Basis.Core.Tests

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

  /// NVB []
  let emptyElemBag = NameValueBag.empty
  /// NVB [("name", ["value"])]
  let oneElemBag = NameValueBag.ofSeq [("name", ["value"])]
  /// NVB [("name1", ["value"]); ("name2", ["value1"; "value2"])]
  let twoElemBag = NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"])]

  let src_addValue = seq {
    yield TestCaseData(emptyElemBag, "name",  null,     null).Throws(typeof<System.ArgumentNullException>)
    yield TestCaseData(emptyElemBag, null,    "value",  null).Throws(typeof<System.ArgumentNullException>)
    yield TestCaseData(emptyElemBag, "name",  "value",  NameValueBag.ofSeq [("name", ["value"])])
    yield TestCaseData(emptyElemBag, "name",  "other",  NameValueBag.ofSeq [("name", ["other"])])
    yield TestCaseData(oneElemBag,   "name",  "value",  NameValueBag.ofSeq [("name", ["value"; "value"])])
    yield TestCaseData(oneElemBag,   "name",  "value2", NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield TestCaseData(oneElemBag,   "name2", "value",  NameValueBag.ofSeq [("name", ["value"]); ("name2", ["value"])])
    yield TestCaseData(twoElemBag,   "name1", "value",  NameValueBag.ofSeq [("name1", ["value"; "value"]); ("name2", ["value1"; "value2"])])
    yield TestCaseData(twoElemBag,   "name1", "value2", NameValueBag.ofSeq [("name1", ["value"; "value2"]); ("name2", ["value1"; "value2"])])
  }

  let src_remove = seq {
    yield TestCaseData(emptyElemBag, "name",  emptyElemBag)
    yield TestCaseData(oneElemBag,   "name",  emptyElemBag)
    yield TestCaseData(oneElemBag,   "aaaa",  oneElemBag)
    yield TestCaseData(twoElemBag,   "name1", NameValueBag.ofSeq [("name2", ["value1"; "value2"])])
    yield TestCaseData(twoElemBag,   "name2", NameValueBag.ofSeq [("name1", ["value"])])
    yield TestCaseData(twoElemBag,   "aaaaa", twoElemBag)
  }

  [<TestCaseSource "src_addValue">]
  let addValue(initBag: NameValueBag, addingName: string, addingValue: string, expected: NameValueBag) =
    initBag
    |> NameValueBag.addValue (addingName, addingValue)
    |> should equal expected

  [<TestCaseSource "src_remove">]
  let remove(bag: NameValueBag, name: string, expected: NameValueBag) =
    bag
    |> NameValueBag.remove name
    |> should equal expected

  let testCaseFor_add(initBag, name, values: string list, expected) = TestCaseData(initBag, name, values, expected)

  let src_add = seq {
    yield testCaseFor_add(emptyElemBag, "name",  [],         emptyElemBag)
    yield testCaseFor_add(oneElemBag,   "name",  [],         oneElemBag)
    yield testCaseFor_add(twoElemBag,   "name",  [],         twoElemBag)
    yield testCaseFor_add(emptyElemBag, "name",  ["value"],  NameValueBag.ofSeq [("name", ["value"])])
    yield testCaseFor_add(oneElemBag,   "name",  ["value2"], NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield testCaseFor_add(twoElemBag,   "name",  ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("name", ["value"])])
    yield testCaseFor_add(twoElemBag,   "name1", ["value2"], NameValueBag.ofSeq [("name1", ["value"; "value2"]); ("name2", ["value1"; "value2"])])
    yield testCaseFor_add(emptyElemBag, "other", ["value"],  NameValueBag.ofSeq [("other", ["value"])])
    yield testCaseFor_add(oneElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name", ["value"]); ("other", ["value"])])
    yield testCaseFor_add(twoElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"])])
    yield testCaseFor_add(twoElemBag,   "other", ["value"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"])])
    yield testCaseFor_add(emptyElemBag, "name",  ["value"; "value2"],  NameValueBag.ofSeq [("name", ["value"; "value2"])])
    yield testCaseFor_add(oneElemBag,   "name",  ["value2"; "value3"], NameValueBag.ofSeq [("name", ["value"; "value2"; "value3"])])
    yield testCaseFor_add(twoElemBag,   "name",  ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("name", ["value"; "value2"])])
    yield testCaseFor_add(twoElemBag,   "name1", ["value2"; "value3"], NameValueBag.ofSeq [("name1", ["value"; "value2"; "value3"]); ("name2", ["value1"; "value2"])])
    yield testCaseFor_add(emptyElemBag, "other", ["value"; "value2"],  NameValueBag.ofSeq [("other", ["value"; "value2"])])
    yield testCaseFor_add(oneElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name", ["value"]); ("other", ["value"; "value2"])])
    yield testCaseFor_add(twoElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"; "value2"])])
    yield testCaseFor_add(twoElemBag,   "other", ["value"; "value2"],  NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value1"; "value2"]); ("other", ["value"; "value2"])])
  }

  [<TestCaseSource "src_add">]
  let addValues(initBag: NameValueBag, addingName: string, addingValues: string list, expected: NameValueBag) =
    initBag
    |> NameValueBag.add (addingName, addingValues)
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
  let testCaseFor_forall = testCaseFor_exists

  let src_exists = seq {
    yield testCaseFor_exists(emptyElemBag, (fun _ -> true),  false)
    yield testCaseFor_exists(emptyElemBag, (fun _ -> false), false)
    yield testCaseFor_exists(oneElemBag,   (fun _ -> true),  true)
    yield testCaseFor_exists(oneElemBag,   (fun _ -> false), false)
    yield testCaseFor_exists(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), false)
    yield testCaseFor_exists(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), true)  // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_exists(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), false)
  }

  let src_forall = seq {
    yield testCaseFor_forall(emptyElemBag, (fun _ -> true),  true)
    yield testCaseFor_forall(emptyElemBag, (fun _ -> false), true)
    yield testCaseFor_forall(oneElemBag,   (fun _ -> true),  true)
    yield testCaseFor_forall(oneElemBag,   (fun _ -> false), false)
    yield testCaseFor_forall(oneElemBag,   (fun (name, values) -> name.Length + values.Length < 8), true)
    yield testCaseFor_forall(twoElemBag,   (fun (name, values) -> name.Length + values.Length < 8), true)  // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_forall(twoElemBag,   (fun (name, values) -> name.Length + values.Length < 7), false)
  }

  [<TestCaseSource "src_exists">]
  let exists(bag: NameValueBag, pred: (string * string list) -> bool, expected: bool) =
    bag
    |> NameValueBag.exists pred
    |> should equal expected

  [<TestCaseSource "src_forall">]
  let forall(bag: NameValueBag, pred: (string * string list) -> bool, expected: bool) =
    bag
    |> NameValueBag.forall pred
    |> should equal expected

  let testCaseFor_filter(bag, pred: (string * string list) -> bool, expected) = TestCaseData(bag, pred, expected)
  let testCaseFor_partition = testCaseFor_filter

  let src_filter = seq {
    yield testCaseFor_filter(emptyElemBag, (fun _ -> true),  emptyElemBag)
    yield testCaseFor_filter(emptyElemBag, (fun _ -> false), emptyElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun _ -> true),  oneElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun _ -> false), emptyElemBag)
    yield testCaseFor_filter(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), emptyElemBag)
    yield testCaseFor_filter(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), NameValueBag.ofSeq [("name2", ["value1"; "value2"])]) // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_filter(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), emptyElemBag)
  }

  let src_partition = seq {
    yield testCaseFor_partition(emptyElemBag, (fun _ -> true),  (emptyElemBag, emptyElemBag))
    yield testCaseFor_partition(emptyElemBag, (fun _ -> false), (emptyElemBag, emptyElemBag))
    yield testCaseFor_partition(oneElemBag,   (fun _ -> true),  (oneElemBag, emptyElemBag))
    yield testCaseFor_partition(oneElemBag,   (fun _ -> false), (emptyElemBag, oneElemBag))
    yield testCaseFor_partition(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6),
                                                                (emptyElemBag, oneElemBag))
    yield testCaseFor_partition(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6),
                                                                (NameValueBag.ofSeq [("name2", ["value1"; "value2"])], NameValueBag.ofSeq [("name1", ["value"])])) // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_partition(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7),
                                                                (emptyElemBag, twoElemBag))
  }

  [<TestCaseSource "src_filter">]
  let filter(bag: NameValueBag, pred: (string * string list) -> bool, expected: NameValueBag) =
    bag
    |> NameValueBag.filter pred
    |> should equal expected

  [<TestCaseSource "src_partition">]
  let partition(bag: NameValueBag, pred: (string * string list) -> bool, expected: NameValueBag * NameValueBag) =
    bag
    |> NameValueBag.partition pred
    |> should equal expected

  let src_get = seq {
    yield TestCaseData(emptyElemBag, "name",  null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(oneElemBag,   "name",  ["value"])
    yield TestCaseData(oneElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(twoElemBag,   "name1", ["value"])
    yield TestCaseData(twoElemBag,   "name2", ["value1"; "value2"])
    yield TestCaseData(twoElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
  }

  let src_getAsSingleString= seq {
    yield TestCaseData(emptyElemBag, "name",  null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(oneElemBag,   "name",  "value")
    yield TestCaseData(oneElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
    yield TestCaseData(twoElemBag,   "name1", "value")
    yield TestCaseData(twoElemBag,   "name2", "value1,value2")
    yield TestCaseData(twoElemBag,   "other", null).Throws(typeof<KeyNotFoundException>)
  }

  [<TestCaseSource "src_get">]
  let get(bag: NameValueBag, name: string, expected: string list) =
    bag
    |> NameValueBag.get name
    |> should equal expected

  [<TestCaseSource "src_getAsSingleString">]
  let getAsSingleString(bag: NameValueBag, name: string, expected: string) =
    bag
    |> NameValueBag.getAsSingleString name
    |> should equal expected

  let testCaseFor_find(bag, pred: (string * string list) -> bool, expected: string list) =
    let data = TestCaseData(bag, pred, expected)
    if (box expected) = null then data.Throws(typeof<KeyNotFoundException>) else data
  let testCaseFor_findName(bag, pred: (string * string list) -> bool, expected: string) =
    let data = TestCaseData(bag, pred, expected)
    if expected = null then data.Throws(typeof<KeyNotFoundException>) else  data
  let testCaseFor_pick(bag, chooser: (string * string list) -> string option, expected: string) =
    let data = TestCaseData(bag, chooser, expected)
    if expected = null then data.Throws(typeof<KeyNotFoundException>) else data

  let src_find = seq {
    yield testCaseFor_find(emptyElemBag, (fun _ -> true),  Unchecked.defaultof<string list>)
    yield testCaseFor_find(emptyElemBag, (fun _ -> false), Unchecked.defaultof<string list>)
    yield testCaseFor_find(oneElemBag,   (fun _ -> true),  ["value"])
    yield testCaseFor_find(oneElemBag,   (fun _ -> false), Unchecked.defaultof<string list>)
    yield testCaseFor_find(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), Unchecked.defaultof<string list>)
    yield testCaseFor_find(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), ["value1"; "value2"]) // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_find(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), Unchecked.defaultof<string list>)
  }

  let src_findName = seq {
    yield testCaseFor_findName(emptyElemBag, (fun _ -> true),  null)
    yield testCaseFor_findName(emptyElemBag, (fun _ -> false), null)
    yield testCaseFor_findName(oneElemBag,   (fun _ -> true),  "name")
    yield testCaseFor_findName(oneElemBag,   (fun _ -> false), null)
    yield testCaseFor_findName(oneElemBag,   (fun (name, values) -> name.Length + values.Length > 6), null)
    yield testCaseFor_findName(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 6), "name2") // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_findName(twoElemBag,   (fun (name, values) -> name.Length + values.Length > 7), null)
  }

  let src_pick = seq {
    yield testCaseFor_pick(emptyElemBag, (fun _ -> Some "a"), null)
    yield testCaseFor_pick(emptyElemBag, (fun _ -> None),     null)
    yield testCaseFor_pick(oneElemBag,   (fun _ -> Some "a"), "a")
    yield testCaseFor_pick(oneElemBag,   (fun _ -> None),     null)
    yield testCaseFor_pick(oneElemBag,   (fun (name, values) -> if name.Length + values.Length > 6 then Some "a" else None),
                                                              null)
    yield testCaseFor_pick(twoElemBag,   (fun (name, values) -> if name.Length + values.Length > 6 then Some "a" else None),
                                                              "a") // "name2".Length + ["value1"; "value2"].Length = 7
    yield testCaseFor_pick(twoElemBag,   (fun (name, values) -> if name.Length + values.Length > 7 then Some "a" else None),
                                                              null)
  }

  [<TestCaseSource "src_find">]
  let find(bag: NameValueBag, pred: (string * string list) -> bool, expected: string list) =
    bag
    |> NameValueBag.find pred
    |> should equal expected

  [<TestCaseSource "src_findName">]
  let findName(bag: NameValueBag, pred: (string * string list) -> bool, expected: string) =
    bag
    |> NameValueBag.findName pred
    |> should equal expected

  [<TestCaseSource "src_pick">]
  let pick(bag: NameValueBag, chooser: (string * string list) -> string option, expected: string) =
    bag
    |> NameValueBag.pick chooser
    |> should equal expected

  let testCaseFor_fold(bag, f: 'a -> (string * string list) -> 'a, init: 'a, expected: 'a) =
    TestCaseData(bag, f, init, expected)

  let src_fold = seq {
    yield testCaseFor_fold(emptyElemBag, (fun _ -> failwith "oops!"), 10, 10)
    yield testCaseFor_fold(oneElemBag,   (fun acc (name, values) -> name.Length + values.Length + acc), 0, "name".Length + 1) // name: [value]
    yield testCaseFor_fold(twoElemBag,   (fun acc (name, values) -> name.Length + values.Length + acc), 0, "name1name2".Length + 3) // name1: [value]; name2: [value1; value2]
  }

  [<TestCaseSource "src_fold">]
  let fold(bag: NameValueBag, f: 'a -> (string * string list) -> 'a, init: 'a, expected: 'a) =
    bag
    |> NameValueBag.fold f init
    |> should equal expected

  let testCaseFor_foldBack(bag, f: (string * string list) -> 'a -> 'a, init: 'a, expected: 'a) =
    TestCaseData(bag, f, init, expected)

  let src_foldBack = seq {
    yield testCaseFor_foldBack(emptyElemBag, (fun _ -> failwith "oops!"), 10, 10)
    yield testCaseFor_foldBack(oneElemBag,   (fun (name, values) acc -> name.Length + values.Length + acc), 0, "name".Length + 1) // name: [value]
    yield testCaseFor_foldBack(twoElemBag,   (fun (name, values) acc -> name.Length + values.Length + acc), 0, "name1name2".Length + 3) // name1: [value]; name2: [value1; value2]
  }
    
  [<TestCaseSource "src_foldBack">]
  let foldBack(bag: NameValueBag, f: (string * string list) -> 'a -> 'a, init: 'a, expected: 'a) =
    NameValueBag.foldBack f bag init |> should equal expected

  let testCaseFor_map(bag, f: (string * string list) -> string list, expected) = TestCaseData(bag, f, expected)

  let src_map = seq {
    yield testCaseFor_map(emptyElemBag, (fun _ -> failwith "oops!"), emptyElemBag)
    yield testCaseFor_map(oneElemBag,   (fun (n, vs) -> vs |> List.rev), NameValueBag.ofSeq [("name", ["value"])])
    yield testCaseFor_map(twoElemBag,   (fun (n, vs) -> vs |> List.rev), NameValueBag.ofSeq [("name1", ["value"]); ("name2", ["value2"; "value1"])])
  }

  [<TestCaseSource "src_map">]
  let map(bag: NameValueBag, f: (string * string list) -> string list, expected: NameValueBag) =
    bag
    |> NameValueBag.map f
    |> should equal expected