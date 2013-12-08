namespace Basis.Core

open System

module Str =
  [<CompiledName "Init">]
  let init n f = String.init n f

  [<CompiledName "Replicate">]
  let replicate n str = String.replicate n str

  [<CompiledName "Map">]
  let map f str = String.map f str

  [<CompiledName "Collect">]
  let collect f str = String.collect f str

  [<CompiledName "Iter">]
  let iter act str = String.iter act str

  [<CompiledName "Exists">]
  let exists pred str = String.exists pred str

  [<CompiledName "Forall">]
  let forall pred str = String.forall pred str

  [<CompiledName "GetAsciiBytes">]
  let getAsciiBytes (str: string) = Text.Encoding.ASCII.GetBytes(str)

  [<CompiledName "GetBytes">]
  let getBytes (enc: Text.Encoding) (str: string) = enc.GetBytes(str)

  [<CompiledName "Split2">]
  let split2 (separator: string) (str: string) =
    match str.Split([|separator|], 2, StringSplitOptions.None) with
    | [| a; b |] -> (a, b)
    | _ -> failwithf "str has no separators."

  [<CompiledName "SplitBy">]
  let splitBy (separator: string) (str: string) = str.Split([| separator |], StringSplitOptions.None)

  [<CompiledName "SplitAt">]
  let splitAt (pos: int) (str: string) =
    if str.Length < pos then (str, "")
    elif pos <= 0 then ("", str)
    else (str.Substring(0, pos), str.Substring(pos))

  [<CompiledName "Length">]
  let length str = String.length str

  [<CompiledName "IsNullOrEmpty">]
  let isNullOrEmpty str = String.IsNullOrEmpty(str)

  [<CompiledName "IsNullOrWhiteSpace">]
  let isNullOrWhiteSpace str = String.IsNullOrWhiteSpace(str)

  [<CompiledName "Concat">]
  let concat (xs: seq<_>) = String.Concat(xs)

  [<CompiledName "Join">]
  let join separator (xs: seq<'a>) = String.Join(separator, xs)

  [<CompiledName "Contains">]
  let contains searchStr (str: string) = str.Contains(searchStr)

  [<CompiledName "StartsWith">]
  let startsWith searchStr (str: string) = str.StartsWith(searchStr)

  [<CompiledName "EndsWith">]
  let endsWith searchStr (str: string) = str.EndsWith(searchStr)

  [<CompiledName "Format">]
  let format formatStr values = String.Format(formatStr, values |> Seq.map box |> Seq.toArray)

  [<CompiledName "IndexOf">]
  let indexOf (searchStr: string) (str: string) = str.IndexOf(searchStr)

  [<CompiledName "IndexOf">]
  let indexOf2 (searchStr: string) (start: int) (str: string) = str.IndexOf(searchStr, start)

  [<CompiledName "LastIndexOf">]
  let lastIndexOf (searchStr: string) (str: string) = str.LastIndexOf(searchStr)

  [<CompiledName "LastIndexOf">]
  let lastIndexOf2 (searchStr: string) (start: int) (str: string) = str.LastIndexOf(searchStr, start)

  [<CompiledName "RemoveAt">]
  let removeAt pos (str: string) = str.Remove(pos)

  [<CompiledName "RemoveRange">]
  let removeRange start len (str: string) = str.Remove(start, len)

  [<CompiledName "Remove">]
  let remove searchStr (str: string) = str.Replace(searchStr, "")

  [<CompiledName "Replace">]
  let replace (oldStr: string) (newStr: string) (str: string) = str.Replace(oldStr, newStr)

  [<CompiledName "Substring">]
  let sub start len (str: string) = str.Substring(start, len)

  [<CompiledName "Substring">]
  let subFrom start (str: string) = str.Substring(start)

  [<CompiledName "SubstringTo">]
  let subTo len (str: string) = str.Substring(0, len)

  [<CompiledName "Slice">]
  let slice start endPos (str: string) = str.Substring(start, endPos - start)

  [<CompiledName "ToCharArray">]
  let toCharArray (str: string) = str.ToCharArray()

  [<CompiledName "ToLower">]
  let toLower (str: string) = str.ToLower()

  [<CompiledName "ToUpper">]
  let toUpper (str: string) = str.ToUpper()

  [<CompiledName "Trim">]
  let trim (str: string) = str.Trim()

  [<CompiledName "Trim">]
  let trimChars chars (str: string) = str.Trim(chars |> Seq.toArray)

  [<CompiledName "TrimStart">]
  let trimStart (str: string) = str.TrimStart()

  [<CompiledName "TrimEnd">]
  let trimEnd (str: string) = str.TrimEnd()

  [<CompiledName "TryFind">]
  let tryFindIndex pred (str: string) =
    str |> Seq.tryFindIndex pred

  [<CompiledName "Truncate">]
  let truncate n (str: string) =
    if n < 0 then ""
    elif length str < n then str
    else subTo n str

  [<CompiledName "Skip">]
  let skip n (str: string) = subFrom n str

  [<CompiledName "Take">]
  let take n (str: string) = subTo n str

  [<CompiledName "SkipWhile">]
  let skipWhile pred (str: string) =
    match tryFindIndex (pred >> not) str with
    | Some idx -> subFrom idx str
    | None -> ""

  [<CompiledName "TakeWhile">]
  let takeWhile pred (str: string) =
    match tryFindIndex (pred >> not) str with
    | Some idx -> subTo idx str
    | None -> str