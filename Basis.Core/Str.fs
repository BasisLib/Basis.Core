namespace Basis.Core

open System

module Str =
  [<CompiledName "Split2">]
  let split2 (separator: string) (str: string) =
    match str.Split([|separator|], 2, StringSplitOptions.None) with
    | [| a; b |] -> (a, b)
    | [| a |] -> (a, "")
    | _ -> ("", "")