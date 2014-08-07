namespace Basis.Core.RegularExpressions

open System
open System.Text.RegularExpressions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Capture =
  let index (x: Capture) = x.Index
  let length (x: Capture) = x.Length
  let value (x: Capture) = x.Value

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Group =
  let captures (x: Group) = [ for y in x.Captures -> y ]
  let captureCollection (x: Group) = x.Captures
  
  let index (x: Group) = x.Index
  let length (x: Group) = x.Length
  let success (x: Group) = x.Success
  let value (x: Group) = x.Value

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Match =
  let captures (x: Match) = [ for y in x.Captures -> y ]
  let captureCollection (x: Match) = x.Captures
  let groups (x: Match) = [ for y in x.Groups -> y ]
  let groupCollection (x: Match) = x.Groups
  let getGroupByName (name: string) (x: Match) = x.Groups.[name]
  let getGroupByIndex (n: int) (x: Match) = x.Groups.[n]
  let tryGetGroupByName (name: string) (x: Match) =
    let g = getGroupByName name x
    if Group.success g then
      Some g
    else
      None
  let tryGetGroupByIndex (n: int) (x: Match) =
    let g = getGroupByIndex n x
    if Group.success g then
      Some g
    else
      None

  let index (x: Match) = x.Index
  let length (x: Match) = x.Length
  let success (x: Match) = x.Success
  let value (x: Match) = x.Value

  let result replacement (x: Match) = x.Result(replacement)

  let nextMatch (x: Match) = x.NextMatch()
  let tryNextMatch (x: Match) =
    let m = nextMatch x
    if m <> Match.Empty then Some m else None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Regex =
  let create pattern = Regex(pattern)
  let createWithOptions options pattern = Regex(pattern, options)

  let match' (x: Regex) input = x.Match(input)
  let tryMatch (x: Regex) input =
    let m = match' x input
    if m.Success then Some m else None
  let matches (x: Regex) input = [ for y in x.Matches(input) -> y ]
  let isMatch (x: Regex) input = x.IsMatch(input)
  let split (x: Regex) input = x.Split(input)
  let replace (x: Regex) (replacement: string) (input: string) = x.Replace(input, replacement)
  let replaceWithEvaluator (x: Regex) (f: Match -> string) (input: string) = x.Replace(input, MatchEvaluator(f))

  let escape str = Regex.Escape(str)
  let unescape str = Regex.Unescape(str)

module Patterns =
  let (|Matched|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some m else None
  let (|NotMatched|_|) pattern input =
    if Regex.IsMatch(input, pattern) then None else Some ()
  let (|MatchSuccess|MatchFailure|) (x: #Group) = if Group.success x then MatchSuccess x else MatchFailure
  let (|Captures|) (x: #Group) = Group.captures x
  let (|CaptureCollection|) (x: #Group) = Group.captureCollection x
  let (|CaptureValues|) (x: #Group) = Group.captures x |> List.map Capture.value
  let (|Groups|) (x: Match) = Match.groups x
  let (|GroupCollection|) (x: Match) = Match.groupCollection x
  let (|GroupValues|) (x: Match) = Match.groups x |> List.map Group.value

  let (|Nth|) (n: int) (xs: GroupCollection) = xs.[n]
  let (|Name|) (name: string) (xs: GroupCollection) = xs.[name]

  let (|Value|) (x: #Capture) = Capture.value x
  let (|Values|) (x: Match) = (|GroupValues|) x |> List.tail