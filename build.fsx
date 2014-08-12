#r @"./lib/FAKE/tools/FakeLib.dll"

open Fake

let nunitPath = "./lib/NUnit.Runners/tools/"
let configuration = getBuildParamOrDefault "Configuration" "Debug"

Target "Build" (fun () ->
  build (fun p -> { p with Targets = [ "Build" ]; Properties = [ ("Configuration", configuration) ]}) "Basis.Core.sln"
)

Target "Test" (fun () ->
  !! (sprintf "./Basis.Core.Tests/bin/%s/*.Tests.dll" configuration)
  |> NUnit (fun p -> { p with ToolPath = nunitPath; Framework = "v4.0" })
)

"Build" ==> "Test"

(* Start Build *)
RunParameterTargetOrDefault "target" "Build"