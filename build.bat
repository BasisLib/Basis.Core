@echo off

".nuget\NuGet.exe" "install" "FAKE" "-OutputDirectory" "lib" "-ExcludeVersion"
".nuget\NuGet.exe" "install" "NUnit.Runners" "-OutputDirectory" "lib" "-ExcludeVersion"

"lib\FAKE\tools\FAKE.exe" "build.fsx" "encoding=utf-8" %*