module PackageTasks

open ProjectInfo

open MessagePrompts
open BasicTasks
open TestTasks

open BlackFox.Fake
open Fake.Core
open Fake.DotNet
open Fake.IO.Globbing.Operators

open System.Text.RegularExpressions

/// https://github.com/Freymaurer/Fake.Extensions.Release#release-notes-in-nuget
let private replaceCommitLink input = 
    let commitLinkPattern = @"\[\[#[a-z0-9]*\]\(.*\)\] "
    Regex.Replace(input,commitLinkPattern,"")

let pack = BuildTask.create "Pack" [clean; buildSolution; runTests] {
    if promptYesNo (sprintf "creating stable package with version %s OK?" stableVersionTag ) 
        then
            !! "src/**/*.*proj"
            -- "src/bin/*"
            |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
                let msBuildParams =
                    {p.MSBuildParams with 
                        Properties = ([
                            "Version",stableVersionTag
                            "PackageReleaseNotes",  (release.Notes |> List.map replaceCommitLink |> String.concat "\r\n" )
                        ] @ p.MSBuildParams.Properties)
                        DisableInternalBinLog = true
                    }
                {
                    p with 
                        MSBuildParams = msBuildParams
                        OutputPath = Some pkgDir
                }
                |> DotNet.Options.withCustomParams (Some "-tl")
            ))
    else failwith "aborted"
}

let packPrerelease = BuildTask.create "PackPrerelease" [setPrereleaseTag; clean; buildSolution; runTests] {
    if promptYesNo (sprintf "package tag will be %s OK?" prereleaseTag )
        then 
            !! "src/**/*.*proj"
            -- "src/bin/*"
            |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
                let msBuildParams =
                    {p.MSBuildParams with 
                        Properties = ([
                            "Version", prereleaseTag
                            "PackageReleaseNotes",  (release.Notes |> List.map replaceCommitLink  |> String.toLines )
                        ] @ p.MSBuildParams.Properties)
                        DisableInternalBinLog = true
                    }
                {
                    p with 
                        VersionSuffix = Some prereleaseSuffix
                        OutputPath = Some pkgDir
                        MSBuildParams = msBuildParams
                }
                |> DotNet.Options.withCustomParams (Some "-tl")
            ))
    else
        failwith "aborted"
}
