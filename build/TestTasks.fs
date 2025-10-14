module TestTasks

open BlackFox.Fake
open Fake.DotNet

open ProjectInfo
open BasicTasks


let runTests = BuildTask.create "RunTests" [clean; buildSolution] {
    testProjects
    |> Seq.iter (fun testProject ->
        testProject
        |> Fake.DotNet.DotNet.test (fun testParams ->
            { testParams with
                Logger = Some "console;verbosity=detailed"
                Configuration = DotNet.BuildConfiguration.fromString configuration
                NoBuild = true
                MSBuildParams = { testParams.MSBuildParams with DisableInternalBinLog = true }
            }
            |> DotNet.Options.withCustomParams (Some "-tl")
        )
    )
}

let runTestsWithCoverage = BuildTask.create "RunTestsWithCoverage" [clean; buildSolution] {
    testProjects
    |> Seq.iter (fun testProject ->
        testProject
        |> Fake.DotNet.DotNet.test (fun testParams ->
            { testParams with
                Logger = Some "console;verbosity=detailed"
                Configuration = DotNet.BuildConfiguration.fromString configuration
                NoBuild = true
                MSBuildParams = { testParams.MSBuildParams with DisableInternalBinLog = true }
            }
            |> DotNet.Options.withCustomParams (Some "--collect:\"XPlat Code Coverage\" --results-directory ./TestResults --settings ./coverlet.runsettings --logger trx --logger \"console;verbosity=detailed\"")
        )
    )
}

