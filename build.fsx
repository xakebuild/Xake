#r "paket:
    nuget Xake ~> 1.1 prerelease //"

#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#endif

open Xake
open Xake.Tasks

let frameworks = ["netstandard2.0"; "net46"]
let libtargets =
    [ for t in frameworks do
      for e in ["dll"; "xml"]
        -> sprintf "out/%s/Xake.%s" t e
    ]

let getVersion () = recipe {
    let! verVar = getVar "VER"
    let! verEnv = getEnv "VER"
    let ver = verVar |> Option.defaultValue (verEnv |> Option.defaultValue "0.0.1")

    let! verSuffix =
        getVar "SUFFIX"
        |> Recipe.map (
            function
            | None -> "-beta"
            | Some "" -> "" // this is release!
            | Some s -> "-" + s
            )
    return ver + verSuffix
}

let makePackageName = sprintf "Xake.%s.nupkg"

let dotnet arglist = recipe {
    do! shell {
        cmd "dotnet"
        args arglist
        failonerror
        } |> Recipe.Ignore
}

do xakeScript {
    filelog "build.log" Verbosity.Diag
    // consolelog Verbosity.Normal

    rules [
        "main"  => recipe {
            do! need ["build"]
            do! need ["test"]
            }

        "build" <== libtargets
        "clean" => rm {dir "out"}

        "test" => recipe {
            do! alwaysRerun()

            let! where =
              getVar("FILTER")
              |> Recipe.map (function |Some clause -> ["--filter"; sprintf "Name~\"%s\"" clause] | None -> [])

            // in case of travis only run tests for standard runtime, eventually will add more
            let! limitFwk = getEnv("TRAVIS") |> Recipe.map (function | Some _ -> ["-f:netcoreapp2.0"] | _ -> [])

            do! dotnet <| ["test"; "src/tests"; "-c"; "Release"; "-p:ParallelizeTestCollections=false"] @ where @ limitFwk
        }

        libtargets *..> recipe {

            let! allFiles
                = getFiles <| fileset {
                    basedir "src/core"
                    includes "Xake.fsproj"
                    includes "**/*.fs"
                }

            do! needFiles allFiles
            let! version = getVersion()

            for framework in frameworks do
                do! dotnet
                        [
                            "build"
                            "src/core"
                            "/p:Version=" + version
                            "--configuration"; "Release"
                            "--framework"; framework
                            "--output"; "../../out/" + framework
                            "/p:DocumentationFile=Xake.xml"
                        ]
        }
    ]

    (* Nuget publishing rules *)
    rules [
        "pack" => recipe {
            let! version = getVersion()
            do! need ["out" </> makePackageName version]
        }

        "out/Xake.(ver:*).nupkg" ..> recipe {
            let! ver = getRuleMatch "ver"
            do! dotnet
                  [
                      "pack"; "src/core"
                      "-c"; "Release"
                      "/p:Version=" + ver
                      "--output"; "../../out/"
                      "/p:DocumentationFile=Xake.xml"
                  ]
        }

        // push need pack to be explicitly called in advance
        "push" => recipe {
            let! version = getVersion()

            let! nuget_key = getEnv "NUGET_KEY"
            do! dotnet
                  [
                    "nuget"; "push"
                    "out" </> makePackageName version
                    "--source"; "https://www.nuget.org/api/v2/package"
                    "--api-key"; nuget_key |> Option.defaultValue ""
                  ]
        }
    ]
}
