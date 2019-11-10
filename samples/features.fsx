#r "paket:
    nuget Xake ~> 1.1 prerelease
    nuget Xake.Dotnet ~> 1.1 prerelease //"

#if !FAKE
#load ".fake/features.fsx/intellisense.fsx"
#endif

// This a sample Xake script to show off some features.
//
// USAGE:
// * `fake run` or
// * `dotnet restore && dotnet fake run`
// 
// Running particular target:
// * `dotnet fake run build.fsx -- clean`

open Xake
open Xake.Tasks
open Xake.Dotnet

do xakeScript {

    consolelog Verbosity.Diag

    // this instruction defines the default target and could be overriden by command-line parameters
    // this is redundant as "main" is default target
    want ["main"]

    rules [

        // this rule does nothing but demands the other targets
        // the execution of the recipe is suspended until all demanded targets are built.
        // Targets are executed in parallel. Dependencies could be demanded in any part of recipe.
        "main"  => recipe {
            do! need ["tracetest"; "temp/a.exe"]
            }

        // this is shorter way to express the same. See also `<==` and '<<<' operators.
        "main"  => need ["tracetest"; "temp/a.exe"]

        // "phony" rule that produces no file but just removes the files
        // `rm` recipe (Xake.Tasks namespace) allow to remove files and folders
        "clean" => recipe {
            do! rm {file "paket-files/*.*"}
            do! rm {dir "out"}
            do! rm {files (fileset {
                    includes "samplefile*"
                }); verbose
            }
        }

        // .NET build rules
        // build .net executable from C# sources using full .net framework (or mono under unix)
        // notice there's no "out" parameter: csc recipe will use the target file as an output
        "temp/a.exe" ..> csc {src (!!"temp/a.cs" + "temp/AssemblyInfo.cs")}

        // the rule above demands a.cs source file, this rule creates the source file
        "temp/a.cs" ..> writeText
            """
            class Program
            {
            	public static void Main()
            	{
            		System.Console.WriteLine("Hello world!");
            	}
            }
            """

        // this rule gets the version from VERSION script variable and generates
        // define the variable by running `dotnet fake run features.fsx -- -d VERSION:2.1.1`
        "temp/AssemblyInfo.cs" ..> recipe {
            let! envVersion = getVar("VERSION")
            let version = envVersion |> Option.defaultValue "1.0.0"
            do! writeText <| sprintf "[assembly: System.Reflection.AssemblyVersion(\"%s\")]" version
        }


        // defining the target which produces multiple files
        // recipe will be executed just once, regardless how many times its outcome was requested in other targets
        // notice the `*..>` operator is used
        ["app.exe"; "app.xml"] *..> recipe {

            let! [appfile; xmlfile] = getTargetFiles()

            do! Fsc {
                FscSettings with
                    Src = fileset {
                        basedir "core"
                        includes "Logging.fs"
                        includes "Program.fs"
                    }
                    Out = appfile
                    Ref = !! "bin/FSharp.Core.dll"
                    RefGlobal = ["System.dll"; "System.Core.dll"]
                    CommandArgs = ["--utf8output"; "--doc:" + xmlfile.FullName]
            }
        }

        // using wildcards and named groups when defining target
        // The following rule defines how to build any file which names matches "*/*.cs*" pattern.
        // Round brackets specify the named groups (known from regexps), the values can be accessed via getRuleMatch function
        // e.g. for src/hello.csx the matches will be: dir=src, file=hello, ext=csx

        "(dir:*)/(file:*).(ext:c*)" ..> recipe {

            let! dir = getRuleMatch "dir"
            let! file = getRuleMatch "file"
            let! ext = getRuleMatch "ext"

            // here you place regular build steps

            return ()
        }

        "libs" => recipe {
            // this command will copy all dlls to `lib` (flat files)
            do! cp {file "packages/mylib/net46/*.dll"; todir "lib"}

            // this command will copy content of the specified folder to `lib` folder preserving structure starting from packages
            do! cp {dir "packages/theirlib"; todir "lib"}

            // this `cp` accepts fileset and also preserves the directory structure, using basedir as a root of the structure.
            do! cp {
                files (!!"*.exe" @@ "bin")
                todir "deploy"
            }
        }

        // shell commands runs shell command
        "shell" => recipe {
            let! errorLevel = shell {
                cmd "dir"
                args ["*.*"; "/A"]
                workdir "."
            }

            // this will fail the script
            // if errorLevel <> 0 then failwith "command failed"
            // the same results could be obtained by `failonerror` instruction within shell {}

            do! trace Info "dir command finished with %i error code" errorLevel
        }

        // all kind of control flow constructs are supported with recipe
        "control-flow" => recipe {

            // defining recipe
            let log text = recipe {
                do! trace Info "%s" text
            }

            for i in [1;2;3] do
                do! trace Info "Circle %i" i
                if i = 2 then
                    do! log "Fizz"  // use let!, do! to call any recipe
            
            try
                let j = ref 3
                while !j < 5 do
                    do! log (sprintf "j=%i" !j)
                    j := !j + 1                
            with _ ->
                do! trace Error "Exception occured!"
        }

        // working with filesets and dependencies
        "fileset" => recipe {
            let srcFileset = ls "src/*.cs"
            let! files = getFiles srcFileset
            do! needFiles files

            // `let! files...` above records the dependency of `fileset` target from the set of files matching `src/*.cs` pattern. Whenever file is added or removed the dependency will be triggered
            // `do! needFiles` records that `fileset` depends on *contents* of each file matching the mask. It will trigger if file size or timestamp is changed
        }


        // `trace` function demo
        // note: output verbosity is set to Diag to display all messages (see the "consolelog" instruction on top of xakeScript body)

        "tracetest" => recipe {
            do! trace Message "=============== Sample output follows this line\n\n"

            for loglevel in [Level.Command; Level.Message; Level.Error; Level.Warning; Level.Debug; Level.Info; Level.Verbose] do
                do! trace loglevel "Sample text"

            do! trace Message "\n\n\tend of Sample output follows this line"
        }
    ]
}