﻿module FileTasksRm

open NUnit.Framework

open Xake
open Xake.FileTasks

type private File = System.IO.File

let TestOptions = {ExecOptions.Default with Threads = 1; Targets = ["main"]; ConLogLevel = Chatty; FileLogLevel = Silent}

// [<Test>]
//let ``new modules``() =
//    printfn "%s" Cp

//    do xake TestOptions {
//        rules [
//            "main" => recipe {
//                do! Rm {RmArgs.Default with file = "aaa.cs"}
//                do! Rm {RmArgs.Default with dir = "dummy"}
//                do! Rm {RmArgs.Default with files = fileset {includes "**/*.tmp"}; verbose = true}

//                do! rm {file "aaa.cs"}
//                do! rm {dir "aaa"}
//                do! rm {files !! "**/*.tmp"}
//                do! rm {
//                    files (fileset {includes "**/*.tmp"})

//                    verbose
//                    includeemptydirs true
//                }
//            }
//        ]
//    }


//[<Test>]
let ``Rm deletes single file``() =
    "." </> ".xake" |> File.Delete

    do xake TestOptions {
        rules [
            "main" => action {
                do! need ["samplefile"]
                File.Exists "samplefile" |> Assert.True
                //do! rm {file "samplefile"}
            }

            "samplefile" ..> writeTextFile "hello world"
        ]
    }

    File.Exists "samplefile" |> Assert.False

//[<Test>]
let ``Rm deletes files by mask``() =
    "." </> ".xake" |> File.Delete

    do xake TestOptions {
        rules [
            "main" => action {
                do! need ["samplefile"; "samplefile1"]
                File.Exists "samplefile" |> Assert.True
                File.Exists "samplefile1" |> Assert.True
//                do! rm {file "samplefile*"}
            }

            "samplefile" ..> writeTextFile "hello world"
            "samplefile1" ..> writeTextFile "hello world1"
        ]
    }

    File.Exists "samplefile" |> Assert.False
    File.Exists "samplefile1" |> Assert.False

[<Test>]
let ``Rm deletes files in dir``() =
    "." </> ".xake" |> File.Delete

    do xake TestOptions {
        rules [
            "main" => recipe {
                printfn "ddd"
                do! need ["1/samplefile"; "a_b_samplefile1"]
//                File.Exists "a\\b\\samplefile1" |> Assert.True
//                do! rm {dir "a"}
            }

            "1/samplefile" ..> writeTextFile "hello world"
            // "a/b/samplefile1" ..> writeTextFile "hello world1"
        ]
    }

    // System.IO.Directory.Exists "a" |> Assert.False

    
//[<Test>]
let ``Rm deletes empty dirs in dir``() =
    "." </> ".xake" |> File.Delete

    do xake TestOptions {
        rules [
            "main" => action {
                do! need ["a/b/samplefile1"]
//                do! rm {files !! "a/**/*"; includeemptydirs true}
            }

            "a/b/samplefile1" ..> writeTextFile "hello world1"
        ]
    }

    System.IO.Directory.Exists "a" |> Assert.False