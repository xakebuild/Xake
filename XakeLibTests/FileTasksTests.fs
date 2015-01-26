﻿
namespace XakeLibTests

open System
open System.IO
open NUnit.Framework

open Xake
open Xake.FileTasks

[<TestFixture>]
type FileTasksTests() = 

    let TestOptions = {XakeOptions with Threads = 1; Want = ["main"]; ConLogLevel = Chatty; FileLogLevel = Silent}

    [<Test>]
    member this.DeleteSimple() =
        "." </> ".xake" |> File.Delete

        let execCount = ref 0
        do xake TestOptions {
            rules [
              "main" => action {
                  execCount := !execCount + 1
                  do! need ["samplefile"]
                  Assert.IsTrue <| File.Exists ("samplefile")
                  do! rm ["samplefile"]
              }

              "samplefile" *> fun file -> action {
                  File.WriteAllText(file.FullName, "hello world")
              }
            ]
        }

        Assert.AreEqual(1, !execCount)
        Assert.IsFalse <| File.Exists ("samplefile")

    [<Test>]
    member this.DeleteByMask() =
        "." </> ".xake" |> File.Delete
        let execCount = ref 0
    
        do xake TestOptions {
            rules [
              "main" => action {
                  do! need ["$$1"; "$$2"]
                  Assert.IsTrue <| File.Exists ("$$2")
                  do! rm ["$$*"]
                  execCount := !execCount + 1
              }

              "$$*" *> fun file -> action {
                  File.WriteAllText(file.FullName, "hello world")
              }
            ]
        }

        Assert.AreEqual(1, !execCount)
        ["$$1"; "$$2"] |> List.iter (Assert.IsFalse << File.Exists)

    [<Test>]
    member this.DeleteMany() =
        "." </> ".xake" |> File.Delete
        do xake TestOptions {
            rules [
              "main" => action {
                  do! need ["$aa"; "$bb"]
                  Assert.IsTrue <| File.Exists ("$bb")
                  do! rm ["$aa"; "$b*"]
              }

              "$*" *> fun file -> action {
                  File.WriteAllText(file.FullName, "hello world")
              }
            ]
        }

        ["$aa"; "$bb"] |> List.iter (Assert.IsFalse << File.Exists)

    [<Test>]
    member this.CopySimple() =
        "." </> ".xake" |> File.Delete
        do xake TestOptions {
            rules [
              "main" => action {
                  do! writeLog Error "Running inside 'main' rule"
                  do! need ["aaa"; "clean"]
                  do! cp "aaa" "aaa-copy"
              }

              "clean" => action {
                  do! rm ["aaa-copy"]
              }

              "aaa" *> fun file -> action {
                  File.WriteAllText(file.FullName, "hello world")
              }
            ]
        }

        Assert.IsTrue <| File.Exists ("aaa-copy")