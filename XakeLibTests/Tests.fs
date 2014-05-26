﻿namespace XakeLibTests

open System.IO
open NUnit.Framework

open Xake
open Storage
open BuildLog

[<TestFixture (Description = "Verious tests")>]
type MiscTests() =

    [<Test (Description = "Verifies need is executed only once")>]
    member test.NeedExecutesOnes() =

        let wasExecuted = ref []
        let needExecuteCount = ref 0
    
        do xake {XakeOptions with Threads = 1} {  // one thread to avoid simultaneous access to 'wasExecuted'
            want (["test"; "test1"])

            rules [
              "test" => action {
                  do! writeLog Error "Running inside 'test' rule"
                  do! need ["aaa"]
                  wasExecuted := ("test" :: !wasExecuted)
              }
              "test1" => action {
                  do! writeLog Error "Running inside 'test1' rule"
                  do! need ["aaa"]
                  wasExecuted := ("test1" :: !wasExecuted)
              }
              "aaa" => action {
                  needExecuteCount := !needExecuteCount + 1
              }
            ]
        }

        Assert.IsTrue(!wasExecuted |> List.exists ((=) "test"))
        Assert.IsTrue(!wasExecuted |> List.exists ((=) "test1"))

        Assert.AreEqual(1, !needExecuteCount)

    [<Test (Description = "Verifies need is executed only once")>]
    member test.SkipTask() =

        let needExecuteCount = ref 0
    
        do xake {XakeOptions with Threads = 1} {  // one thread to avoid simultaneous access to 'wasExecuted'
            want (["hello"])

            rules [
              "hello" *> fun file -> action {
                  do! writeLog Error "Running inside 'hello' rule"
                  do! need ["hello.cs"]

                  do! writeLog Error "Rebuilding..."
                  do! Csc {
                    CscSettings with
                      Out = file
                      Src = !!"hello.cs"
                  }
              }
              "hello.cs" *> fun src -> action {
                  do File.WriteAllText (src.FullName, """class Program
                    {
	                    public static void Main()
	                    {
		                    System.Console.WriteLine("Hello world!");
	                    }
                    }""")
                  do! writeLog Error "Done building 'hello.cs' rule in %A" src
                  needExecuteCount := !needExecuteCount + 1
              }
            ]
        }

        Assert.AreEqual(1, !needExecuteCount)

    [<Test (Description = "Verifies need is executed only once")>]
    member test.BuildLog() =

        let cdate = System.DateTime(2014, 1, 2, 3, 40, 50);

        let dbname = "." </> ".xake"
        try File.Delete(dbname) with _ -> ()

        // execute script, check what's in build log
        File.WriteAllText ("bbb.c", "// empty file")
        FileInfo("bbb.c").LastWriteTime <- cdate
    
        do xake {XakeOptions with Threads = 1} {  // one thread to avoid simultaneous access to 'wasExecuted'
            want (["test"; "test1"])

            rules [
              "test" => action {
                  do! need ["aaa"]

                  // check nested actions are also collected
                  do! action {
                    do! action {
                      do! need ["deeplyNested"]
                      do! need ["bbb.c"]
                    }
                  }
              }
              "test1" => action {
                  do! need ["aaa"]
              }
              "aaa" => action {
                return ()
              }
              "deeplyNested" => action {
                return ()
              }
            ]
        }

        use testee = Storage.openDb "." (ConsoleLogger Verbosity.Diag)
        try

          let (Some buildResult) = testee.PostAndReply <| fun ch -> DatabaseApi.GetResult ((PhonyAction "test"), ch)
          let buildResult = {buildResult with Built = cdate}

          //printf
          Assert.AreEqual (
            {
              BuildResult.Result = PhonyAction "test"
              Built = cdate
              Depends =
                [ArtifactDep (PhonyAction "aaa"); ArtifactDep (PhonyAction "deeplyNested"); File (Artifact @"C:\projects\Mine\xake\bin\bbb.c", cdate)]
              BuildResult.Steps = []
            },
            buildResult)

          let (Some buildResult) = testee.PostAndReply <| fun ch -> DatabaseApi.GetResult ((PhonyAction "test1"), ch)

          let buildResult = {buildResult with Built = cdate}
          Assert.AreEqual (
            {
              BuildResult.Result = PhonyAction "test1"
              BuildResult.Built = cdate
              BuildResult.Depends = [ArtifactDep (PhonyAction "aaa")]
              BuildResult.Steps = []
            },
            buildResult)

        finally
          testee.PostAndReply DatabaseApi.CloseWait