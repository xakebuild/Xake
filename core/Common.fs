﻿// common tasks

[<AutoOpen>]
module Xake.Common

open Xake
open System.IO
open System.Diagnostics

type internal SystemOptionsType = {LogPrefix:string; StdOutLevel: Level; ErrOutLevel: Level}
let internal SystemOptions = {LogPrefix = ""; StdOutLevel = Level.Info; ErrOutLevel = Level.Error}

// internal implementation
let internal _system settings cmd args =
  action {
    let! ctx = getCtx()
    let log = ctx.Logger.Log

    let pinfo =
      ProcessStartInfo
        (cmd, args,
          UseShellExecute = false, WindowStyle = ProcessWindowStyle.Hidden,
          RedirectStandardError = true, RedirectStandardOutput = true)

    let proc = new Process(StartInfo = pinfo)

    proc.ErrorDataReceived.Add(fun  e -> if e.Data <> null then log settings.ErrOutLevel "%s %s" settings.LogPrefix e.Data)
    proc.OutputDataReceived.Add(fun e -> if e.Data <> null then log settings.StdOutLevel  "%s %s" settings.LogPrefix e.Data)

    do proc.Start() |> ignore

    do proc.BeginOutputReadLine()
    do proc.BeginErrorReadLine()

    // task might be completed by that time
    do! Async.Sleep 50
    if proc.HasExited then
      return proc.ExitCode
    else
      proc.EnableRaisingEvents <- true
      do! Async.AwaitEvent proc.Exited |> Async.Ignore
      return proc.ExitCode
  }

// joins and escapes strings
let internal joinArgs (args:#seq<string>) =
  (" ", args |> Array.ofSeq) |> System.String.Join

// executes command
let internal _cmd cmdline (args : string list) =
  action {
    let! exitCode = _system SystemOptions "cmd.exe" (joinArgs (["/c"; cmdline] @ args))
    return exitCode
  } 

// executes external process and waits until it completes
let system cmd args =
  action {
    do! writeLog Info "[system] starting '%s'" cmd
    let! exitCode = _system SystemOptions cmd (joinArgs args)
    do! writeLog Info "[system] сompleted '%s' exitcode: %d" cmd exitCode
    return exitCode
  }

// executes command
let cmd cmdline (args : string list) =
  action {
    do! writeLog Level.Info "[cmd] starting '%s'" cmdline
    let! exitCode = _cmd cmdline args
    do! writeLog Level.Info "[cmd] completed '%s' exitcode: %d" cmdline exitCode
    return exitCode
  } 

// reads the file and returns all text
let readtext artifact =
  artifact |> getFullname |> File.ReadAllText

/// Gets true if running under Mono framework
let internal isRunningOnMono =
  System.Type.GetType ("Mono.Runtime") <> null
