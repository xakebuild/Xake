// common tasks
module internal Xake.ProcessExec

open System.Diagnostics

// internal implementation
let pexec handleStd handleErr cmd args (envvars:(string * string) list) workDir =
    let pinfo =
      ProcessStartInfo
        (cmd, args,
          UseShellExecute = false, WindowStyle = ProcessWindowStyle.Hidden,
          RedirectStandardError = true, RedirectStandardOutput = true)

    for name,value in envvars do            
        pinfo.EnvironmentVariables.[name] <- value

    match workDir with
    | Some path -> pinfo.WorkingDirectory <- path
    | _ -> ()

    let proc = new Process(StartInfo = pinfo)

    proc.ErrorDataReceived.Add(fun e -> if e.Data <> null then handleErr e.Data)
    proc.OutputDataReceived.Add(fun e -> if e.Data <> null then handleStd e.Data)

    do proc.Start() |> ignore

    do proc.BeginOutputReadLine()
    do proc.BeginErrorReadLine()

    // task might be completed by that time
    Async.RunSynchronously <|
    async {
        do! Async.Sleep 50
        if proc.HasExited then
            return proc.ExitCode
        else
            proc.EnableRaisingEvents <- true
            do! Async.AwaitEvent proc.Exited |> Async.Ignore
            return proc.ExitCode
    }
