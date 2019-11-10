namespace Xake

module private impl =

  let compareNames : string -> string -> int =
      let ignoreCase = Env.isUnix
      fun a b -> System.String.Compare(a, b, ignoreCase)

  let getFileHash : string -> int =
      if Env.isUnix then
          fun name -> name |> hash
      else
          fun name -> name.ToLowerInvariant() |> hash

open impl
open System.IO

[<CustomEquality; CustomComparison>]
type File = T of string * System.IO.FileInfo with

    // Note the Name used in application output for better readability

    member f.Name = let (T (n,_)) = f in n
    member f.FullName = let (T (_,f)) = f in f |> function |null -> "" |_ -> f.FullName

    static member undefined = T ("",null)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? File as y -> compareNames (x.FullName) (y.FullName)
            | _ -> invalidArg "yobj" "cannot compare values of different types"

    override x.Equals(yobj) =
        match yobj with
        | :? File as y -> 0 = compareNames x.FullName y.FullName
        | _ -> false
    override x.GetHashCode() =
        if x = File.undefined then "" else x.FullName
        |> getFileHash

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module File =

    type String = System.String

    (*
        Need a file type which:
         * effectively handles relativity to a root folder - WHY it's important? - for relocation of .xake?
         * could compare to other file regardless the way it's specified
         * could be both relative and absolute

         Rule is defined for files matching the path mask.
    *)

    type private BclFile = System.IO.File

    let make n =
        if String.IsNullOrWhiteSpace n then
            failwith "File name cannot be empty"

        T (n, System.IO.FileInfo n)

    let getFileName (f:File) = f.Name |> Path.GetFileName
    let getFileExt (f:File) = f.Name |> Path.GetExtension
    let getDirName (f:File) = f.FullName |> Path.GetDirectoryName
    let getFullName (f:File) = f.FullName
    let exists (f:File) = BclFile.Exists f.FullName
    let getLastWriteTime (f:File) = BclFile.GetLastWriteTime f.FullName
