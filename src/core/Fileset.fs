[<AutoOpen>]
module Xake.Fileset

open System.IO
open Xake

/// <summary>
/// Defines interface to a file system
/// </summary>
type FileSystemType = {
    GetDisk: string -> string
    GetDirRoot: string -> string
    GetParent: string -> string
    AllDirs: string -> string seq
    ScanDirs: string -> string -> string seq  // mask -> dir -> dirs
    ScanFiles: string -> string -> string seq // mask -> dir -> files
}

type FilePattern = string

/// Filesystem pattern
type FilesetElement = | Includes of Path.PathMask | Excludes of Path.PathMask

type FilesetOptions = {FailOnEmpty:bool; BaseDir:string option}

// Fileset is either set of rules or list of files (materialized)
type Fileset = Fileset of FilesetOptions * FilesetElement list
type Filelist = Filelist of File list

/// Default fileset options
let DefaultOptions = {FilesetOptions.BaseDir = None; FailOnEmpty = false}

let Empty = Fileset (DefaultOptions,[])
let EmptyList = Filelist []

/// Implementation module
module private Impl =

    open Path

    let fullname (f:DirectoryInfo) = f.FullName

    let FileSystem = {
        GetDisk = fun d -> d + Path.DirectorySeparatorChar.ToString()
        GetDirRoot = fun x -> Directory.GetDirectoryRoot x
        GetParent = Directory.GetParent >> fullname
        AllDirs = fun dir -> Directory.EnumerateDirectories(dir, "*", SearchOption.AllDirectories)
        ScanDirs = fun mask dir -> Directory.EnumerateDirectories(dir, mask, SearchOption.TopDirectoryOnly)
        ScanFiles = fun mask dir -> Directory.EnumerateFiles(dir, mask)
    }

    /// <summary>
    ///  Changes current directory
    /// </summary>
    /// <param name="fs">File system implementation</param>
    /// <param name="startIn">Starting path</param>
    /// <param name="path">target path</param>
    let cd (fs:FileSystemType) startIn (Path.PathMask path) =
        // TODO check path exists after each step
        let applyPart (path:string) = function
        | CurrentDir  -> path
        | Disk d      -> fs.GetDisk d
        | FsRoot      -> path |> fs.GetDirRoot
        | Parent      -> path |> fs.GetParent
        | Directory d -> Path.Combine(path, d)
        | _ -> failwith "ChDir could only contain disk or directory names"
        in
        (startIn, path) ||> List.fold applyPart

    let listFiles (fs:FileSystemType) startIn (Path.PathMask pat) =

        // The pattern without mask become "explicit" file reference which is always included in resulting file list, regardless file presence. See impl notes for details.
        let isExplicitRule = pat |> List.exists (function | DirectoryMask _ | FileMask _ | Recurse -> true | _ -> false) |> not
        let filterDir = if isExplicitRule then id else Seq.filter Directory.Exists
        let filterFile = if isExplicitRule then id else Seq.filter File.Exists

        // Recursively applies the pattern rules to every item is start list
        let applyPart (paths: seq<string>) :_ -> seq<string> = function
        | Disk d          -> fs.GetDisk d |> Seq.singleton
        | FsRoot          -> paths |> Seq.map fs.GetDirRoot
        | CurrentDir      -> paths
        | Parent          -> paths |> Seq.map fs.GetParent
        | Recurse         -> paths |> Seq.collect fs.AllDirs |> Seq.append paths
        | DirectoryMask mask -> paths |> Seq.collect (fs.ScanDirs mask)
        | Directory d     -> paths |> Seq.map (fun dir -> Path.Combine(dir, d)) |> filterDir
        | FileMask mask   -> paths |> Seq.collect (fs.ScanFiles mask)
        | FileName f      -> paths |> Seq.map (fun dir -> Path.Combine(dir, f)) |> filterFile
        in
        (startIn, pat) ||> List.fold applyPart

    let ifNone = Option.fold (fun _ -> id)

    /// Implementation of fileset execute
    /// "Materializes" fileset to a filelist
    let scan fileSystem root (Fileset (options,filesetItems)) =

        let startDirPat = options.BaseDir |> ifNone root |> Path.parseDir
        let startDir = startDirPat |> cd fileSystem "."

        // TODO check performance, build function
        let includes src = [startDir] |> (listFiles fileSystem) >> Seq.append src
        let excludes src pat =
            let matchFile = Path.join startDirPat pat |> Path.matchesPattern in
            src |> Seq.filter (matchFile >> not)

        let folditem i = function
            | Includes pat -> includes i pat
            | Excludes pat -> excludes i pat

        filesetItems |> Seq.ofList |> Seq.fold folditem Seq.empty<string> |> Seq.map File.make |> List.ofSeq |> Filelist

    // combines two fileset options
    let combineOptions (o1:FilesetOptions) (o2:FilesetOptions) =
        {DefaultOptions with
            BaseDir =
                match o1.BaseDir,o2.BaseDir with
                | Some _, Some _ -> failwith "Cannot combine filesets with basedirs defined in both (not implemented)"
                | Some _, None -> o1.BaseDir
                | _ -> o2.BaseDir
            FailOnEmpty = o1.FailOnEmpty || o2.FailOnEmpty}

    // combines two filesets
    let combineWith (Fileset (o2, set2)) (Fileset (o1,set1)) = Fileset(combineOptions o1 o2, set1 @ set2)

    // Combines result of reading file to a fileset
    let combineWithFile map (file:FileInfo) (Fileset (opts,fs)) =
        let elements = File.ReadAllLines file.FullName |> Array.toList |> List.map map in
        Fileset (opts, fs @ elements)
        // TODO filter comments, empty lines? |> Array.filter

    let changeBasedir dir (Fileset (opts,ps)) =   Fileset ({opts with BaseDir = Some dir}, ps)
    let changeFailonEmpty f (Fileset (opts,ps)) = Fileset ({opts with FailOnEmpty = f}, ps)

/// Fileset persistance implementation
module private PicklerImpl =

    open Pickler

    let filesetoptions =
        wrap(
            (fun(foe,bdir) -> {FilesetOptions.FailOnEmpty = foe; BaseDir = bdir}),
            fun o -> (o.FailOnEmpty, o.BaseDir))
            (pair bool (option str))

    let filesetElement =
      alt
        (function | Includes _ -> 0 | Excludes _ -> 1)
        [|
          wrap (Includes, fun (Includes p | OtherwiseFail p) -> p) Path.pickler
          wrap (Excludes, fun (Excludes p | OtherwiseFail p) -> p) Path.pickler
        |]

    let fileinfo = wrap(File.make, File.getFullName) str

    let fileset  = wrap(Fileset, fun (Fileset (o,l)) -> o,l) (pair filesetoptions (list filesetElement))
    let filelist = wrap(Filelist, fun (Filelist l) -> l) (list fileinfo)

open Impl

/// Gets the pickler for fileset type
let filesetPickler = PicklerImpl.fileset
let filelistPickler = PicklerImpl.filelist

/// <summary>
/// Creates a new fileset with default options.
/// </summary>
/// <param name="filePattern"></param>
let ls (filePattern:FilePattern) =
    // TODO Path.parse is expected to handle trailing slash character
    let parse = match filePattern.EndsWith ("/") || filePattern.EndsWith ("\\") with | true -> Path.parseDir | _-> Path.parse
    Fileset (DefaultOptions, [filePattern |> parse |> Includes])

/// <summary>
/// Create a file set for specific file mask. The same as "ls"
/// </summary>
let (!!) = ls

/// <summary>
/// Defines the empty fileset with a specified base dir.
/// </summary>
/// <param name="dir"></param>
let (~+) dir =
    Fileset ({DefaultOptions with BaseDir = Some dir}, [])

[<System.Obsolete("Use Path.parse instead")>]
let parseFileMask = Path.parse

[<System.Obsolete("Use Path.parseDir instead")>]
let parseDirMask = Path.parseDir

// let matches filePattern projectRoot
[<System.Obsolete("Use Path.matches instead")>]
let matches = Path.matches

let FileSystem = Impl.FileSystem

/// <summary>
/// "Materializes" fileset to a filelist
/// </summary>
let toFileList = Impl.scan Impl.FileSystem

/// <summary>
/// "Materializes" file mask to a list of files/paths
/// </summary>
let listByMask (root:string) = Impl.listFiles Impl.FileSystem [root]

/// <summary>
/// The same as toFileList but allows to provide file system adapter
/// </summary>
let toFileList1 = Impl.scan

type ListDiffType<'a> = | Added of 'a | Removed of 'a

/// <summary>
/// Compares two file lists and returns differences list.
/// </summary>
/// <param name="list1"></param>
/// <param name="list2"></param>
let compareFileList (Filelist list1) (Filelist list2) =

    let setOfNames = List.map File.getFullName >> Set.ofList

    let set1, set2 = setOfNames list1, setOfNames list2

    let removed = Set.difference set1 set2 |> List.ofSeq |> List.map (Removed)
    let added = Set.difference set2 set1 |> List.ofSeq |> List.map (Added)

    removed @ added

/// <summary>
/// Defines various operations on Fieset type.
/// </summary>
type Fileset with
    static member (+) (fs1, fs2: Fileset) :Fileset = fs1 |> combineWith fs2
    static member (+) (fs1: Fileset, pat) = fs1 ++ pat
    static member (-) (fs1: Fileset, pat) = fs1 -- pat
    static member (@@) (fs1, basedir) = fs1 |> Impl.changeBasedir basedir
    static member (@@) (Fileset (_,lst), options) = Fileset (options,lst)

    /// Conditional include/exclude operator
    static member (+?) (fs1: Fileset, (condition:bool,pat: FilePattern)) = if condition then fs1 ++ pat else fs1
    static member (+?) (fs1: Fileset, (condition:bool,fs2: Fileset)) :Fileset = if condition then fs1 |> combineWith fs2 else fs1
    static member (-?) (fs1: Fileset, (condition:bool,pat: FilePattern)) = if condition then fs1 -- pat else fs1

    /// Adds includes pattern to a fileset.
    static member (++) ((Fileset (opts,pts)), includes) :Fileset =
        Fileset (opts, pts @ [includes |> Path.parse |> Includes])

    /// Adds excludes pattern to a fileset.
    static member (--) (Fileset (opts,pts), excludes) =
        Fileset (opts, pts @ [excludes |> Path.parse |> Excludes])
end

(******** builder ********)
type FilesetBuilder() =

    [<CustomOperation("failonempty")>]
    member __.FailOnEmpty(fs,f) = fs |> changeFailonEmpty f

    [<CustomOperation("basedir")>]
    member __.Basedir(fs,dir) = fs |> changeBasedir dir

    [<CustomOperation("includes")>]
    member __.Includes(fs:Fileset,pattern) = fs ++ pattern

    [<CustomOperation("includesif")>]
    member __.IncludesIf(fs:Fileset,condition, pattern:FilePattern) =    fs +? (condition,pattern)

    [<CustomOperation("join")>]
    member __.JoinFileset(fs1, fs2) = fs1 |> Impl.combineWith fs2

    [<CustomOperation("excludes")>]
    member __.Excludes(fs:Fileset, pattern) = fs -- pattern

    [<CustomOperation("excludesif")>]
    member __.ExcludesIf(fs:Fileset, pattern) = fs -? pattern

    [<CustomOperation("includefile")>]
    member __.IncludeFile(fs, file)  = (fs,file) ||> combineWithFile (Path.parse >> Includes)

    [<CustomOperation("excludefile")>]
    member __.ExcludeFile(fs,file)    = (fs,file) ||> combineWithFile (Path.parse >> Excludes)

    member __.Yield(())    = Empty
    member __.Return(pattern:FilePattern) = Empty ++ pattern

    member __.Combine(fs1, fs2) = fs1 |> Impl.combineWith fs2
    member __.Delay(f) = f()
    member this.Zero() = this.Yield ( () )

    member __.Bind(fs1:Fileset, f) = let fs2 = f() in fs1 |> Impl.combineWith fs2
    member x.For(fs, f) = x.Bind(fs, f)
    member x.Return(a) = x.Yield(a)

let fileset = FilesetBuilder()
