module Xake.Path

open System.IO
open System.Text.RegularExpressions

type Part =
    | FsRoot
    | Parent
    | CurrentDir
    | Disk of string
    | DirectoryMask of string
    | Directory of string
    | Recurse
    | FileMask of string
    | FileName of string

type PathMask = PathMask of Part list

type MatchResult =
    | Matches of (string*string) list
    | Nope

module private impl =

    let notNullOrEmpty = System.String.IsNullOrEmpty >> not

    let driveRegex = Regex(@"^[A-Za-z]:$", RegexOptions.Compiled)
    let isMask (a:string) = a.IndexOfAny([|'*';'?'|]) >= 0
    let iif fn b c a = match fn a with | true -> b a | _ -> c a
    
    let isRoot = function | FsRoot::_ | Disk _::_ -> true | _ -> false

    /// <summary>
    /// Normalizes the pattern by resolving parent references and removing \.\
    /// </summary>
    let rec normalize = function
        | [] -> []
        | [x] -> [x]
        | x::tail ->               
            match x::(normalize tail) with
            | Directory _::Parent::t -> t
            | CurrentDir::t -> t
            | rest -> rest

    /// <summary>
    /// Maps part of file path to a path part.
    /// </summary>
    /// <param name="mapPart"></param>
    let mapPart isLast = function
        | "**" -> Recurse
        | "." -> CurrentDir
        | ".." -> Parent (* works well now with Path.Combine() *)
        | a when a.EndsWith(":") && driveRegex.IsMatch(a) -> Disk(a)
        | a when not isLast -> a |> iif isMask DirectoryMask Directory
        | a -> a |> iif isMask FileMask FileName

    let parse isLastPart pattern =
     
        if notNullOrEmpty pattern then
            let parts = pattern.Split([|'\\'; '/'|], System.StringSplitOptions.RemoveEmptyEntries)
            let fsroot = pattern.[0] |> function | '\\' | '/' -> [FsRoot] | _ -> []
            in
            let isLast = isLastPart parts
            fsroot @ (parts |> Array.mapi (isLast >> mapPart) |> List.ofArray)
                |> normalize |> PathMask
        else
            PathMask []

    /// <summary>
    /// supplementary function for parsing directory
    /// </summary>
    let isLastPartForDir _ _ = false
    /// <summary>
    /// supplementary function for parsing file
    /// </summary>
    let isLastPartForFile (parts:_ array) = (=) (parts.Length-1)

    let dirSeparator = string Path.DirectorySeparatorChar
    let partToString =
        function
        | Directory s
        | FileName s
        | DirectoryMask s
        | FileMask s
            -> s
        | Parent -> ".."
        | Part.CurrentDir -> "."
        | Part.Disk d -> d + dirSeparator
        | Part.Recurse -> "**"
        | Part.FsRoot -> dirSeparator


module private PicklerImpl =

    open Pickler

    let patternpart=
        alt(function
            | FsRoot -> 0
            | Parent -> 1
            | Disk _ -> 2
            | DirectoryMask _ -> 3
            | Directory _ -> 4
            | Recurse -> 5
            | FileMask _ -> 6
            | FileName _ -> 7
            | CurrentDir -> 8
            )
          [|
            wrap0 FsRoot
            wrap0 Parent
            wrap (Disk, fun (Disk d | OtherwiseFail d) -> d) str
            wrap (DirectoryMask, fun (DirectoryMask d | OtherwiseFail d) -> d) str
            wrap (Directory, fun (Directory d | OtherwiseFail d) -> d) str
            wrap0 Recurse
            wrap (FileMask, fun (FileMask m | OtherwiseFail m) -> m) str
            wrap (FileName, fun (FileName m | OtherwiseFail m) -> m) str
            wrap0 CurrentDir
          |]

    let pattern = wrap(PathMask, fun(PathMask pp) -> pp) (list patternpart)

module internal matchImpl =

    let eq s1 s2 = System.StringComparer.OrdinalIgnoreCase.Equals(s1, s2)

    let wildcard2regexMap =
       ["**", "(.*)"
        "*", """([^/\\]*)"""
        "?", "([^/\\\\])"
        ".", "\\."; "$", "\\$"; "^", "\\^"; "[", "\\["; "]", "\\]"
        "+", "\\+"; "!", "\\!"; "=", "\\="; "{", "\\{"; "}", "\\}"
       ] |> dict
       
    let wildcardToRegex (m:Match) =
        match m.Groups.Item("tag") with
        | t when not t.Success ->
            match wildcard2regexMap.TryGetValue(m.Value) with
            | true, v -> v
            | _ -> m.Value
        | t -> "(?<" + t.Value + ">"

    let normalizeSlashes (pat: string) =
        pat.Replace('\\', '/')

    let maskToRegex (pattern:string) =
        let pat = Regex.Replace(pattern |> normalizeSlashes, @"\((?'tag'\w+?)\:|\*\*|([*.?$^+!={}])", wildcardToRegex)
        // TODO mask with sq brackets
        let ignoreCase = if Env.isUnix then RegexOptions.None else RegexOptions.IgnoreCase
        in
        Regex(@"^" + pat + "$", RegexOptions.Compiled + ignoreCase)

    let matchPart (mask:Part) (path:Part) =
        let matchByMask (rx:Regex) value = rx.Match(value).Success
        match mask,path with
        | (FsRoot, FsRoot) -> true
        | (Disk mask, Disk d) | (Directory mask, Directory d) | FileName mask, FileName d when eq mask d -> true

        | DirectoryMask mask, Directory d | FileMask mask, FileName d ->
            matchByMask (maskToRegex mask) d

        | _ -> false

    let rec matchPaths (mask:Part list) (p:Part list) =
        match mask,p with
        | [], [] -> true
        | [], _ | _, [] -> false

        | Directory _::Recurse::Parent::ms, _ -> matchPaths (Recurse::ms) p
        | Recurse::Parent::ms, _              -> matchPaths (Recurse::ms) p    // ignore parent ref

        | Recurse::ms, (FileName _)::_ -> matchPaths ms p
        | Recurse::ms, Directory _::xs -> (matchPaths mask xs) || (matchPaths ms p)
        | m::ms, x::xs ->
            matchPart m x && matchPaths ms xs

// API
let pickler = PicklerImpl.pattern

/// <summary>
/// Converts path to string representation (platform specific).
/// </summary>
let toString =
    List.map impl.partToString
    >> List.fold (fun s ps -> Path.Combine (s, ps)) ""

/// <summary>
/// Joins two patterns.
/// </summary>
/// <param name="p1"></param>
/// <param name="p2"></param>
let join (PathMask p1) (PathMask p2) =
    match impl.isRoot p2 with
    | true -> PathMask p2
    | _ -> p1 @ p2 |> impl.normalize |> PathMask

/// <summary>
/// Converts Ant-style file pattern to a list of parts. Assumes the path specified
/// </summary>
let parseDir = impl.parse impl.isLastPartForDir

/// <summary>
/// Converts Ant-style file pattern to a PathMask.
/// </summary>
let parse = impl.parse impl.isLastPartForFile

(*
/// <summary>
/// Returns true if a file name (parsed to p) matches specific file mask.         
/// </summary>
/// <param name="mask"></param>
/// <param name="file"></param>
let matchesPattern (pattern:string) =

    let regex = matchImpl.maskToRegex pattern
    fun file -> regex.Match(matchImpl.normalizeSlashes file).Success
*)
let matchesPattern (PathMask mask) file =
    let (PathMask fileParts) = file |> impl.parse impl.isLastPartForFile in
    matchImpl.matchPaths mask fileParts

let matches filePattern rootPath =
    // IDEA: make relative path then match to pattern?
    // matches "src/**/*.cs" "c:\!\src\a\b\c.cs" -> true

    matchesPattern <| join (parseDir rootPath) (parse filePattern)

/// file name match implementation for rules
let matchGroups (pattern:string) rootPath =

    let regex = Path.Combine(rootPath, pattern) |> matchImpl.maskToRegex
    fun file -> 
        let m = regex.Match(matchImpl.normalizeSlashes file)
        if m.Success then
            [for groupName in regex.GetGroupNames()  do
                let group = m.Groups.[groupName]
                yield groupName, group.Value] |> Some
        else
            None

