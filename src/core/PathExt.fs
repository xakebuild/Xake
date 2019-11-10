[<AutoOpen>]
module Xake.PathExt

open System.IO

/// <summary>
/// Changes or appends file extension.
/// </summary>
let (-.) path ext = Path.ChangeExtension(path, ext)

/// <summary>
/// Combines two paths.
/// </summary>
let (</>) path1 path2 = Path.Combine(path1, path2)

/// <summary>
/// Appends the file extension.
/// </summary>
let (<.>) path ext = if System.String.IsNullOrWhiteSpace(ext) then path else path + "." + ext
