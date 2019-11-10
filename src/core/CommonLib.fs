[<AutoOpen>]
module internal Xake.CommonLib

type private CacheKey<'K> = K of 'K

/// <summary>
/// Creates a memoized function with the same signature. Performs memoization by storing run results to a cache.
/// </summary>
/// <param name="f"></param>
let memoize f =
    let cache = ref Map.empty
    let lck = System.Object()
    fun x ->
        match !cache |> Map.tryFind (K x) with
        | Some v -> v
        | None ->
            lock lck (fun () ->
                match !cache |> Map.tryFind (K x) with
                | Some v -> v
                | None ->
                    let res = f x
                    cache := !cache |> Map.add (K x) res
                    res)


///**Description**
/// Memoizes the recursive function. Memoized function is passed as first argument to f.
///**Parameters**
///  * `f` - parameter of type `('a -> 'b) -> 'a -> 'b` The function to be memoized.
///
///**Output Type**
///  * `'a -> 'b`
///
///**Exceptions**
///
let memoizeRec f =
    let rec fn x = f fm x
    and fm = fn |> memoize
    in
    fm

/// <summary>
/// Takes n first elements from a list.
/// </summary>
/// <param name="cnt"></param>
let rec take cnt = function |_ when cnt <= 0 -> [] |[] -> [] |a::rest -> a :: (take (cnt-1) rest)

/// <summary>
/// Returns a list of unique values for a specific list.
/// </summary>
/// <param name="ls"></param>
let distinct ls =        
    ls |>
    List.fold (fun map item -> if map |> Map.containsKey item then map else map |> Map.add item 1) Map.empty
    |> Map.toList |> List.map fst
