module RFTimeline.ConfigParser

open System
open System.IO
open System.Text.RegularExpressions

//TODO: Allow custom video id partition size
type AppConfig = {APIKey : string; PlaylistIDs : string list} 

let selectfirstmatchgroup regex line =
    Regex.Match(line,regex) // Theope following throws an exception if there are no match groups defined.
    |> fun result -> if result.Success then Some((result.Groups.Item 1).Value) else None

let parseapikey line = selectfirstmatchgroup "^apikey: {0,1}(\S+)" line // Allow apikey:key or apikey: key ignore everything after
let parseplaylistid line = selectfirstmatchgroup "^(\S+)" line // Grab first value before a space.

//TODO: Optimize    
let parseconfig lines =
    let apikey = Seq.head lines |> parseapikey
    let playlistids = Seq.skip 2 lines |> Seq.map(parseplaylistid)
    match apikey with
    | None -> None
    | Some apikey ->
        let addplaylistids ids = {APIKey = apikey; PlaylistIDs = ids} 
        match Seq.forall(function Some _ -> true | None -> false) playlistids with
        | false -> None
        | true -> Some (addplaylistids (Seq.choose id playlistids |> Seq.toList))
        
let openconfig path =
    try
        let filelines = File.ReadAllLines(path) //File.ReadLines apparently closing and throwing exception with message that stream is closed
        match parseconfig filelines with //should dispose properly
        | Some config -> Some config
        | None -> 
            do printfn "Error parsing config file at %s:" path
            None
    with
    | ex ->
        do printfn "Error opening config file at %s: %s" path ex.Message
        None    
