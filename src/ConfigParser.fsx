module RFTimeline.ConfigParser

#r "../packages/FSharp.Data/FSharp.Data.dll"

open System
open System.IO
open FSharp.Data

//TODO: Allow custom video id partition size
type AppConfig = 
    { APIKey : string
      PlaylistIDs : string list
      Filtered : string list
      Ungrouped : string list } 


[<Literal>]
let configformat = 
    """
{
  "apikey": "youtube api key",
  "playlists": [
    {"Channel":"channel name", "PlaylistID":"channel id"}, 
    {"Channel":"channel name", "PlaylistID":"channel id"}
  ],
  "ungrouped": [
    "video id 1",
    "video id 2"
  ],
  "filtered": [
    "ignored video id 1",
    "ignored video id 2"
  ]
}
    """


type ConfigParser = JsonProvider<configformat>

let openconfig (path : string) =
    try
        let config = ConfigParser.Load(path)
        let apikey = config.Apikey
        let playlists = List.ofArray (config.Playlists |> Array.map(fun c -> c.PlaylistId))
        let filtered = List.ofArray config.Filtered
        let ungrouped = List.ofArray config.Ungrouped
        Some { APIKey = apikey; PlaylistIDs = playlists; Filtered = filtered; Ungrouped = ungrouped }
    with
    | ex ->
        do printfn "Error opening config file at %s: %s" path ex.Message
        None
