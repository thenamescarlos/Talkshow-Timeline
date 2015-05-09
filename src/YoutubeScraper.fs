module RFTimeline.YoutubeScraper

#r "../packages/FSharp.Data/FSharp.Data.dll"

open System
open FSharp.Data

//Strongly typed json thanks to FSharp.Data

type YoutubeVideo = JsonProvider< "../YoutubeAPI/Video.json" >

type YoutubePlaylistItems = JsonProvider< "../YoutubeAPI/PlaylistItems.json" >

//Abbreviations for pretty type inference

type YoutubePlaylistItemsResponse = YoutubePlaylistItems.Root

type YoutubeVideoSearchResponse = YoutubeVideo.Root

type YoutubeVideoResponse = YoutubeVideo.Item

type YoutubeVideoID = string

//Type to be consumed by other functions

type FilteredVideoRequest = 
    { Title : string
      Uploader : string
      Description : string
      ThumbnailLink : string
      VideoID : string
      ISODuration : string
      Views : int
      Likes : int
      Favorited : int }

//Functions for constructing a request url

let urlrequest apikey content filter = 
    filter >> sprintf "%s%s&key=%s&%s" "https://www.googleapis.com/youtube/v3/" content apikey

let urlVideo apikey filter = urlrequest apikey "videos?part=snippet,statistics,contentDetails" filter

let urlPlaylistVideos apikey filter = urlrequest apikey "playlistItems?part=id,snippet&maxResults=50" filter

let urlPlaylistVideosfromID apikey playlistid = urlPlaylistVideos apikey (sprintf "playlistId=%s") playlistid

let urlVideofromID apikey videoid = urlVideo apikey (sprintf "id=%s") videoid

let urlVideofromIDs apikey videoids = urlVideo apikey (String.concat "," >> sprintf "id=%s") videoids

let playlistrequest (url : string) : YoutubePlaylistItemsResponse option = 
    try 
        Some(YoutubePlaylistItems.Load(url))
    with ex -> //Output error on console
        do printfn "Error grabbing playlist with url %s - Message %s" url ex.Message
        None

//TODO: Thought a PlaylistItem id would be a video id, but it's not. Go back to partially filling filteredvideoinfo from playlistitem
let playlistvideos url : YoutubeVideoID seq option = 
    let nextplaylisturl url token = sprintf "%s&pageToken=%s" url token
    
    let rec paginate videoids (result : YoutubePlaylistItemsResponse) = 
        match result.NextPageToken with
        | token when String.IsNullOrWhiteSpace(token) -> Seq.append result.Items videoids
        | token -> YoutubePlaylistItems.Load(nextplaylisturl url token) |> paginate (Seq.append result.Items videoids)
    playlistrequest url |> Option.map (paginate [||] >> Seq.map (fun p -> p.Snippet.ResourceId.VideoId))

let filtervideoresponse (response : YoutubeVideoResponse) = 
    let snippet = response.Snippet
    let statistics = response.Statistics
    let details = response.ContentDetails
    { Title = snippet.Title
      VideoID = response.Id
      ThumbnailLink = snippet.Thumbnails.Default.Url
      Description = snippet.Description
      Uploader = snippet.ChannelTitle
      Views = statistics.ViewCount
      Likes = statistics.LikeCount
      Favorited = statistics.FavoriteCount
      ISODuration = details.Duration }

//Grabs info from a single video from a videoid. Mostly for testing purposes
let videorequest apikey videoid = 
    try 
        Some(YoutubeVideo.Load(urlVideofromID apikey videoid).Items.[0] |> filtervideoresponse)
    with
    | :? IndexOutOfRangeException -> 
        do printfn "Error grabbing video with id %s - No video found" videoid
        None
    | ex -> 
        do printfn "Error grabbing video with id %s - Message %s" videoid ex.Message
        None

//As of 5/18/2014 it seems you can only request 50 videos at a time.
//partitionsize is if the limit is increased
let processplaylist apikey partitionsize playlistid =
    playlistvideos (urlPlaylistVideosfromID apikey playlistid) |> 
    Option.map (
        //Group into batches by the number specified by windowsize
        Seq.mapi(fun i videoid -> i,videoid) >>
        Seq.groupBy(fun (i,_) -> i % partitionsize) >>
        Seq.map(fun (_,group) -> group |> Seq.map(fun (_,truncated) -> truncated)) >>
        //TODO: Make Async
        Seq.map (
            fun truncatedids -> 
            YoutubeVideo.Load(urlVideofromIDs apikey truncatedids).Items |>
            Seq.map(filtervideoresponse)) >>
        Seq.collect id)
