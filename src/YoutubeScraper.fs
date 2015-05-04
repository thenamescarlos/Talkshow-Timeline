module RFTimeline.YoutubeScraper

open System
open FSharp.Data

//Seems that you cannot get all the details of a video without multiple requests hence the following.
type FilteredVideoRequest = 
    { ISODuration : string
      Views : int
      Likes : int
      Favorited : int }

type FilteredPlaylistItemRequest = 
    { Title : string
      Uploader : string
      Description : string
      ThumbnailLink : string
      VideoID : string } //Video id can be used to construct link

type VideoInformation = 
    { FromPlaylistItem : FilteredPlaylistItemRequest
      FromVideo : FilteredVideoRequest }

//TODO: Fix -> In interactive replace . with .. in interactive      
type YoutubePlaylistItems = JsonProvider< "./YoutubeAPI/PlaylistItems.json" >
type YoutubeVideo = JsonProvider< "./YoutubeAPI/Video.json" >

//Abbreviations for pretty type inference
type PlaylistResponse = YoutubePlaylistItems.Root

type PlaylistResponseInfo = YoutubePlaylistItems.Item

type VideoResponse = YoutubeVideo.Root

type VideoResponseInfo = YoutubeVideo.Item

//Functions for constructing a url
let urlrequest apikey content filter = 
    filter >> sprintf "%s%s&key=%s&%s" "https://www.googleapis.com/youtube/v3/" content apikey
let urlVideo apikey filter = urlrequest apikey "videos?part=statistics,contentDetails" filter
let urlPlaylistVideos apikey filter = urlrequest apikey "playlistItems?part=snippet&maxResults=50" filter
let urlPlaylistVideosfromID apikey playlistid = urlPlaylistVideos apikey (sprintf "playlistId=%s") playlistid
let urlVideofromID apikey videoid = urlVideo apikey (sprintf "id=%s") videoid

//Grabs info from a single video from a videoid
let videorequest apikey videoid = 
    try 
        Some(//the following request only grabs one video
             let response = YoutubeVideo.Load(urlVideofromID apikey videoid)
             let video = response.Items.[0]
             let statistics = video.Statistics
             let details = video.ContentDetails
             { Views = statistics.ViewCount
               Likes = statistics.LikeCount
               Favorited = statistics.FavoriteCount
               ISODuration = details.Duration })
    with 
    | :? IndexOutOfRangeException ->
        do printfn "Error grabbing video with id %s - No video found" videoid
        None
    | ex -> 
        do printfn "Error grabbing video with id %s - Message %s" videoid ex.Message
        None

//TODO: Combine multiple videos into single request.
let playlistrequest (url : string) : PlaylistResponse option = 
    try 
        Some(YoutubePlaylistItems.Load(url))
    with ex -> //Output error on console
        do printfn "Error grabbing playlist with url %s - Message %s" url ex.Message
        None

let playlistvideos url : PlaylistResponseInfo array option = 
    let nextplaylisturl url token = sprintf "%s&pageToken=%s" url token

    let rec paginate videos (result : PlaylistResponse) = 
        match result.NextPageToken with
        | token when String.IsNullOrWhiteSpace(token) -> Array.append result.Items videos
        | token -> paginate (Array.append result.Items videos) (YoutubePlaylistItems.Load(nextplaylisturl url token))
    playlistrequest url //Expecting playlist request error to log
    |> Option.bind (fun playlist -> Array.append (playlist.Items) (paginate [||] playlist) |> Some)

let videoinfo apikey playlistid = 
    //On a valid playlist response - grabs any details it can
    //Then for each video - tries to grab other info or fails completely
    //TODO: Smarter request handling
    let processPlaylistItem (response : PlaylistResponseInfo) = 
        let detail = response.Snippet
        let videoid = response.Snippet.ResourceId.VideoId
        let pinfo = 
            { Title = detail.Title
              Uploader = detail.ChannelTitle
              VideoID = detail.ResourceId.VideoId
              Description = detail.Description
              ThumbnailLink = detail.Thumbnails.Default.Url }
        match videorequest apikey videoid with
        | Some vinfo -> 
            Some {FromPlaylistItem = pinfo; FromVideo = vinfo}
        | None ->
            do printfn "Could not grab information from video %s" videoid
            None
    
    playlistvideos (urlPlaylistVideosfromID apikey playlistid)
    |> Option.bind( fun playlistitems ->
        let processed = 
            Seq.map processPlaylistItem playlistitems
            |> Seq.takeWhile (function Some _ -> true | None -> false) 
            |> Seq.fold( fun acc item -> match item with Some videoinfo -> videoinfo :: acc | None -> acc) []
        let lengthcorrectlyprocessed = List.length processed
        match playlistitems.Length , lengthcorrectlyprocessed with
        | actual, computed when actual = computed -> Some processed
        | _, _ ->
            do printfn "Process aborted - Some videos didn't download properly. Probably bad internet or sumthin'"
            None
    )
