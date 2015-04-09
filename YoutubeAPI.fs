module RFTimeline.YoutubeAPI

open System
open FSharp.Data
         
type YoutubePlaylist = JsonProvider<"./YoutubeAPI/PlaylistItems.json">

type Channel = {ChannelID :string ; UploadedID : string}
type VideoInfo =  YoutubePlaylist.Item

let apikey = "AIzaSyDVR-g1L3iD1R7MEPhlPa1i8DsnmPP6Wdc"
let baseurl = "https://www.googleapis.com/youtube/v3/"
let paginateplaylist playlistid  : VideoInfo seq =
    let videorequest playlistid = sprintf "%splaylistItems?part=snippet&playlistId=%s&maxResults=50&key=%s" baseurl playlistid apikey
    let nextplaylistpage token = videorequest playlistid |> fun result -> sprintf "%s&pageToken=%s" result token
    let firstpage = (YoutubePlaylist.Load(videorequest playlistid))
    let rec paginate videos (result : YoutubePlaylist.Root) =
        match result.NextPageToken with
        | token when String.IsNullOrWhiteSpace(token) -> 
            Seq.append result.Items videos
        | token ->
            paginate (Seq.append result.Items videos) (YoutubePlaylist.Load(nextplaylistpage token)) 
    Seq.append (firstpage.Items) (paginate [] firstpage)

let discodog = {ChannelID="UCFKzRzwT28NDxFfxuq1vEmQ"; UploadedID="UUFKzRzwT28NDxFfxuq1vEmQ"}
let ronandfriendz = {ChannelID="UC_JW3s3GEoNIc9bBUnrU8iw";UploadedID="UU_JW3s3GEoNIc9bBUnrU8iw"}
let heybuddays = {ChannelID="UCCiCH4bjwNnxKTy0KmdpXBQ"; UploadedID="UUCiCH4bjwNnxKTy0KmdpXBQ"}
let thebobwhookidsamshow = {ChannelID="UCYR-07tWRxv9pToHvBJ3JoA";UploadedID="UUYR-07tWRxv9pToHvBJ3JoA"}
let kirkangel = {ChannelID="UCDcDSFLXLeC4zwxfsndzLcw"; UploadedID="UUDcDSFLXLeC4zwxfsndzLcw"}
let frrrunkis = {ChannelID="UCUXBl9Kar4ty2WWR-EVy9OQ"; UploadedID="UUUXBl9Kar4ty2WWR-EVy9OQ"}
let ronscigarbjj = {ChannelID="UCwTMLRqNN1wOMR8A8gyaLvA"; UploadedID="UUwTMLRqNN1wOMR8A8gyaLvA"}

let knownchannels = 
    [
        discodog
        ronandfriendz
        heybuddays
        thebobwhookidsamshow
        kirkangel
        frrrunkis
        ronscigarbjj
    ]

let getuploadedvideos channel = 
    channel.UploadedID 
    |> paginateplaylist

let getalluploadedvideos channels = 
    channels |> Seq.map(getuploadedvideos) |> Seq.collect(id)

let videosfromall = Seq.cache (getalluploadedvideos knownchannels)
