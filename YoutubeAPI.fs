module Youtube.API
open System
open FSharp.Data

let apikey = "AIzaSyDVR-g1L3iD1R7MEPhlPa1i8DsnmPP6Wdc"
let baseurl = "https://www.googleapis.com/youtube/v3/"
let videorequest playlistid = sprintf "%splaylistItems?part=snippet&playlistId=%s&maxResults=50&key=%s" baseurl playlistid apikey

type YoutubePlaylist = JsonProvider<"./YoutubeAPI/PlaylistItems.json">

type Channel = {ID :string ; UploadedID : string}
type VideoInfo =  {Info : YoutubePlaylist.Item}


let discodog = {ID="UCFKzRzwT28NDxFfxuq1vEmQ"; UploadedID="UUFKzRzwT28NDxFfxuq1vEmQ"}
let ronandfriendz = {ID="UC_JW3s3GEoNIc9bBUnrU8iw";UploadedID="UU_JW3s3GEoNIc9bBUnrU8iw"}

let knownchannels = 
    [
        discodog
        ronandfriendz
    ]

let getuploadedvideos channel = 
    channel.UploadedID 
    |> videorequest
    |> YoutubePlaylist.Load
    |> fun json -> json.Items
    |> Seq.map(fun vi -> {Info=vi})

let getalluploadedvideos channels = 
    channels |> Seq.map(getuploadedvideos) |> Seq.collect(id)
