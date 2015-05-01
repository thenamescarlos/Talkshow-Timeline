module RFTimeline.Bot

open System
open System.Text.RegularExpressions
open RFTimeline.YoutubeScraper
open RFTimeline.DateParser

type RFTimelineEntry = 
    {
        Airdate : DateTime list;
        Link: string //Parsed link
        DurationSeconds: int //Parsed ISO time
        BaseInfo : VideoInformation;
    }

let totimelineentry videoinfo =
    let link = sprintf "https://www.youtube.com/watch?v=%s" videoinfo.FromPlaylistItem.Description
    let airdate = 
        let airdatefromtitle = scrapedates (videoinfo.FromPlaylistItem.Title)
        let airdatefromdescription = scrapedates (videoinfo.FromPlaylistItem.Description)
        match airdatefromdescription, airdatefromtitle with
        | [] , titledates -> titledates
        | descriptiondates, _ -> descriptiondates
    let duration isotext = //only parsing hours for now
        let numericvalue timeid = //pattern needs to have atleast one group and it parses into a number
            withregexmatchgroups (regex (sprintf "(\d+)%s" timeid)  ) (fun group -> 
                try 
                    (group.Item 1).Value |> int |> Some //index 1 is the matchresult
                with
                    | ex -> None
            )
        let seconds = numericvalue "S" isotext
        let minutes = numericvalue "M" isotext
        let hours = numericvalue "H" isotext
        match hours, minutes, seconds with
        | Some h, Some m, Some s -> (h * 60 * 60) + (m * 60) + 2
        | None, Some m, Some s -> (m * 60) + s  
        | None, None, Some s -> s
        | _ -> 0 //Shouldn't happen

    { 
        Airdate=airdate
        Link=link
        DurationSeconds = duration videoinfo.FromVideo.ISODuration
        BaseInfo = videoinfo
    }

//For Output
let trimtitle title = 
    Regex.Replace(title,"^(Classic){0,1}\s{0,1}Ron (&|and) Fez:{0,1}\s{0,1}-{0,1}\s+","")

let timetext seconds = 
    match seconds / 60 with
    | minutes when minutes > 60 ->
        let hours = minutes / 60
        let minuteremainder = minutes - (hours * 60)
        sprintf "%dh:%dm" hours minuteremainder
    | minutes when minutes > 0 ->
        sprintf "%dm:%ds" minutes (seconds - (minutes * 60))
    | seconds -> 
        sprintf "%ds" seconds
          
let airdates (sorteddatelist : DateTime list) = 
    match sorteddatelist with
    | [] -> "?"
    | some -> some |> List.map(fun date -> date.ToShortDateString()) |> String.concat ", "


//General Format Helpers 
//TODO: Better structure
let header = ["Channel";"Title";"Length";"Likes";"Views";"Favorited";"Link";"Thumbnail";"Airdates";"Description"]
let tocolumns entry = 
    [ 
        entry.BaseInfo.FromPlaylistItem.Uploader
        trimtitle entry.BaseInfo.FromPlaylistItem.Title
        timetext entry.DurationSeconds
        entry.BaseInfo.FromVideo.Likes |> string
        entry.BaseInfo.FromVideo.Views |> string
        entry.BaseInfo.FromVideo.Favorited |> string
        entry.Link
        entry.BaseInfo.FromPlaylistItem.ThumbnailLink
        airdates entry.Airdate
       //entry.BaseInfo.FromPlaylistItem.Description
    ]

let toreddittab videos =
    let torow = String.concat "    "
    videos |> Seq.map(tocolumns >> torow) |> Seq.append (torow header |> Seq.singleton)

let totabdelimited videos =
    let torow = String.concat "\t"
    videos |> Seq.map(tocolumns >> torow) |> Seq.append (torow header |> Seq.singleton)

let writefile contents = IO.File.WriteAllText("/Users/sbhsadmin/Desktop/Result.txt",contents)

let scrapeyoutube playlistids =
    playlistids 
    |> Seq.choose (videoinfo "AIzaSyDVR-g1L3iD1R7MEPhlPa1i8DsnmPP6Wdc")
    |> Seq.collect(List.map totimelineentry)
    |> totabdelimited
    |> String.concat (Environment.NewLine)
    |> writefile

//demo
scrapeyoutube 
<| ["UUFKzRzwT28NDxFfxuq1vEmQ";
"UU_JW3s3GEoNIc9bBUnrU8iw";
"UUCiCH4bjwNnxKTy0KmdpXBQ";
"UUYR-07tWRxv9pToHvBJ3JoA";
"UUDcDSFLXLeC4zwxfsndzLcw";
"UUUXBl9Kar4ty2WWR-EVy9OQ";
"UUwTMLRqNN1wOMR8A8gyaLvA"]

