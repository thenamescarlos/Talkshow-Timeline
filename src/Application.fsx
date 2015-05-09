#load "./DateParser.fsx"
#load "./ConfigParser.fsx"
#load "./YoutubeScraper.fsx"

open System
open System.Text.RegularExpressions
open RFTimeline.YoutubeScraper
open RFTimeline.DateParser
open RFTimeline.ConfigParser

type RFTimelineEntry = 
    { Airdate : DateTime list; //Parsed dates from description or title
      Link: string //Parsed link
      DurationSeconds: int //Parsed ISO time
      BaseInfo : FilteredVideoRequest }

let totimelineentry videoinfo =
    let link = sprintf "https://www.youtube.com/watch?v=%s" videoinfo.VideoID
    let airdate = 
        let airdatefromtitle = scrapedates (videoinfo.Title)
        let airdatefromdescription = scrapedates (videoinfo.Description)
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

    { Airdate=airdate
      Link=link
      DurationSeconds = duration videoinfo.ISODuration       
      BaseInfo = videoinfo }

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
        sprintf "%dm:%d" minutes (seconds - (minutes * 60))
    | seconds -> 
        sprintf "%ds" seconds
          
let airdates (sorteddatelist : DateTime list) = 
    match sorteddatelist with
    | [] -> "?"
    | some -> some |> List.map(fun date -> date.ToShortDateString()) |> String.concat ", "


//General Format Helpers 
//TODO: Better structure, includes descriptoins
let header = ["Channel";"Title";"Length";"Likes";"Views";"Favorited";"Link";"Thumbnail";"Airdates"]
let tocolumns entry = 
    [ 
        entry.BaseInfo.Uploader
        trimtitle entry.BaseInfo.Title
        timetext entry.DurationSeconds
        entry.BaseInfo.Likes |> string
        entry.BaseInfo.Views |> string
        entry.BaseInfo.Favorited |> string
        entry.Link
        entry.BaseInfo.ThumbnailLink
        airdates entry.Airdate
        //entry.BaseInfo.FromPlaylistItem.Description
    ]

let toreddittab videos =
    let torow = String.concat "    "
    videos |> Seq.map(tocolumns >> torow) |> Seq.append header

let totabdelimited videos =
    let torow = String.concat "\t"
    videos |> Seq.map(tocolumns >> torow) |> Seq.append header

let writefile contents = IO.File.WriteAllText("/Users/sbhsadmin/Desktop/Result.txt",contents)

//remove duplicates like s1nber playlist
let scrapeyoutube apikey playlistids =
    playlistids |> 
    Seq.map(processplaylist apikey 50) |>
    Seq.choose id |> //Make sure that application closes on invalid id
    Seq.collect id |>
    Seq.map totimelineentry |>
    totabdelimited |>
    String.concat (Environment.NewLine) |>
    writefile

let application() =
    let projectroot = IO.Path.GetDirectoryName(__SOURCE_DIRECTORY__) 
    let configpath = IO.Path.Combine(projectroot,"config.txt")
    openconfig configpath
    |> Option.map(fun appconfig -> scrapeyoutube appconfig.APIKey appconfig.PlaylistIDs)
