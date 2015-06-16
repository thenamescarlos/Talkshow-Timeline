//Lots of quick and dirty, but not hard to clean up code.

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
      SanitizedDescription: string
      BaseInfo : FilteredVideoRequest }

//TODO: Seperate Request when link in description.

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
        | Some h, Some m, Some s -> (h * 60 * 60) + (m * 60) + s
        | None, Some m, Some s -> (m * 60) + s  
        | Some h , None , None -> h * 60 * 60
        | Some h , Some m , None -> (h * 60 * 60) + (m * 60)
        | None, Some m, None -> m * 60
        | Some h, None, Some s -> (h * 60 * 60)
        | None, None, Some s -> s
        | _ -> 0 //Shouldn't happen
    let sanitizeddescription =
        let description = videoinfo.Description 
        Regex.Replace(description,"\r{0,1}\n","<br>") //hack way to eliminate newlines
        |> fun linesremoved -> Regex.Replace(linesremoved,"\t","<t>") //remove tabs just in case
    
    //Essentially being used for disco dog's channel
    let haslinkindescription title = 
        Regex.IsMatch(title,"link in description",RegexOptions.IgnoreCase)
         
    // Used to see if a link is in the video.
    let extractlink = 
        withregexmatch (regex "http(s){0,1}:\/\/youtu\.be\/\S+") ( fun match' ->
            Some match'.Value )

    { Airdate=airdate
      SanitizedDescription = sanitizeddescription
      Link =
        match haslinkindescription videoinfo.Title with
        | true ->
            match extractlink videoinfo.Description with
            | Some extracted -> extracted
            | None -> link
        | false -> link        
      DurationSeconds = duration videoinfo.ISODuration       
      BaseInfo = videoinfo }

//For Output
let trimtitle title = 
    Regex.Replace(title,"^(Classic){0,1}\s{0,1}Ron (&|and) Fez:{0,1}\s{0,1}-{0,1}\s+","")

let timetext seconds = 
    match seconds with
    | _ when seconds >= 3600 ->
        let hours = seconds / 3600
        let minuteremainder = (seconds - (hours * 3600)) / 60
        sprintf "%dh:%dm" hours minuteremainder
    | _ when seconds > 60 ->
        let minutes = seconds / 60
        sprintf "%dm:%d" minutes (seconds % 60)
    | seconds -> 
        sprintf "%ds" seconds
          
let airdates (sorteddatelist : DateTime list) = 
    match sorteddatelist with
    | [] -> "?"
    | some -> some |> List.map(fun date -> date.ToShortDateString()) |> String.concat ", "


//General Format Helpers 
//TODO: Better structure, includes descriptoins
let header = ["Channel";"Title";"Length";"Likes";"Views";"Favorited";"Link";"Thumbnail";"Airdates";"Description"]
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
        entry.SanitizedDescription
    ]

let toreddittab videos =
    let torow = String.concat "    "
    videos |> Seq.map(tocolumns >> torow) |> Seq.append header

let totabdelimited videos =
    let torow = String.concat "\t"
    videos |> Seq.map(tocolumns >> torow) |> Seq.append (Seq.singleton (header |> torow))

let writefile destination contents = IO.File.WriteAllText(destination,contents)

//remove duplicates like s1nber playlist
let scrapeyoutube destination (appconfig : AppConfig) =
    let playlistids = appconfig.PlaylistIDs
    let apikey = appconfig.APIKey

    playlistids
    |> Seq.map(processplaylist apikey 50)
    |> Seq.choose id
    |> Seq.collect id //Make sure that application closes on invalid id
    |> Seq.map totimelineentry
    |> totabdelimited 
    |> String.concat (Environment.NewLine) 
    |> writefile destination

let application outputdestination =
    //Expects config file to be in the parent folder.
    let projectroot = IO.Directory.GetParent(__SOURCE_DIRECTORY__).FullName
    let configpath = IO.Path.Combine(projectroot,"config.json")
    openconfig configpath
    |> Option.map(fun appconfig -> scrapeyoutube outputdestination appconfig)
