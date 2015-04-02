module Youtube.Formatting

open System
open Youtube.API
open Youtube.DateParser


type RFClipDate =
    | Day of DateTime
    | DateRange of DateTime * DateTime

type RFShowLink = {Title: string; Uploader: string; Airdate : string; Link:string}

let daterange uploadtext = 
    let todatetime dmy = DateTime.Parse(sprintf "%d-%d-%d" dmy.Month dmy.Day dmy.Year)
    let dates = rfshowdates uploadtext
    match List.length dates with
    | 0 -> None
    | 1 -> Some( Day (todatetime (List.head dates) ))
    | l -> Some( DateRange(todatetime (List.head dates) , todatetime(Seq.last dates)))

let torfshowlink videoinfo =
    let goodinfo = videoinfo.Info.Snippet
    let title = goodinfo.Title
    let uploader = goodinfo.ChannelTitle
    let airdate =
        match goodinfo.Description |> daterange with
        | None -> "?"
        | Some date -> 
            match date with
            | Day day -> day.ToShortDateString()
            | DateRange(startof,endof) -> sprintf "%s to %s" (startof.ToShortDateString()) (endof.ToShortDateString())
    let link = ""
    {Title=title;Uploader=uploader;Airdate=airdate;Link=link}

//General Format Helpers 
let tocolumns link = [link.Title ; link.Uploader ; link.Airdate ; link.Link]

let toreddittab videos =
    let torow = String.concat "    "
    videos |> Seq.map(tocolumns >> torow)

let totabdelimited videos =
    let torow = String.concat "\t"
    videos |> Seq.map(tocolumns >> torow)

let writefile contents = IO.File.WriteAllText("/Users/sbhsadmin/Desktop/Result.txt",contents)

let scrapeyoutube showlinks =
    showlinks 
    |> totabdelimited 
    |> String.concat (Environment.NewLine + Environment.NewLine) 
    |> writefile

let program() =
    getalluploadedvideos knownchannels 
    |> Seq.map(torfshowlink)
    |> scrapeyoutube