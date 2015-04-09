module RFTimeline.Bot

open System
open System.Text.RegularExpressions
open RFTimeline.YoutubeAPI
open RFTimeline.DateTokenization

type RFShowLink = {Title: string; Uploader: string; Airdate : DateTime list; Link:string}

let torfshowlink (videoinfo : VideoInfo) =
    let goodinfo = videoinfo.Snippet
    let title = goodinfo.Title 
    let uploader = goodinfo.ChannelTitle
    let airdatefromdescription = goodinfo.Description |> scrapedates
    let airdatefromtitle = goodinfo.Title |> scrapedates
    let link = ""

    let airdate = 
        match airdatefromdescription, airdatefromtitle with
        | [] , titledates -> titledates
        | descriptiondates, _ -> descriptiondates

    {Title=title;Uploader=uploader;Airdate=airdate;Link=link}

let trimtitle rfshowlink = 
    rfshowlink.Title 
    |> fun title ->  Regex.Replace(title,"^(Classic){0,1}\s{0,1}Ron (&|and) Fez:{0,1}\s{0,1}-{0,1}\s+","")
    |> fun newtitle -> {rfshowlink with Title=newtitle}

//General Format Helpers 
let tocolumns link = 
    let airdates = 
        match link.Airdate with
        | [] -> "?"
        | some -> some |> List.map(fun date -> date.ToShortDateString()) |> String.concat ", "
    [link.Title ; link.Uploader ; (airdates) ; link.Link]

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
    |> String.concat (Environment.NewLine) 
    |> writefile

let program() =
    videosfromall
    |> Seq.map(torfshowlink >> trimtitle)
    |> scrapeyoutube
