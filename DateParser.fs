module Youtube.DateParser
open System
open System.Text.RegularExpressions

type DateDM = {Month:int;Day:int}
type DateDMY = {Month:int;Day:int;Year:int}
type DateParseToken =
    | Partial of DateDM
    | Full of DateDMY
 
//If a partial date like 3-22 is followed by a date like 4-02-2015 then it is implied to be the same year.
//If a partial date like 3-22 is followed by a date like 2-04-2015 then is is implied to be the year before
let inferyearof fulldate (partialdate: DateDM) =
    let year = if partialdate.Month > fulldate.Month then fulldate.Year - 1 else fulldate.Year
    {Month=partialdate.Month;Day=partialdate.Day;Year=year}
 
let inferyearsof fulldate = List.map(inferyearof fulldate)

//Regex and Tokenizing
let month = "(1[0-2]|0{0,1}[1-9])" //Should only match range of [0]1-9 or 10-12
let seperator = "([-/])" //TODO group
let day = "(3[0-2]|[12][0-9]|0{0,1}[1-9])" //Should only match range [0]1-9 or 10-32
let year = "(\d{2}|\d{4})" //don't care what they put for year, it must be valid if everythign else follows

let regex pattern input = Regex.Match(input,pattern)
let regexmd = regex (sprintf "^\({0,1}%s%s%s\){0,1},{0,1}$" month seperator day)
let regexmdy = regex (sprintf "^\({0,1}%s%s%s%s%s\){0,1},{0,1}$" month seperator day seperator year)
let matchmd date = 
    let matchresult = regexmd date
    match matchresult.Success with
    | true -> 
        let month = (int) matchresult.Groups.[1].Value
        let day = (int) matchresult.Groups.[3].Value
        Some (Partial({Month=month;Day=day}))
    | false -> None
let matchmdy date = 
    let matchresult = regexmdy date
    match matchresult.Success with
    | true -> 
        let month = (int) matchresult.Groups.[1].Value
        let day = (int) matchresult.Groups.[3].Value
        let year = (int) matchresult.Groups.[5].Value
        Some (Full({Month=month;Day=day;Year=year}))
    | false -> None


let tokenizeuploadtext (uploadtext:string) =
    let matchof input = match matchmdy input with Some(_) as mdy -> mdy | None -> match matchmd input with Some(_) as md -> md | None -> None
    uploadtext.Replace(".","").Replace(";","").Split() //TODO Temp - Should probably remove all punctuation characters or use Matches
    |> Seq.map(matchof) 
    |> Seq.fold(fun tokens result -> match result with None -> tokens | Some(token) -> token :: tokens) []
    |> List.rev

let inferfulldates inferfulldates datetokens =
    let rec f partialdates fulldates tokens =
        match tokens with
        | [] -> fulldates
        | h::t ->
            match h with
            | Partial(partialdate) -> f (partialdate :: partialdates) fulldates t
            | Full(fulldate)-> f [] (fulldates @ (List.rev (inferfulldates fulldate partialdates)) @ [fulldate]) t
    f [] [] datetokens

let rfshowdates = tokenizeuploadtext >> inferfulldates inferyearsof
let a =
    "The Buddays record two weeks worth of live-to-tape segments on the eve of SXM's Christmas party; imbibed producer Chris Stanley rises to the occasion. LINK: http://youtu.be/7u4yFbYGZTU 12/22, 12/23, 12/24, 12/25, 12/26, 12/29, 12/30; 01/05/15."
let b =
    """Grandma Fez introduces the Buddays to his dissociated alter-ego "Inner Voice", after suffering an escalator fall; Ronnie B. calls shenanigans on his co-host's nuanced performance; Pepper's liquor stash gets looted. 12/15, 12/16, 12/17/10"""
let c =
    """Back in early 2007, a masked producer showed up to replace East Side Dave. This is that story (well, part 1 of 4 since there’s an 11 hour limit on YouTube uploads) Click 'show more' for the chapter list and timecodes. 

Chapter 1-8

00:00:00 - 01-12 - Dave Gets Voted Off
01:51:04 - 01-16 - Dave Steps Down
02:43:55 - 01-17-2007 - A Masked Man Appears
04:17:23 - 01-18-2007 - Pre Hypnotist Show
04:53:25 - 01-19-2007 - Hypnotist Show
07:25:21 - 01-22-2007 - Post Hypnotist Show/Earl Can’t Sleep
09:01:31 - 01-23-2007 - Purity Balls
09:28:53 - 01-24-2007 - Earl vs Rider"""