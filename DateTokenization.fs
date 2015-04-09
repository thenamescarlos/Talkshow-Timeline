module RFTimeline.DateTokenization

open System
open RFTimeline.DateParseToken
open System.Text.RegularExpressions

//Regex Helpers
let regex pattern input = Regex.Match(input,pattern)
let regexallmatches pattern input = Regex.Matches(pattern,input) |> Seq.cast |> Seq.map(fun (m:Match) -> m) 
let matchedgroup (group: GroupCollection) (index:int) =  group.[index].Value //remember 0 shows the whole value
let withregexmatch (regex : string -> Match) onmatch = regex >> function m when m.Success -> onmatch m | _ -> None
let withregexmatchgroups regex f = withregexmatch regex (fun matchresult -> f (matchresult.Groups))

//Tokenization
let month = "(1[0-2]|0{0,1}[1-9])" //Should only match range of [0]1-9 or 10-12
let seperator = "([\s-/])"
let rangeseperator = "\s*\({0,1}(-)\s*\){0,1}"
let day = "(3[0-2]|[12][0-9]|0{0,1}[1-9])" //Should only match range [0]1-9 or 10-32
let year = "(\d{2}|\d{4})" //don't care what they put for year, it must be valid if everythign else follows

let regexmd = regex (sprintf "^%s%s%s$" month seperator day)
let regexmdy = regex (sprintf "^%s%s%s%s%s$" month seperator day seperator year)
let regexymd = regex (sprintf "^%s%s%s%s%s$" year seperator month seperator day)
let regexsep = regex rangeseperator
let mdytoken = 
    withregexmatchgroups regexmdy (
        matchedgroup >> fun g ->
            try
                Some ( Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 1) (g 3) (g 5))))
            with //Should only fail on stuff like 2-31-14
            | ex -> None
    )
let ymdtoken = 
    withregexmatchgroups regexymd (
        matchedgroup >> fun g ->
            try
                Some ( Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 3) (g 5) (g 1))))
            with //Should only fail on stuff like 2-31-14
            | ex -> None
    )
let mdtoken = withregexmatchgroups regexmd (matchedgroup >> fun g -> Some(Truncated({Month = int(g 1); Day = int(g 3)})) )
let septoken = regexsep >> fun matchof -> if matchof.Success then Some RangeSeperator else None //Fix high order functions

//Sanitize
let split (text:string) = text.Split()
let monthname = 
    String.map(Char.ToLower) >> function //Easy to parse int
    | "january" | "jan" -> "1" 
    | "february" | "febuary"| "feb" -> "2"
    | "march" | "mar" -> "3"
    | "april" | "apr" -> "4"
    | "may" | "may" -> "5"
    | "june" | "jun" -> "6"
    | "july" | "jul" -> "7"
    | "august" | "aug" -> "8"
    | "september" | "sep" | "sept" -> "9"
    | "october" | "oct" -> "10"
    | "november" | "nov" -> "11"
    | "december" | "dec" -> "12"
    | "through" | "to" -> "-"
    | etc -> etc
let removepunctuation input = Regex.Replace(input,"[;,.\(\)]","")
let sanitize = removepunctuation >> split >> Seq.map(monthname) >> String.concat " "

let tokenizeuploadtext (uploadtext:string) =
    let rec matchof tokenizers input = 
        match tokenizers with
        | [] -> Invalid 
        | tokenizer::others -> 
            match tokenizer input with 
            | Some  token -> token
            | None -> matchof others input
    uploadtext //TODO Temp - Should probably remove all punctuation characters or use Matches
    |> sanitize
    |> split
    |> Seq.map(matchof [mdytoken;ymdtoken;mdtoken;septoken])

let scrapedates = tokenizeuploadtext >> parse >> expanddatelist
