module Youtube.DateParser
open System
open System.Text.RegularExpressions

type ParsedMD = {Month:int;Day:int}

type Date =
    | Day of DateTime
    | DateRange of DateTime * DateTime 

type IncompleteDate = 
    | IncompleteDay of ParsedMD
    | IncompleteRange of ParsedMD * ParsedMD

type DateParseToken =
    | Truncated of ParsedMD
    | Complete of DateTime
    | RangeSeperator

type DateToken = //Subset
    | T of ParsedMD
    | C of DateTime

type DateExpansionState = {PreviousDate : DateToken option; IsRangeEnd : bool; Incompletes : IncompleteDate list; Dates : Date list }

let t m d = Truncated({Month=m;Day=d})
let c m d y = Complete(DateTime(y,m,d)) 

let inferyearof (date : DateTime) truncated =
    let applyyear incomplete = 
        let year = if incomplete.Month > date.Month then date.Year - 1 else date.Year
        DateTime(year,incomplete.Month,incomplete.Day)
    match truncated with
    | IncompleteDay incomplete -> Day(applyyear incomplete)
    | IncompleteRange(start , endof) -> DateRange(applyyear start, applyyear endof)

//TODO: Delete redundant like 3-14-2014 , 3-14-2014 - 3-18-2014
let expand infer datetokens =
    let fold (state : DateExpansionState) token =
       match token with
       | Truncated next -> 
            {state with 
                PreviousDate = Some(T next)
                IsRangeEnd = false
                Incompletes = 
                    match state.IsRangeEnd with
                    | false -> IncompleteDay(next) :: state.Incompletes
                    | true -> 
                        match state.PreviousDate with
                        | None -> IncompleteDay(next) :: state.Incompletes
                        | Some p ->
                            match p with
                            | T previous -> IncompleteRange(previous,next) :: state.Incompletes
                            | C c ->  IncompleteDay(next) :: state.Incompletes //Ignore m-d-y - m-d
            }
       | Complete next -> 
            {state with 
                PreviousDate = Some(C next )
                IsRangeEnd = false
                Incompletes = []
                Dates =
                    let parsedincompletes = List.rev (List.map (infer next) state.Incompletes)
                    let previousparsed = state.Dates 
                    let last = 
                        match state.IsRangeEnd with
                        | false -> [Day next]
                        | true -> 
                            match state.PreviousDate with
                            | None -> [Day next]
                            | Some previous -> 
                                match previous with
                                | T t -> 
                                    match infer next (IncompleteDay t) with
                                    | Day startdate -> [DateRange(startdate,next)]
                                    | DateRange _ -> [Day next] //shouldn't happen
                                | C startdate -> [DateRange(startdate,next)] 
                    previousparsed @ parsedincompletes @ last
            }
       | RangeSeperator ->
           match state.PreviousDate with
           | None -> state
           | Some p -> {state with IsRangeEnd = true}
    Seq.fold fold {PreviousDate = None; IsRangeEnd = false; Incompletes = []; Dates = []} datetokens

let test datetokens = 
    expand inferyearof datetokens 
    |> fun finishedstate -> finishedstate.Dates
    |> Seq.map(function  Day date -> date.ToShortDateString() | DateRange(date1,date2) -> sprintf "%s - %s" (date1.ToShortDateString()) (date2.ToShortDateString()))
    |> Seq.iter(printf "%s ")

//If a partial date like 3-22 is followed by a date like 4-02-2015 then it is implied to be the same year.
//If a partial date like 3-22 is followed by a date like 2-04-2015 then is is implied to be the year before
//Regex Helpers
let regex pattern input = Regex.Match(input,pattern)
let regexallmatches pattern input = Regex.Matches(pattern,input) |> Seq.cast |> Seq.map(fun (m:Match) -> m) 
let matchedgroup (group: GroupCollection) (index:int) =  group.[index].Value //remember 0 shows the whole value
let withregexmatch (regex : string -> Match) onmatch = regex >> function m when m.Success -> Some(onmatch m) | _ -> None
let withregexmatchgroups regex f = withregexmatch regex (fun matches -> f (matches.Groups))
//Tokenization
let split (text:string) = text.Split()
let easytoken = 
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

let month = "(1[0-2]|0{0,1}[1-9])" //Should only match range of [0]1-9 or 10-12
let seperator = "([-/])" //TODO group
let day = "(3[0-2]|[12][0-9]|0{0,1}[1-9])" //Should only match range [0]1-9 or 10-32
let year = "(\d{2}|\d{4})" //don't care what they put for year, it must be valid if everythign else follows
let regexmd = regex (sprintf "^\({0,1}%s%s%s\){0,1},{0,1}$" month seperator day)
let regexmdy = regex (sprintf "^\({0,1}%s%s%s%s%s\){0,1},{0,1}$" month seperator day seperator year)
let mdytoken = withregexmatchgroups regexmd (matchedgroup >> fun g -> Full {Month = int(g 1); Day = int(g 3); Year= int(g 5)})
let mdtoken = withregexmatchgroups regexmdy (matchedgroup >> fun g -> Partial {Month = int(g 1); Day = int(g 3)})
let tokenizeuploadtext (uploadtext:string) =
    let rec matchof tokenizers input = 
        match tokenizers with
        | [] -> None 
        | tokenizer::others -> 
            match tokenizer input with 
            | Some(_) as token -> token
            | None -> matchof others input
    uploadtext.Replace(".","").Replace(";","").Split() //TODO Temp - Should probably remove all punctuation characters or use Matches
    |> Seq.map(matchof [mdytoken;mdtoken]) 
    |> Seq.fold(fun tokens result -> match result with None -> tokens | Some(token) -> token :: tokens) []
    |> List.rev
