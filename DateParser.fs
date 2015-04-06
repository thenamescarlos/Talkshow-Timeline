module Youtube.DateParser

open System
open System.Text.RegularExpressions

type Date = Day of DateTime | DateRange of DateTime * DateTime 

//For dates parsed as 3-22 or 03/22 etc.
type TruncatedDate = {Month:int;Day:int}

//Holds incomplete dates because e.g. a range of 2/27 - 3/1 is impossible to get inbetween dates on account of leap years.
type IncompleteDate = IncompleteDay of TruncatedDate | IncompleteRange of TruncatedDate * TruncatedDate

//Possible tokens that can be regexed out to parse. e.g. 3-13 - 3-17-2015 -> list<Truncated;RangeSeperator;Complete>
type DateParseToken = Truncated of TruncatedDate | Complete of DateTime | RangeSeperator

//Subset of DateParseToken that might be able to be refactored out
type DateToken = T of TruncatedDate | C of DateTime

type DateExpansionState = {PreviousDate : DateToken option; IsRangeEnd : bool; Incompletes : IncompleteDate list; Dates : Date list }

let inferyearof (date : DateTime) truncated =
    let applyyear incomplete = 
        let year = if incomplete.Month > date.Month then date.Year - 1 else date.Year
        DateTime(year,incomplete.Month,incomplete.Day)
    match truncated with
    | IncompleteDay incomplete -> Day(applyyear incomplete)
    | IncompleteRange(start , endof) -> DateRange(applyyear start, applyyear endof)

let todatetimes parseresult =
    let datesbetween start endof =
        let rec generate dates (current : DateTime) =
            match current = endof , current.AddDays(1.0) = endof with
            | true , _ -> dates
            | false, true -> dates
            | _, _ -> current.AddDays(1.0) |> fun newdate ->  generate (newdate :: dates) newdate
        if start >= endof then [] else generate [] start |> List.rev     
    parseresult.Dates 
    |> List.collect(function | Day date -> [date] | DateRange(start,endof) -> [start] @ (datesbetween start endof) @ [endof])
    |> Set.ofList
    |> Set.toList

//Optional TODO: Delete redundant like 3-14-2014 , 3-14-2014 - 3-18-2014 in one pass
let tofulldates infer datetokens =
    let fold (state : DateExpansionState) token =
       let nextstate datetoken = {state with IsRangeEnd = false; PreviousDate = Some(datetoken)}
       match token with
       | Truncated current -> 
            { nextstate (T current) with 
                Incompletes = 
                    match state.IsRangeEnd with
                    | false -> IncompleteDay(current) :: state.Incompletes
                    | true -> 
                        match state.PreviousDate with
                        | None -> IncompleteDay(current) :: state.Incompletes
                        | Some p ->
                            match p with
                            | T previous -> IncompleteRange(previous,current) :: state.Incompletes
                            | C c ->  IncompleteDay(current) :: state.Incompletes //Ignore m-d-y - m-d
            }
       | Complete current -> 
            {nextstate (C current) with 
                Incompletes = []
                Dates =
                    let parsedincompletes = List.rev (List.map (infer current) state.Incompletes)
                    let previousparsed = state.Dates 
                    let last = 
                        match state.IsRangeEnd with
                        | false -> [Day current]
                        | true -> 
                            match state.PreviousDate with
                            | None -> [Day current]
                            | Some previous -> 
                                match previous with
                                | T t -> 
                                    match infer current (IncompleteDay t) with
                                    | Day startdate -> [DateRange(startdate,current)]
                                    | DateRange _ -> [Day current] //shouldn't happen
                                | C startdate -> [DateRange(startdate,current)] 
                    previousparsed @ parsedincompletes @ last
            }
       | RangeSeperator ->
           match state.PreviousDate with
           | None -> state
           | Some p -> {state with IsRangeEnd = true}
    Seq.fold fold {PreviousDate = None; IsRangeEnd = false; Incompletes = []; Dates = []} datetokens

let t m d = Truncated({Month=m;Day=d})
let c m d y = Complete(DateTime(y,m,d)) 

let test datetokens = 
    tofulldates inferyearof datetokens 
    |> todatetimes
    |> Seq.iter(printfn "%A")

//Regex Helpers
let regex pattern input = Regex.Match(input,pattern)
let regexallmatches pattern input = Regex.Matches(pattern,input) |> Seq.cast |> Seq.map(fun (m:Match) -> m) 
let matchedgroup (group: GroupCollection) (index:int) =  group.[index].Value //remember 0 shows the whole value
let withregexmatch (regex : string -> Match) onmatch = regex >> function m when m.Success -> Some(onmatch m) | _ -> None
let withregexmatchgroups regex f = withregexmatch regex (fun matchresult -> f (matchresult.Groups))

//Tokenization
let month = "(1[0-2]|0{0,1}[1-9])" //Should only match range of [0]1-9 or 10-12
let seperator = "([-/])"
let rangeseperator = "\s*\({0,1}(-)\s*\){0,1}"
let day = "(3[0-2]|[12][0-9]|0{0,1}[1-9])" //Should only match range [0]1-9 or 10-32
let year = "(\d{2}|\d{4})" //don't care what they put for year, it must be valid if everythign else follows

let regexmd = regex (sprintf "^\({0,1}%s%s%s\){0,1},{0,1}$" month seperator day)
let regexmdy = regex (sprintf "^\({0,1}%s%s%s%s%s\){0,1},{0,1}$" month seperator day seperator year)
let regexsep = regex rangeseperator
let mdytoken = 
    withregexmatchgroups regexmdy (
        matchedgroup >> fun g -> //Fix throws exception on edge cases like 2/31/_
            Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 1) (g 3) (g 5))) 
    )
let mdtoken = withregexmatchgroups regexmd (matchedgroup >> fun g -> Truncated({Month = int(g 1); Day = int(g 3)}) )
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
        | [] -> None 
        | tokenizer::others -> 
            match tokenizer input with 
            | Some(_) as token -> token
            | None -> matchof others input
    uploadtext //TODO Temp - Should probably remove all punctuation characters or use Matches
    |> sanitize
    |> split
    |> Seq.map(matchof [mdytoken;mdtoken;septoken]) 
    |> Seq.fold(fun tokens result -> match result with None -> tokens | Some(token) -> token :: tokens) []
    |> List.rev
