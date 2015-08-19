// Algorithm that extracts a sorted set of dates from unstructured text.
// It works on

module RFTimeline.DateParser

open System
open System.Text.RegularExpressions

/// Represents an extracted full date or date range from a list of such items.
/// As a written list of dates is usually written with both single dates and
/// date ranges when convenient
/// The structure is meant to be an intermediate step before being expanded to a sorted set of dates.
type Date = 
    | Day of DateTime
    | DateRange of Start : DateTime * End : DateTime 

/// Represents partially written dates such 3-22
/// Holds incomplete dates because e.g. a range of 2/27 - 3/1 is impossible to get inbetween dates on account of leap years.
type TruncatedDate = { Month:int; Day:int }

type IncompleteDate = 
    | IncompleteDay of TruncatedDate 
    | IncompleteRange of TruncatedDate * TruncatedDate

//Possible tokens that can be regexed out to parse. e.g. 3-13 - 3-17-2015 -> list<Truncated;RangeSeperator;Complete>
type DateParseToken = Truncated of TruncatedDate | Complete of DateTime | RangeSeperator | Invalid

type DateExpansionState = 
    { Previous : DateParseToken
      IsRangeEnd : bool
      Incompletes : IncompleteDate list
      Dates : Date list }

//Optional TODO: Delete redundant like 3-14-2014 , 3-14-2014 - 3-18-2014 in one pass
let parse datetokens =
    let infer (date : DateTime) truncated =
        let applyyear incomplete = 
            let year = if incomplete.Month > date.Month then date.Year - 1 else date.Year
            DateTime(year,incomplete.Month,incomplete.Day)
        match truncated with
        | IncompleteDay incomplete -> Day(applyyear incomplete)
        | IncompleteRange(start , endof) -> DateRange(applyyear start, applyyear endof)
    let f (state : DateExpansionState) token =
       let nextstate datetoken = {state with IsRangeEnd = false; Previous = datetoken}
       match token with
       | Truncated current -> 
            { nextstate (Truncated current) with 
                Incompletes = 
                    match state.IsRangeEnd with
                    | false -> IncompleteDay(current) :: state.Incompletes
                    | true -> 
                        match state.Previous with
                        | Truncated previous -> IncompleteRange(previous,current) :: state.Incompletes
                        | _ ->  IncompleteDay(current) :: state.Incompletes //Ignore M-D-Y , M-D. Don't expand if previous was invalid.
            }
       | Complete current -> 
            {nextstate (Complete current) with 
                Incompletes = []
                Dates =
                    let parsedincompletes = List.rev (List.map (infer current) state.Incompletes)
                    let previousparsed = state.Dates 
                    let last = 
                        match state.IsRangeEnd with
                        | false -> Day current
                        | true -> 
                            match state.Previous with
                            | Truncated t -> 
                                match infer current (IncompleteDay t) with
                                | Day startdate -> DateRange(startdate,current)
                                | DateRange _ -> Day current //shouldn't happen
                            | Complete startdate -> DateRange(startdate,current)
                            | _ -> Day current //Again a mistake if range seperator. Invalid means wrong.
                    previousparsed @ parsedincompletes @ [last]
            }
       | RangeSeperator -> //Note range seperator doesn't assign it self as he previous
           match state.Previous with
           | Invalid -> state
           | _ -> {state with IsRangeEnd = true}
       | Invalid -> {state with Previous = Invalid}
    Seq.fold f {Previous = Invalid; IsRangeEnd = false; Incompletes = []; Dates = []} datetokens


let expanddatelist parseresult =
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

let parsetodates tokens = tokens |>  parse |> expanddatelist

// Start of Lexing Code

// Regex Helpers
let regex pattern input = Regex.Match(input,pattern)
let regexallmatches pattern input = Regex.Matches(input,pattern) |> Seq.cast<Match>
let matchedgroup (group: GroupCollection) (index:int) =  group.[index].Value // Note: The first index (0) is the whole match.
let withregexmatch regex f = regex >> function (match' : Match) when match'.Success -> f match' | _ -> None
let withregexmatch' pattern f = regex pattern >> function match' when match'.Success -> f match' | _ -> None
let withregexmatchgroups regex f = withregexmatch regex (fun match' -> f (match'.Groups))
let withregexmatchgroups' pattern f = withregexmatch' pattern (fun match' -> f (match'.Groups))

/// Defines the common date format combinations used around the world.
type DateFormat = 
    | MDY 
    | YMD 
    | DMY

/// Structure used to determine what date lexer to use for a particular piece of text.     
type LexerContext = LexerContext of Format : DateFormat * Seperator : string

// TODO: Allow formats such as 2003Nov09 that have no seperators.

/// Defines the seperators allowed between a date
let validseperators = "[\s\-\/\.]"

/// Generates a regex 
let regexdatefromformat format sep =
    let extract group daynum monthn
    match format with
    | MDY -> 
        withregexmatch' (sprintf "^(%s)%s(%s)$" month sep day)
    | YMD -> sep
    | DMY -> sep

/// Attempts to determine the date format from unstructured text.
/// Searches for the first string that matches a format from a list of formats.
let determinedateformat parsers text =
    let f datestring = List.tryPick ( fun parser -> parser datestring) parsers
    let datelikepattern = sprintf "\d+(?<sep>%s)\d+\k<sep>\d+" validseperators
    withregexmatch' datelikepattern ( fun match' ->
        let seperator = matchedgroup match'.Groups 1
        let lexercontext format = LexerContext(format,seperator)
        match f match'.Value with
        | Some dateformat -> Some (lexercontext dateformat)
        | None -> None ) text

let month = "(1[0-2]|0{0,1}[1-9])" //Should only match range of [0]1-9 or 10-12
let seperator = "([\s-/])"
let rangeseperator = "\s*\({0,1}(-)\s*\){0,1}"
let day = "(3[0-2]|[12][0-9]|0{0,1}[1-9])" //Should only match range [0]1-9 or 10-32
let year = "(\d{2}|\d{4})" //don't care what they put for year, it must be valid if everythign else follows

// TODO: How are dates truncated for formats other than MDY parsed?

// Matches truncated dates.
let regexmd = regex (sprintf "^%s%s%s$" month seperator day)

// Matches a full date.
let regexmdy = regex (sprintf "^%s(?<sep>%s)%s\k<sep>%s$" month validseperators day year)
let regexymd = regex (sprintf "^%s(?<sep>%s)%s\k<sep>%s$" year validseperators month day)
let regexdmy = regex (sprintf "^%s(?<sep>%s)%s\k<sep>%s$" day validseperators month year)

let isregexmdy datestring = if (regexmdy datestring).Success then Some MDY else None
let isregexymd datestring = if (regexymd datestring).Success then Some YMD else None
let isregexdmy datestring = if (regexdmy datestring).Success  then Some DMY else None

let regexsep = regex rangeseperator

let dmytoken = 
    withregexmatchgroups regexymd (
        matchedgroup >> fun g ->
            try
                Some ( Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 3) (g 1) (g 5))))
            with //Should only fail on stuff like 2-31-14
            | ex -> None )

let trydateparse dateformat input =
    try
        match dateformat with
        | MDY | YMD -> Some ( Complete (DateTime.Parse(input)))
        | DMY -> dmytoken input
    with
    | ex -> None

let mdytoken = 
    withregexmatchgroups regexmdy (
        matchedgroup >> fun g ->
            try
                Some ( Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 1) (g 3) (g 5))))
            with //Should only fail on stuff like 2-31-14
            | ex -> None )

let ymdtoken = 
    withregexmatchgroups regexymd (
        matchedgroup >> fun g ->
            try
                Some ( Complete (DateTime.Parse(sprintf "%s-%s-%s" (g 3) (g 5) (g 1))))
            with //Should only fail on stuff like 2-31-14
            | ex -> None )

let mdtoken = withregexmatchgroups regexmd (matchedgroup >> fun g -> Some(Truncated({Month = int(g 1); Day = int(g 3)})) )
let septoken = regexsep >> fun matchof -> if matchof.Success then Some RangeSeperator else None //Fix high order functions


let tokenizetext sanitizer text =
    let sanitized = sanitizer text
    let rec matchof tokenizers input = 
        match tokenizers with
        | [] -> Invalid 
        | tokenizer::others -> 
            match tokenizer input with 
            | Some  token -> token
            | None -> matchof others input
    determinedateformat [isregexmdy;isregexymd;isregexdmy] sanitized
    |> Option.map( fun lexercontext ->
        match lexercontext with
        | LexerContext(MDY, sep) -> 
            let completedate = sprintf "^%s(?<sep>%s)%s\k<sep>%s$" month sep day year
            let partialdate = sprintf "^%s%s%s$" month sep day
            (completedate, partialdate,MDY)
        | LexerContext(YMD, sep) -> 
            let completedate = sprintf "^%s(?<sep>%s)%s\k<sep>%s$" year sep month day
            let partialdate = sprintf "^%s%s%s$" month sep day //Assuming lists are also Month Date
            (completedate,partialdate,YMD)
        | LexerContext(DMY, sep) -> 
            let completedate = sprintf "^%s(?<sep>%s)%s\k<sep>%s$" day sep month year 
            let partialdate = sprintf "^(%s)%s(%s)$" day sep month //Assuming lists are also Date Month 
            (completedate,partialdate,DMY) )
    |> Option.map( fun (complete,partial,context) ->
        let parsecomplete = withregexmatch' complete (fun match' -> trydateparse context match'.Value)
        let parsepartial = withregexmatchgroups' partial ( fun group -> 
            {Month = g )
        sanitized.Split()
        |> Seq.map(matchof [parsecomplete;parsepartial;septoken]) )


// The following is to essentially simplify the regex matching.
let removeordinals text = 
    Regex.Replace(text,"(\d+)(st|nd|rd|th)", fun (matched : Match) -> matched.Groups.[1].Value)


let removepunctuation input = 
    Regex.Replace(input,"[;,\(\)]","") //Harder to predict with various punctuations  

let extractlikedatetoken =
    regexallmatches "(\d+,{0,1}(?<sep>[\.\s\-\/])\d+,{0,1}(\k<sep>\d+){0,1})| \- "
 
/// Converts an "alias" such as a month name or verb for date seperator to something else.
/// Note: Space delimiters make everything alot more difficult it seems.
/// Context with - symbol causes alot of problems.
let simplifyalias text = //Or simplify abbreviation
    let pattern = "\w+" //Extract any sequence of letters or digits
    let f =
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
    Regex.Replace(text,pattern,(fun (matched : Match) -> f matched.Value))
  

let convertspaceseperator text =
    let pattern = "\d+ \d+ \d+|\d+ \d+" //extracts digits seperated by spaces
    Regex.Replace(text,pattern,(fun (m : Match) -> m.Value.Replace(" ","-")))
               
//match specific arrays?


"Back in early 2007, a masked producer showed up to replace East Side Dave. This is that story (well, part 3 since there’s an 11 hour limit on YouTube uploads) Click 'show more' for the chapter list and credits.

Chapter 19-28

00:00:00 - 23-12-2007 - Evaluation Day
01:50:13 - 02-08-2007 - Fez In The Barrel/Rider Pisses On The Israeli Embassy
04:19:58 - 02-09-2007 - Triple Threat Match: Dave vs Black Earl vs Billy Staples
05:20:13 - 02-12-2007 - Less Than Two Minutes Of Interesting Rider Audio
05:21:50 - 02-13-2007 - Dave Day: An Abortion (Featuring Roberto!)
06:08:42 - 02-14-2007 - The Buddays Do A Run In On O&A
07:28:15 - 02-15-2007 - A Metaphysics Discussion With The Rider
08:30:47 - 02-16-2007 - Earl & Dave vs FreeFM
10:41:58 - 02-20-2007 - Jay Mohr Phoner"
|> removepunctuation
|> removeordinals
|> simplifyalias |> fun a -> (do printfn "%O" a); a
|> convertspaceseperator
