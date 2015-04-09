module RFTimeline.DateParseToken

open System

type Date = Day of DateTime | DateRange of DateTime * DateTime 

//For dates parsed as 3-22 or 03/22 etc.
type TruncatedDate = {Month:int;Day:int}

//Holds incomplete dates because e.g. a range of 2/27 - 3/1 is impossible to get inbetween dates on account of leap years.
type IncompleteDate = IncompleteDay of TruncatedDate | IncompleteRange of TruncatedDate * TruncatedDate

//Possible tokens that can be regexed out to parse. e.g. 3-13 - 3-17-2015 -> list<Truncated;RangeSeperator;Complete>
type DateParseToken = Truncated of TruncatedDate | Complete of DateTime | RangeSeperator | Invalid

type DateExpansionState = {Previous : DateParseToken; IsRangeEnd : bool; Incompletes : IncompleteDate list; Dates : Date list }

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
