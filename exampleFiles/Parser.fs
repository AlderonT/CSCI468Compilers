namespace Parser

module Parser =
    type ParserInput = string
    type Parser<'T> = Parser of (ParserInput -> Result<'T*ParserInput,string>)
    let run (Parser p) input = p input
    let listCharsToStr l = (l:char list) |> List.toArray |> System.String

    let runUnwrapped p i= 
        match run p i with 
        |Ok (x,_)->x
        |Error (m) -> failwith m
    let ignoreP (Parser p) = Parser(fun s -> 
        match p s with 
        |Error m -> Error m 
        |Ok (_,s') -> Ok((),s') 
        )

    let eof = Parser(fun s ->
        if s.Length = 0 then Ok((),s) else Error ("expecting eof, but found additional chars")
    )
    let bindP f (Parser m) = Parser(fun s ->
        match m s with
        | Error m -> Error m
        | Ok (r,s') ->
            let (Parser p) = f r
            p s'
    )
    let (>>=) m f = bindP f m
    let returnP x = Parser(fun s -> Ok(x,s))

    let failwithP fmt = Printf.kprintf (fun msg -> Parser(fun s -> Error msg)) fmt

    type ParserBuilder() =
        member this.Bind (m,f) = bindP f m
        member this.Return x = returnP x
        member this.ReturnFrom p = (p:_ Parser)
        member this.Zero() = returnP ()
        member this.Delay f = f()
        member this.Combine((Parser p1),(Parser p2)) =
            Parser(fun s ->
                match p1 s with
                | Error m -> Error m
                | Ok ((),s') ->
                    p2 s'
            )
    let parser = ParserBuilder()
    let orElse (Parser left) (Parser right) = Parser (fun s ->
        match left s with
        | Error m ->
            right s
        | x -> x        
    )
    let (<|>) left right = orElse left right

    let mapP f p =
        parser {
            let! r = p
            return f r
        }
    let (<!>) = mapP
    let (|>>) x f = mapP f x
    let many (Parser p) =
        Parser(fun s ->
            let rec loop s cont =
                match p s with
                | Error _ -> ([],s) |> cont
                | Ok (r,s') -> loop s' (fun (rest,s'') -> ((r::rest),s'') |> cont)
            loop s Ok            
        )
    let many1 p =
        parser {
            let! first = p
            let! rest = many p
            return first::rest
        }
    let (.>>.) left right =
        parser {
            let! left = left
            let! right = right
            return left,right
        }
    let (.>>) left right = left .>>. right |>> fst
    let (>>.) left right = left .>>. right |>> snd

    let anyChar = Parser (fun s -> if s.Length = 0 then Error "expecting input found EOF instead" else Ok(s.[0],s.[1..]))

    let pchar c =
        parser {
            let! c' = anyChar
            if c' = c then
                return c
            else
                return! failwithP "Expecting [%c] found [%c] instead" c c'
        }
    let notpchar c =
        parser {
            let! c' = anyChar
            if c' <> c then
                return c
            else
                return! failwithP "Not Expecting [%c], but found [%c] instead" c c'
        }
    let choice listOfParsers =
        Seq.reduce (<|>) listOfParsers    
    let anyOf listOfChars =
        let theSet =
            listOfChars
            |> Set.ofSeq
        let expectingOneOf = listOfChars |> Seq.toArray |> System.String                
        parser {
            let! c = anyChar
            if Set.contains c theSet then return c
            else return! failwithP "Expecting one of [%s], but found [%c] instead" expectingOneOf c
        }        
    let notAnyOf listOfChars =
        let theSet =
            listOfChars
            |> Set.ofSeq
        let notExpectingOneOf = listOfChars |> Seq.toArray |> System.String                
        parser {
            let! c = anyChar
            if Set.contains c theSet then return! failwithP "Not expecting one of [%s], but found [%c] anyways" notExpectingOneOf c
            else return c
        }        
    let applyP fP xP =
        parser {
            let! f = fP
            let! x = xP
            return f x
        }
    let (<*>) = applyP
    let lift2 f xP yP =
        returnP f <*> xP <*> yP    

    let pstring (str:string) = Parser (fun s ->
        if s.StartsWith str then
            Ok(str,s.[str.Length..])
        else Error (sprintf "Expecting [%s] found [%s]" str s)        
    )    
    let pstringi (str:string) = Parser (fun s ->
        if s.StartsWith(str,System.StringComparison.InvariantCultureIgnoreCase) then
            Ok(str,s.[str.Length..])
        else Error (sprintf "Expecting [%s] found [%s]" str s)        
    )    

    let pregex (regex:string) =
        let regex' = System.Text.RegularExpressions.Regex(regex)
        Parser (fun s ->
            let m = regex'.Match(s)
            if m.Success then
                Ok(m,s.[m.Value.Length..])
            else Error (sprintf "Expecting regex match [%s]." regex)
        )

    let rec sequence parserList =
        let cons head tail = head::tail
        let consP = lift2 cons
        match parserList with
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

    let optP p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        parser {
            let! p' = p
            let! pList = many (sep >>. p)
            return p'::pList
        }
    let sepBy p sep =
        sepBy1 p sep <|> returnP []    

    let lookAhead (Parser p) (Parser lookahead) = Parser (fun s ->
        match p s with
        | Error m -> Error m
        | Ok (r,s') ->
            match lookahead s' with
            | Error m -> Error m
            | Ok(_,_) -> Ok(r,s')
    )
    let notP (Parser p) = Parser (fun s ->
        match p s with
        | Error _ -> Ok((),s)
        | Ok(_,_) -> Error ("Found parser, should not have")
    )
    let choiceOf2 (Parser p1) (Parser p2) = Parser (fun s ->
        match p1 s with
        | Error m1 ->
            match p2 s with
            | Error m2 -> Error (sprintf "%s or\n%s" m1 m2)
            | Ok (r,s') -> Ok(Choice2Of2(r),s')
        | Ok(r,s') -> Ok(Choice1Of2(r),s')        
    )
    let choiceOf3 (Parser p1) p2 p3 = Parser (fun s ->
        let (Parser p') = choiceOf2 p2 p3
        match p1 s with
        | Error m1 ->
            match p' s with
            | Error m2 -> Error (sprintf "%s or\n%s" m1 m2)
            | Ok (Choice1Of2(r),s') -> Ok(Choice2Of3(r),s')
            | Ok (Choice2Of2(r),s') -> Ok(Choice3Of3(r),s')
        | Ok(r,s') -> Ok(Choice1Of3(r),s')        
    )
    let choiceOf4 p1 p2 p3 p4 =
        choiceOf2 (choiceOf2 p1 p2) (choiceOf2 p3 p4)
        |>> (function
            | Choice1Of2(Choice1Of2 r) -> Choice1Of4 r
            | Choice1Of2(Choice2Of2 r) -> Choice2Of4 r
            | Choice2Of2(Choice1Of2 r) -> Choice3Of4 r
            | Choice2Of2(Choice2Of2 r) -> Choice4Of4 r
        )
    let choiceOf5 p1 p2 p3 p4 p5 =
        choiceOf2 (choiceOf3 p1 p2 p3) (choiceOf2 p4 p5)
        |>> (function
            | Choice1Of2(Choice1Of3 r) -> Choice1Of5 r
            | Choice1Of2(Choice2Of3 r) -> Choice2Of5 r
            | Choice1Of2(Choice3Of3 r) -> Choice3Of5 r
            | Choice2Of2(Choice1Of2 r) -> Choice4Of5 r
            | Choice2Of2(Choice2Of2 r) -> Choice5Of5 r
        )
    let choiceOf6 p1 p2 p3 p4 p5 p6 =
        choiceOf2 (choiceOf3 p1 p2 p3) (choiceOf3 p4 p5 p6)
        |>> (function
            | Choice1Of2(Choice1Of3 r) -> Choice1Of6 r
            | Choice1Of2(Choice2Of3 r) -> Choice2Of6 r
            | Choice1Of2(Choice3Of3 r) -> Choice3Of6 r
            | Choice2Of2(Choice1Of3 r) -> Choice4Of6 r
            | Choice2Of2(Choice2Of3 r) -> Choice5Of6 r
            | Choice2Of2(Choice3Of3 r) -> Choice6Of6 r
        )
    let choiceOf7 p1 p2 p3 p4 p5 p6 p7 =
        choiceOf2 (choiceOf3 p1 p2 p3) (choiceOf4 p4 p5 p6 p7)
        |>> (function
            | Choice1Of2(Choice1Of3 r) -> Choice1Of7 r
            | Choice1Of2(Choice2Of3 r) -> Choice2Of7 r
            | Choice1Of2(Choice3Of3 r) -> Choice3Of7 r
            | Choice2Of2(Choice1Of4 r) -> Choice4Of7 r
            | Choice2Of2(Choice2Of4 r) -> Choice5Of7 r
            | Choice2Of2(Choice3Of4 r) -> Choice6Of7 r
            | Choice2Of2(Choice4Of4 r) -> Choice7Of7 r
        )