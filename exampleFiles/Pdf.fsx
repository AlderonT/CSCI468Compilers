// type Stream =
//     {
//         data: byte[]
//         index: int
//         limit: int
//     }
//     with
//         static member New(data:byte[]) = Stream.New(data,0,data.Length)
//         static member New(data,offset,limit) =
//             let data' = if data <> null then data else [||]
//             let index' = if offset > data'.Length then data'.Length else offset
//             let limit' = if limit > data'.Length then data'.Length else limit
//             { data = data'; index = index'; limit = limit' }
//         member stream.atEol =        
//             if stream.index+1 > stream.limit then Error ("","expecting EOF marker (\\r, \\n, \\r\\n or \\n\\r), but reached EOF")
//             elif stream.index+1 = stream.limit then
//                 match stream.data.[stream.index] with
//                 | 10uy (*\n*)
//                 | 13uy (*\r*) -> Success ((),{stream with index = stream.index+1})
//                 | b -> Error ("",sprintf "expecting EOF marker (\\r, \\n, \\r\\n or \\n\\r) at offset %d instead found '%c'(0x%02x)" stream.index (char b) b)
//             else
//                 match stream.data.[stream.index],stream.data.[stream.index+1] with
//                 | 10uy (*\n*), 13uy (*\r*)
//                 | 13uy (*\r*), 10uy (*\n*) -> Success ((),{stream with index = stream.index+2})
//                 | 10uy (*\r*), _
//                 | 13uy (*\r*), _ -> Success ((),{stream with index = stream.index+1})
//                 | b, _ -> Error ("",sprintf "expecting EOF marker (\\r, \\n, \\r\\n or \\n\\r) at offset %d instead found '%c'(0x%02x)" stream.index (char b) b)
//         member stream.atEof = stream.index >= stream.limit            
//         member stream.atEofIn distance = stream.index+distance > stream.limit

type InputStateResponse<'T> =
    | Eof
    | Failed of byte
    | Successful of 'T

and InputState = InputState of data:byte[]*index:int*limit:int
    with
        member __.nextByte() =
            let (InputState(data,index,limit)) = __
            if data = null || index >= limit then Eof, __
            else
                Successful data.[index], InputState(data,index+1,limit)
        member __.nextEol() =
            match __.nextByte() with
            | Successful 10uy, input' ->
                match input'.nextByte() with
                | Successful 13uy, input'' -> Successful [|10uy;13uy|], input''
                | _ -> Successful [|10uy|], input'
            | Successful 13uy, input' ->            
                match input'.nextByte() with
                | Successful 10uy, input'' -> Successful [|13uy;10uy|], input''
                | _ -> Successful [|13uy|], input'
            | Eof, input' -> Eof, input'
            | Failed x,input' -> (*Can't happend due to nextByte implemenation*) Failed x,input'
            | Successful x,input' -> Failed x,input'

        member __.matches(prefix:byte[]) =
            let (InputState(data,index,limit)) = __
            if data = null || (index+prefix.Length) > limit then Eof, InputState(data,limit,limit)
            else
                let rec loop i =
                    if i >= prefix.Length then Successful prefix, InputState(data,index+prefix.Length,limit)
                    elif data.[index+i] <> prefix.[i] then Failed data.[index+i],InputState(data,index+i,limit)
                    else loop (i+1)
                loop 0
        member __.getBytes cnt =
            let (InputState(data,index,limit)) = __
            if data = null || (index+cnt) > limit then Eof, InputState(data,limit,limit)
            else
                Successful data.[index..index+cnt],InputState(data,index+cnt,limit)

                       

        static member New(data) = InputState.New(data,0,(data:byte[]).Length)
        static member New(data,offset,limit) =
            let data' = if data <> null then data else [||]
            let index' = if offset > data'.Length then data'.Length else offset
            let limit' = if limit > data'.Length then data'.Length else limit
            InputState(data',index',limit')
        static member FromString (text:string) = System.Text.Encoding.UTF8.GetBytes text |> InputState.New
        static member FromFile (path:string) = System.IO.File.ReadAllBytes path |> InputState.New



and ParseResult<'T> =
    | Success of result:'T*InputState
    | Error of lbl:string*reason:string*InputState
    with
        member this.Result = match this with | Error (lbl,reason,InputState(_,offset,_)) -> failwithf "%s\nError at [%d]: %s" lbl offset reason | Success (result,_) -> result
        member this.Input = match this with | Error (lbl,reason,InputState(_,offset,_)) -> failwithf "%s\nError at [%d]: %s" lbl offset reason | Success (_,input) -> input
        member this.AsTuple = match this with | Error (lbl,reason,InputState(_,offset,_)) -> failwithf "%s\nError at [%d]: %s" lbl offset reason | Success (result,input) -> result,input
        static member getInput (pr:ParseResult<'T>) = pr.Input
        static member getResult (pr:ParseResult<'T>) = pr.Result

type Parser<'T> =
    | Parser of lbl:string*parser:(InputState->ParseResult<'T>)
    with
        member this.Run (data:byte[]) = this.Run(data,0,data.Length)
        member this.Run (text:string) = System.Text.Encoding.UTF8.GetBytes text |> this.Run
        member this.Run (data,offset,limit) =
            let stream = InputState.New(data,offset,limit)
            this.Run stream
        member this.Run (stream:InputState) =
            let (Parser(_,p)) = this
            p stream




module Parsers =

    let expectingFailMsg expecting actual = sprintf "expecting %s, but found '%c' (0x%02x)" expecting (char actual) actual
    let expectingEofFailMsg expecting = sprintf "expecting %s, but reached EOF" expecting

    let errorExpectingFail lbl expecting input actual = Error(lbl,expectingFailMsg expecting actual,input)
    let errorExpectingEofFail lbl expecting input = Error(lbl,expectingEofFailMsg expecting,input)

    let setLabel lbl (Parser(_,p)) =
        Parser(lbl,fun input ->
            match p input with
            | Error (_,reason,input') -> Error(lbl,reason,input')
            | x -> x
        )
    let (<?>) parser lbl = setLabel lbl parser
    let getLabel (Parser(lbl,_)) = lbl
    let run parser input = let (Parser(_,p)) = parser in p input
    let eof =
        Parser("EOF",fun input ->
            match input.nextByte() with
            | Eof, _ -> Success((),input)
            | _ -> Error ("EOF","not at EOF",input)
        )

    let eol =
        let lbl = "EOL Marker"
        let expecting = "EOL marker (\\r, \\n, \\r\\n or \\n\\r)"
        Parser(lbl,fun input ->
            match input.nextEol() with
            | Eof, input' -> errorExpectingEofFail lbl expecting input'
            | Successful r,input' -> Success(r,input')
            | Failed b,input' -> errorExpectingFail lbl expecting input' b
        )
    let satify predicate lbl =
        Parser(lbl,fun input ->
            match input.nextByte() with
            | Eof, input' -> errorExpectingEofFail lbl lbl input'
            | Successful b, input' when predicate b -> Success(b,input')
            | Failed b, input'
            | Successful b, input' -> errorExpectingFail lbl lbl input b
        )
    let ascii (text:string) =
        let text' = System.Text.Encoding.ASCII.GetBytes text
        let lbl = text
        Parser(lbl,fun input ->
            match input.matches text' with
            | Eof, input' -> errorExpectingEofFail lbl lbl input'
            | Failed b,input' -> errorExpectingFail lbl lbl input' b
            | Successful _, input' -> Success (text,input')
        )

    let pchar (ch:char) = satify ((=) (byte ch)) (string ch)
    let pbyte b = satify ((=) b) (sprintf "0x%02x" b)

    let returnP x = Parser("",fun input -> Success(x,input))
    let bindP f (Parser (lbl,p)) =
        Parser("",fun input ->
            match p input with
            | Error (lbl,reason,input') -> Error(lbl,reason,input')
            | Success (result,input') ->
                let (Parser(_,p')) = f result
                p' input'
        )

    let ( >>= ) p f = bindP f p

    let andThen p1 p2 =
        let lbl = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
        p1 >>= (fun result1 ->
        p2 >>= (fun result2 ->
            returnP (result1,result2)))
        <?> lbl
        
    let ( .>>. ) = andThen

    let orElse (Parser(lblL,pLeft)) (Parser(lblR,pRight)) =
        let lbl = sprintf "%s orElse %s" lblL lblR
        Parser(lbl,fun input ->
            match pLeft input with
            | Error (_,reason,input') ->
                match pRight input with
                | Error _ -> Error(lbl,reason,input')
                | success -> success
            | success -> success
        )

    let ( <|> ) = orElse

    let delayP f =
        Parser("",fun stream ->
            let (Parser(_,p)) = f()
            p stream
        )
        
    let mapP f p =
        let lbl = getLabel p
        p >>= (f >> returnP)
        <?> lbl
    let ( <!> ) = mapP
    let ( |>> ) x f = mapP f x

    type ParserBuilder() =
        member this.Return x = returnP x
        member this.Bind(p,f) = bindP f p

        member this.Delay f = delayP f
        member this.Zero() = returnP ()
        member this.ReturnFrom p = p
        member this.Combine (Parser(_,pUnit),Parser(_,pNext)) =
            Parser("combine",fun input ->
                match pUnit input with
                | Error (lbl,msg,input') -> Error (lbl,msg,input')
                | Success ((),input') -> pNext input'
            )

    let parser = ParserBuilder()
        


    let choice listOfParsers =
        Seq.reduce ( <|> ) listOfParsers

    let anyOf listOfChars =
        let lbl = sprintf "one of %A" listOfChars
        listOfChars
        |> Seq.map pchar
        |> List.ofSeq
        |> choice
        <?> lbl    

    let applyP fP xP =
        let lbl = sprintf "applying %s to %s" (getLabel fP) (getLabel xP)
        (fP .>>. xP)
        |>> (fun (f,x) -> f x)
        <?> lbl

    let ( <*> ) = applyP
    let lift2 f xP yP = returnP f <*> xP <*> yP

    let sequenceP parserList =
        let lbl = sprintf "sequence of %A" (parserList |> List.map getLabel)
        let rec loop parsers input cont =
            match parsers with
            | [] -> cont ([],input)
            | (Parser (plbl,phead)) :: rest ->
                match phead input with
                | Error (_,msg,input') -> Error (lbl,msg,input')
                | Success (result,input') ->
                    loop rest input' (fun (results,input'') -> (result :: results,input'') |> cont)
        Parser(lbl,fun input -> loop parserList input Success)
    
    let pZeroOrMore (Parser(lbl,p)) =
        let rec loop input cont =
            match p input with
            | Error _ -> cont ([],input)
            | Success (result,input') ->
                loop input' (fun (results,input'') -> (result::results, input'') |> cont)
        Parser(sprintf "%s*" lbl,fun input -> loop input Success)
    let many = pZeroOrMore
    let many1 ((Parser(lbl,p)) as parser) =
        let lbl = sprintf "%s+" lbl
        let (Parser(_,pz)) = pZeroOrMore parser
        Parser(lbl,fun input ->
            match p input with
            | Error (_,reason,input') -> Error(lbl,reason,input')
            | Success (result,input') ->
                match pz input' with
                | Error (_,reason,input'') -> Error (lbl,reason,input'')
                | Success (results,input'') -> Success (result::results,input'')
        )

    let optional (Parser(lbl,p)) =
        Parser(sprintf "%s?" lbl,fun input ->
            match p input with
            | Error _ -> Success (None,input)
            | Success (result,input') -> Success (Some result,input')
        )

    let ( .>> ) p1 p2 =
        p1 .>>. p2
        |>> fst

    let ( >>. ) p1 p2 =
        p1 .>>. p2
        |>> snd

    let between pleft pbody pright = pleft >>. pbody .>> pright

    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p,pList) -> p::pList

    let sepBy p sep = sepBy1 p sep <|> returnP []

    // PDF specific parsers

    let ``true`` = ascii "true"
    let ``false`` = ascii "false"
    let boolean =
        ``true`` <|> ``false``
        
    let (|IsWhitespace|_|) (b:byte) =
        match b with
        | 0uy (*nul*)
        | 9uy (*tab*)
        | 10uy (*\n*)
        | 12uy (*\f*)
        | 13uy (*\r*)
        | 32uy (* *) -> Some b
        | _ -> None

    let pcomment =
        let lbl = "%comment"
        Parser(lbl,fun input ->
            match input.nextByte() with
            | Eof, _ -> errorExpectingEofFail lbl "%" input
            | Successful b, input' when b = 37uy (*%*) ->
                let comment = ResizeArray()
                comment.Add(37uy)
                let rec loop input =
                    match (input:InputState).nextEol() with
                    | Failed c, input' ->
                        comment.Add(c)
                        loop input'
                    | Eof, _                    
                    | Successful _, _ -> Success(comment.ToArray(),input)
                loop input'
            | Successful b, input'
            | Failed b,input' -> errorExpectingFail lbl "%" input' b
        )

    let whitespaceChars = satify (function | IsWhitespace _ -> true | _ -> false) "whitespace (\\000,\\t,\\n,\\f,\\r,' ')" |>> (fun a -> [|a|])

    let pwhitespace =
        (whitespaceChars <|> pcomment) |> many1 |>> Array.concat

    let digit = satify (fun b -> 48uy <= b && b <= 57uy) "[0-9]"

    let digits = digit |>> char |> many1 |>> (List.toArray >> System.String)

    let pdfMagic = ascii "%PDF"
    let pdfVersion = pdfMagic >>. ascii "-1." >>. (digit |>> (char >> (sprintf "1.%c")))

    let pSign = anyOf "-+" |>> char <?> "sign"
    let pOptionalSign = pSign |> optional <?> "optional sign"

    let pint =
        pOptionalSign .>>. digits
        |>> (fun (sign,digits) ->
            match sign with
            | Some '+'
            | None -> System.Int32.Parse digits
            | Some '-' -> -1*System.Int32.Parse digits
            | Some c -> failwithf "Unknown character as sign: %c" c (*should never happen!*)
        )
        <?> "integer"        


    let preal =
        let decimal = pchar '.'
        let digitsOptional = digits |> optional
        let form1 = digits .>>. (decimal >>. digitsOptional) |>> (fun (firstDigits,lastDigits) -> match lastDigits with | None -> firstDigits+"." | Some lastDigits -> firstDigits+"."+lastDigits)
        let form2 = decimal >>. digits |>> (fun digits -> "."+digits)
        pOptionalSign .>>. (form1 <|> form2)
        |>> (fun (sign,digits) ->
            match sign with
            | Some '+'
            | None -> System.Double.Parse digits
            | Some '-' -> -1.0 * System.Double.Parse digits
            | Some c -> failwithf "Unknown character as sign: %c" c (*should never happen!*)
        )
        <?> "real"

    let pnumber = preal <|> (pint |>> float) <?> "number"

    let pliteralString =
        let octal = satify (fun b -> 48uy <= b && b <= 55uy) "[0-7]" |>> (((+) 208uy) (*same as - 48uy*) >> int)
        let octal3 =
            octal >>= (fun o1 ->
                (optional octal) >>= (fun o2 ->
                    match o2 with
                    | None -> o1 |> returnP
                    | Some o2 ->
                        (optional octal) >>= (fun o3 ->
                            match o3 with
                            | None -> ((o1 <<< 3)+o2) |> returnP
                            | Some o3 -> ((((o1 <<< 3)+o2)<<<3)+o3) |> returnP
                        )
                )
            )
        let escapedChars = anyOf "nrtbf()\\" |>> (char >> (function | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t' | 'b' -> '\b' | 'f' -> '\f' | c -> c) >> int)
        let escaped =
            pchar '\\'
            >>. choice [octal3;eol |>> (fun _ -> -1);escapedChars]
        let (Parser(_,escapedP)) = escaped
        let lbl = "PDF String Literal (...)"
        Parser(lbl,fun input ->
            match input.nextByte() with
            | Eof, _ -> errorExpectingEofFail lbl "(" input
            | Successful b, input' when b = 40uy (*'('*) ->
                let text = ResizeArray()
                let rec loop insideLiteral input =
                    match (input:InputState).nextByte() with
                    | Eof, _ -> errorExpectingEofFail lbl ")" input
                    | Failed b, input' -> errorExpectingFail lbl ")" input b
                    | Successful b, input' ->
                        match b with
                        | 41uy (*')'*) when insideLiteral ->
                            text.Add(b)
                            Success (Array.empty,input')
                        | 41uy (*')'*) -> Success(text.ToArray(),input')
                        | 40uy (*'('*) ->
                            text.Add(b)
                            match loop true input' with
                            | Error (lbl,reason,input'') -> Error(lbl,reason,input'')
                            | Success (_,input'') -> loop insideLiteral input''
                        | 92uy (*'\\'*) ->
                            match escapedP input with
                            | Error (_,reason,input'') -> Error(lbl,reason,input'')
                            | Success (-1, input'') -> loop insideLiteral input''
                            | Success (c, input'') -> text.Add(byte c); loop insideLiteral input''
                        | c ->
                            text.Add(c)
                            loop insideLiteral input'
                loop false input'                        
            | Failed b, input'
            | Successful b, input' -> errorExpectingFail lbl "(" input' b
        )

    let (|IsHexDigit|_|) (b:byte) =
        if 48uy <= b && b <= 57uy then Some (b-48uy)
        elif 97uy <= b && b <= 102uy then Some (b-97uy+10uy)
        elif 65uy <= b && b <= 70uy then Some (b-65uy+10uy)
        else None

    let phexdigit =
        satify (fun b ->
            (48uy <= b && b <= 57uy)
            || (97uy <= b && b <= 102uy)
            || (65uy <= b && b <= 70uy)) "[0-9a-fA-F]"
        |>> (fun b -> if b > 70uy then b-87uy elif b > 57uy then b - 55uy else b-48uy)

    let phexdigits =
        many phexdigit
        |>> (fun digits ->
            let data = ResizeArray()
            digits
            |> Seq.fold (fun high nibble ->
                if high = 1 then
                    (high <<< 4) + (int nibble)
                else
                    let b = ((high&&&0x0f)<<<4)+(int nibble)
                    data.Add(byte b)
                    1) 1
            |> function | 1 -> () | high -> data.Add(byte ((high&&&0x0f)<<<4))
            data.ToArray()
        )

    let andThenMaybe p1 p2 = p1 .>>. (optional p2)    

    let phexadecimalStrings =
        pchar '<' >>. phexdigits .>> pchar '>'

    let pstar =
        let lbl = "[.]"
        Parser(lbl,fun input ->
            match input.nextByte() with
            | Eof, _ -> errorExpectingEofFail lbl "*" input
            | Failed b, input'
            | Successful b, input' -> Success(b,input')
        )

    let notP (Parser(lbl,p)) =
        let lbl' = sprintf "!%s" lbl
        Parser(lbl',fun input ->
            match p input with
            | Error _ -> Success((),input)
            | Success _ -> Error(lbl',sprintf "not expecting %s" lbl,input)
        )

    let pName =
        let hexEscaped = pchar '#' >>. (phexdigit .>>. phexdigit) |>> (fun (high,low) -> (high <<< 4)+low)
        let regularChar = satify (function | IsWhitespace _ | 47uy | 35uy | 60uy | 62uy | 91uy | 93uy | 40uy | 41uy -> false | _ -> true) "anything other than whitespace, /, #, <, >, [, ], ( and )"
        pchar '/' >>. many (regularChar <|> hexEscaped) |>> (List.toArray >> System.Text.Encoding.UTF8.GetString)

    type PDFObject =
        | Null
        | Boolean of bool
        | Number of float
        | String of byte[]
        | HexString of byte[]
        | Name of string
        | Array of PDFObject list
        | Dictionary of Map<string,PDFObject>
        | IndirectReference of id:int*gen:int
        | Stream of dict:Map<string,PDFObject>*stream:byte[]
        with
            member this.Length =
                match this with
                | Null -> 0
                | Boolean _ -> 1
                | Number _ -> 1
                | String ba -> ba.Length
                | HexString ba -> ba.Length
                | Name s -> s.Length
                | Array l -> l.Length
                | Dictionary m -> m.Count
                | IndirectReference _ -> 2
                | Stream (dict,ba) -> ba.Length
            member this.Value =
                match this with
                | Null -> box null
                | Boolean b -> box b
                | Number n -> box n
                | String ba -> box ba
                | HexString ba -> box ba
                | Name s -> box s
                | Array l -> box l
                | Dictionary m -> box m
                | IndirectReference (id,gen) -> box (id,gen)
                | Stream (dict,ba) -> box (dict,ba)
            member this.ValueAs() : 'a = this.Value |> unbox<'a>
            member this.Resolve (pdfFile:PdfFile) =
                let rec loop v =
                    match v with
                    | Null -> box null
                    | Boolean b -> box b
                    | Number n -> box n
                    | String ba -> box ba
                    | HexString ba -> box ba
                    | Name s -> box s
                    | Array l -> box l
                    | Dictionary m -> box m
                    | Stream (dict,ba) -> box (dict,ba)
                    | IndirectReference (id,gen) ->
                        pdfFile.refMap.[id,gen].value |> loop
                loop this
            member this.ResolveAs (pdfFile:PdfFile) : 'a = this.Resolve pdfFile |> unbox<'a>

    and XRefEntry =
        {
            objid: int
            offset: int
            gen: int
            used: bool
        }
    and IndirectObject =
        {
            objid : int
            gen: int
            value: PDFObject
        }
    and PdfFile =
        {
            version: string
            xrefs: XRefEntry[][]
            body: IndirectObject[]
            trailerDicts : Map<string,PDFObject>[]
            xrefStarts : int[]
            refMap: Map<int*int,IndirectObject>
        }
        with
            static member Empty = { version = null; xref = [||]; body = [||]; trailerDict = Map.empty; xrefStart = -1; refMap = Map.empty }

    let pfail<'a> msg =
        let lbl = sprintf "Fail:%s" msg
        Parser(lbl,fun input -> Error(lbl,msg,input))    

    let pnull = ascii "null"

    let ignoreP p = p |>> ignore

    let pIndirectReference =
        (pint .>> pwhitespace) .>>. (pint .>> pwhitespace) .>> pchar 'R'
        <?> "indirectReference"

    let pstream =
        let stream = ascii "stream" .>> (ascii "\r\n" <|> ascii "\n")
        let streamBody =
            let (Parser(_,endstream)) = ascii "endstream"
            Parser("stream body",fun input ->
                let (InputState(data,start,_)) = input
                let rec loop (input:InputState) =
                    match input.nextByte() with
                    | Eof, _ -> errorExpectingEofFail "stream" "endstream" input
                    | Failed b, input -> errorExpectingFail "stream" "shouldn't happen" input b
                    | Successful 101uy,input' ->
                        match endstream input with
                        | Error _ -> loop input'
                        | Success (_,input'') ->
                            let (InputState(_,endpos,_)) = input
                            let data' = data.[start..endpos-1]
                            Success(data',input'')
                    | Successful _,input' -> loop input'
                loop input                
            )
        (stream >>. streamBody)
        <?> "stream"
    
    let mutable private pObject' = pfail<PDFObject> "not implemented"

    let rec pArray =
        let pObject = delayP(fun _ -> pObject')
        let arrayBody =
            pObject
            |> many
        let arrayStart = pchar '['
        let arrayEnd = pchar ']'
        between arrayStart arrayBody arrayEnd
        <?> "array"

    and pDictionary =
        let pObject = delayP(fun _ -> pObject')        
        let optionalWhitespace = optional pwhitespace
        let name = pName
        let value = pObject
        let keyValuePair = name .>>. value
        let dictBody =
            keyValuePair
            |> many
        let dictStart = (ascii "<<" .>> optionalWhitespace)
        let dictEnd = ascii ">>"
        between dictStart dictBody dictEnd
        |>> (fun entries ->
            // filter out all null values
            entries
            |> Seq.filter (function | _,Null -> false | _ -> true)
            |> Map.ofSeq
        )
        <?> "dictionary"
    and indirectObject =
        let pObject = delayP(fun _ -> pObject')
        let optionalWhitespace = optional pwhitespace |> ignoreP
        parser {
            let! objid = pint
            do! pwhitespace |> ignoreP
            let! gen = pint
            do! optionalWhitespace
            do! ascii "obj" |> ignoreP
            let! content = pObject
            do! ascii "endobj" |> ignoreP
            return objid,gen,content
        }
        //((pint .>> pwhitespace) .>>. (pint .>> optionalWhitespace)) .>>. (between (ascii "obj" .>> optionalWhitespace) pObject (ascii "endobj"))
        //|>> (fun ((id,gen),o) -> id,gen,o)
        <?> "indirectObject"
    and indirectObjectInObjectStream =
        let pObject = delayP(fun _ -> pObject')
        let optionalWhitespace = optional pwhitespace
        (pint .>> pwhitespace) >>= (fun id ->
            (pchar '0' .>> optionalWhitespace) >>= (fun _ ->
                (pObject .>> optionalWhitespace) >>= (fun o ->
                    returnP (id,0,o)
                )
            )
        )    
        <?> "indirectObjectInObjectStream"
    and pObject =
        let optionalWhitespace = optional pwhitespace
        let body =
            (pnull |>> (fun _ -> Null))
            <|> (boolean |>> (System.Boolean.Parse >> Boolean))
            <|> (pIndirectReference |>> IndirectReference)
            <|> (pnumber |>> Number)
            <|> (pliteralString |>> String)
            <|> (phexadecimalStrings |>> HexString)
            <|> (pName |>> Name)
            <|> (pArray |>> Array)
            <|> (andThenMaybe pDictionary (optionalWhitespace >>. pstream)
                 |>> (function | dict, None -> Dictionary dict | dict, Some stream -> Stream(dict,stream)))
        between optionalWhitespace body optionalWhitespace
        <?> "object"            


    pObject' <- pObject

    type PdfBodyElement =
        | StartXRef of int
        | XRef of XRefEntry[]
        | IO of IndirectObject
        | Trailer of Map<string,PDFObject>


    let pstartXref =
        parser {
            do! optional pwhitespace |> ignoreP
            do! ascii "startxref" |> ignoreP
            do! optional pwhitespace |> ignoreP
            let! byteOffsetOfXref = pint
            do! (whitespaceChars |> many) |> ignoreP
            do! ascii "%%EOF" |> ignoreP
            return StartXRef byteOffsetOfXref
        }
        <?> "startxref"

    let ptrailer =
        parser {
            do! optional pwhitespace |> ignoreP
            do! ascii "trailer" |> ignoreP
            do! optional pwhitespace |> ignoreP
            let! dict = pDictionary
            return Trailer(dict)
        }

    let pxref =
        let xrefLine =
            parser {
                do! optional pwhitespace |> ignoreP
                let! offset = pint
                do! pwhitespace |> ignoreP
                let! genNum = pint
                do! pwhitespace |> ignoreP
                let! entryType = pchar 'n' <|> pchar 'f'
                return offset,genNum,(entryType|>char)
            }
        let subSectionP =
            parser {
                do! optional pwhitespace |> ignoreP
                let! objNum = pint
                do! pchar ' ' |> ignoreP
                let! objCnt = pint
                let! lines = xrefLine |> many
                return objNum,objCnt,lines
            }
        parser {
            do! optional pwhitespace |> ignoreP
            do! ascii "xref" |> ignoreP
            let! items = subSectionP |> many1       
            return
                items
                |> Seq.collect (fun (startId,cnt,entries) ->
                    entries
                    |> Seq.mapi (fun i (offset,gen,entryType) ->
                        { objid = startId+i; offset = offset; gen = gen; used = entryType = 'n' }
                    ))
                |> Seq.toArray
                |> XRef
        }

    let pFileBody =
        let indirectObject = indirectObject |>> fun (id,gen,v) -> { objid = id; gen = gen; value = v} |> IO
        parser {
            do! optional pwhitespace |> ignoreP
            return! indirectObject <|> pstartXref <|> pxref <|> ptrailer
        }
        |> many <?> "File Body"

    let pPdfFile =
        parser {
            let! version = pdfVersion
            let! fileBody = pFileBody
            let xrefs = fileBody |> Seq.choose (function | XRef xa -> Some xa | _ -> None) |> Seq.toArray
            let body = fileBody |> Seq.choose (function | IO io -> Some io | _ -> None) |> Seq.toArray
            let xrefStarts = fileBody |> Seq.choose (function | StartXRef sx -> Some sx | _ -> None) |> Seq.toArray
            let trailerDicts = fileBody |> Seq.choose (function | Trailer t -> Some t | _ -> None) |> Seq.toArray
            let refMap = body |> Seq.map (fun x -> (x.objid,x.gen),x) |> Map.ofSeq
            return {
                version = version
                xrefs = xrefs
                trailerDicts = trailerDicts
                xrefStarts = xrefStarts
                body = body
                refMap = refMap
            }
        }
        
    let getStreamContent (pdfFile:PdfFile) ({value=value}:IndirectObject) =
        // we only support FlateDecode right now
        let (|Length|_|) (x:PDFObject option) =
            match x with
            | None -> None
            | Some (Number length) -> int length |> Some
            | Some (IndirectReference(id,gen)) ->
                match pdfFile.refMap.TryFind(id,gen) with
                | None -> None
                | Some o ->
                    match o.value with
                    | Number length -> int length |> Some
                    | _ -> None
            | _ -> None
        match value with
        | Stream(dict,stream) ->
            match dict.TryFind "Length" with
            | None -> failwithf "No Length Specifed for Stream"
            | Length length ->
                match dict.TryFind "Filter" with
                | None -> stream.[0..length-1]
                | Some (Name "FlateDecode") ->
                    use ms = new System.IO.MemoryStream(stream.[2..length-1])
                    use cs = new System.IO.Compression.DeflateStream(ms,System.IO.Compression.CompressionMode.Decompress)
                    use os = new System.IO.MemoryStream()
                    cs.CopyTo(os)
                    cs.Close()
                    os.Close()
                    os.ToArray()
                | Some (Name other) -> failwithf "We don't support %s as a Stream Filter yet!" other
                | Some other -> failwithf "Filter must be a Name value! %A" other
            | Some other -> failwithf "Length property must be a Number or Indirect Reference! %A" other
        | other -> failwithf "IndirectObject must be pointing to a Stream not %A" other

    let pOperatorName =
        [
            "b"
            "B"
            "b*"
            "B*"
            "BDC"
            "BI"
            "BMC"
            "BT"
            "BX"
            "c"
            "cm"
            "CS"
            "cs"
            "d"
            "d0"
            "d1"
            "Do"
            "DP"
            "EI"
            "EMC"
            "ET"
            "EX"
            "f"
            "F"
            "f*"
            "G"
            "g"
            "gs"
            "h"
            "i"
            "ID"
            "j"
            "J"
            "K"
            "k"
            "l"
            "m"
            "M"
            "MP"
            "n"
            "q"
            "Q"
            "re"
            "RG"
            "rg"
            "ri"
            "s"
            "S"
            "SC"
            "sc"
            "SCN"
            "scn"
            "sh"
            "T*"
            "Tc"
            "Td"
            "TD"
            "Tf"
            "Tj"
            "TJ"
            "TL"
            "Tm"
            "Tr"
            "Ts"
            "Tw"
            "Tz"
            "v"
            "w"
            "W"
            "W*"
            "y"
            "'"
            "\""
        ]
        |> Seq.map ascii
        |> choice
        <?> "opNames"

    type Operator =
        {
            name: string
            args: PDFObject list
        }

    let pOperator =
        let optionalWhitespace = optional pwhitespace
        (pObject |> many) .>>. (between optionalWhitespace pOperatorName optionalWhitespace)
        |>> fun (args,name) -> { name = name; args = args }
        <?> "operator"

    let pcode =
        pOperator |> many1

    let parseStreamContent (pdfFile:PdfFile) ({value=value}:IndirectObject) : ParseResult<_> =
        match value with
        | Stream(dict,stream) ->
            // we only support FlateDecode right now
            let (|Length|_|) (x:PDFObject option) =
                match x with
                | None -> None
                | Some (Number length) -> int length |> Some
                | Some (IndirectReference(id,gen)) ->
                    match pdfFile.refMap.TryFind(id,gen) with
                    | None -> None
                    | Some o ->
                        match o.value with
                        | Number length -> int length |> Some
                        | _ -> None
                | _ -> None
            let dummyInput = InputState.New([||])
            match dict.TryFind "Length" with
            | None -> Error("stream content","No Length Specifed for Stream",dummyInput)
            | Length length ->
                let streamDataResult =
                    match dict.TryFind "Filter" with
                    | None -> Success (stream.[0..length-1],dummyInput)
                    | Some (Name "FlateDecode") ->
                        use ms = new System.IO.MemoryStream(stream.[2..length-1])
                        use cs = new System.IO.Compression.DeflateStream(ms,System.IO.Compression.CompressionMode.Decompress)
                        use os = new System.IO.MemoryStream()
                        cs.CopyTo(os)
                        cs.Close()
                        os.Close()
                        Success (os.ToArray(),dummyInput)
                    | Some (Name other) -> Error("stream content",sprintf "We don't support %s as a Stream Filter yet!" other,dummyInput)
                    | Some other -> Error("stream content",sprintf "Filter must be a Name value! %A" other,dummyInput)
                match streamDataResult with
                | Error (a,b,c) -> Error(a,b,c)
                | Success(streamData,_) ->
                    streamData
                    |> InputState.New
                    |> pcode.Run
            | Some other -> Error("stream content",sprintf "Length property must be a Number or Indirect Reference! %A" other,dummyInput)
        | other -> Error("stream content",sprintf "Expecting a Stream Indirect Object got %A instead" other,InputState.New([||]))

open Parsers


module Tests =
    open Parsers

    let data =
        System.IO.Directory.EnumerateFiles(@"I:\Patrick\PDF Examples","*.pdf")
        |> Seq.skip 0
        |> Seq.head
        |> System.IO.File.ReadAllBytes
    let data =
        //@"I:\Patrick\PDF Examples\revsharecw.pdf"
        //@"D:\temp\revsharecw.pdf"
        //@"I:\Patrick\PDF Examples\Revshare  Week of 5  22 Post.pdf"
        @"I:\Patrick\PDF Examples\Revshare - Fuse 5.29 Pre Logs.pdf"
        |> System.IO.File.ReadAllBytes

    let parsings =
        System.IO.Directory.EnumerateFiles(@"I:\Patrick\PDF Examples","*.pdf")
        |> Seq.map (fun fn -> fn, System.IO.File.ReadAllBytes fn)
        |> Seq.map (fun (fn, ba) -> fn, ba |> InputState.New)
        |> Seq.map (fun (fn, is) -> fn, is |> run pPdfFile)
        |> Seq.map (function | fn,Success(x,_) -> fn,x)
        |> Seq.toList

    let pdfFile = parsings.[1] |> snd

    pdfFile.version
    pdfFile.trailerDicts
    pdfFile.xrefStarts
    pdfFile.xrefs
    pdfFile.

    do
        //let! version = pdfVersion
        //let! fileBody = pFileBody
        //let! xref = pxref
        //let! trailerDict,xrefStart = pTrailer
        data |> InputState.New
        |> pdfVersion.Run
        |> ParseResult.getInput
        |> pFileBody.Run
        |> ParseResult.getResult
        |> fun a -> a.[1]
        |> fun (id,gen,s) -> { objid = id; gen = gen; value = s }
        |> getStreamContent PdfFile.Empty

        data.[272+4..] |> InputState.New
        |> pstream.Run


    let pdfFile =
        data |> InputState.New
        |> pPdfFile.Run
        |> ParseResult.getResult

    let getRootElement (pdfFile:PdfFile) =
        let (IndirectReference (id,gen)) = pdfFile.trailerDict.["Root"]
        match pdfFile.refMap.TryFind (id,gen) with
        | None -> failwithf "Can't find Root element of pdf"
        | Some root ->
            match root.value with
            | Dictionary map -> map
            | other -> failwithf "Root Element must be a Dictionary"

    let getValueInMap (key:string) (pdfFile:PdfFile) (m:Map<string,PDFObject>) =
        match m.TryFind key with
        | Some (IndirectReference (id,gen)) ->
            match pdfFile.refMap.TryFind (id,gen) with
            | None -> None
            | Some ({value=value}) -> Some value
        | Some other -> Some other
        | None -> None
    let getValueInDict (key:string) (pdfFile:PdfFile) (Dictionary(m:Map<string,PDFObject>)) = getValueInMap key pdfFile m

    let findReference (pdfFile:PdfFile) (IndirectReference(id,gen)) = pdfFile.refMap.[id,gen]

    let examinePdf (pdfFile:PdfFile) =
        let root = getRootElement pdfFile
        let pages =
            getValueInMap "Pages" pdfFile root
            |> function | Some (Dictionary x) -> x
            |> getValueInMap "Kids" pdfFile
            |> function | Some (Array x) -> x
        let page1 = pages.[0] |> findReference pdfFile |> fun o -> o.value
        let fonts =
            page1 |> getValueInDict "Resources" pdfFile |> Option.get
            |> getValueInDict "Font" pdfFile |> Option.get
            |> function | Dictionary m -> m
            |> Map.map (fun k (IndirectReference (id,gen)) ->
                pdfFile.refMap.[id,gen].value
                |> getValueInDict "Encoding" pdfFile
            )



        

    pdfFile.refMap.[5,0] |> fun o -> o.value |> getStreamContent pdfFile
    |> System.Text.Encoding.UTF8.GetString
    pdfFile.refMap.[7,0]
    let (Stream(dict,stream)) = pdfFile.refMap.[5,0] |> fun o -> o.value

    pdfFile.xref.[5]

    let xa = data.[15+7+1+37+1..] |> InputState.New |> pstream.Run |> ParseResult.getResult
    xa.Length
