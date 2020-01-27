namespace CIF
module CifParser =
    open Parser.Parser
    module Internals =

        let sp = pchar ' '
        let ht = pchar '\t'
        let eol =
            (pchar '\n' .>> (optP (pchar '\r')))
            <|> ((pchar '\r' >>. (optP (pchar '\n'))) >>= (function | None -> returnP '\n' | Some ch -> returnP ch))
        let noteol = notAnyOf "\n\r"
        let singleQuote = pchar '\''
        let doubleQuote = pchar '"'
        let ordinaryChar =
            choice [
                anyOf "!%&()*+,-./"
                anyOf ['0'..'9']
                anyOf ":<=>?@"
                anyOf ['A'..'Z']
                anyOf "\\^`"
                anyOf ['a'..'z']
                anyOf "{|}~"
            ]
        let anyPrintChar =
            choice [
                ordinaryChar
                doubleQuote
                singleQuote
                sp
                ht
                anyOf "#$_;[]"
            ]
        let textLeadChar =
            choice [
                ordinaryChar
                doubleQuote
                anyOf "#$_[]"
                singleQuote
                sp
                ht
            ]
        let nonBlankChar =
            choice [
                ordinaryChar
                doubleQuote
                singleQuote
                anyOf "#$_;[]"
            ]
        let comments =
            let commentLine =
                parser {
                    let! hash = pchar '#'
                    let! chars = anyPrintChar |> many
                    let! endOfLine = (eol |> ignoreP) <|> eof
                    return chars |> List.toArray |> System.String
                }
            many1 commentLine        
        let tokenizedComments =
            let prefix =
                choice [
                    sp |> ignoreP
                    ht |> ignoreP
                    eol |> ignoreP
                ]
                |> many
                |>> (fun _ -> ())
            prefix >>. comments        
        let whitespace =
            choice [
                sp |> ignoreP
                ht |> ignoreP
                eol |> ignoreP
                tokenizedComments |> ignoreP
            ]
            |> many1
            |>> (fun _ -> ())

        let data_ = pstringi "data_"
        let loop_ = pstringi "loop_"
        let save_ = pstringi "save_"
        let stop_ = pstringi "stop_"
        let global_ = pstringi "global_"

        // // Version using a negated parser
        // let singleQuotedString =
        //     let notSingleQuote = Parser(fun s ->
        //         let (Parser p) = anyPrintChar
        //         match p s with
        //         | Ok ('\'',_) -> Error "found '"
        //         | x -> x
        //     )
        //     let strChar =
        //         (lookAhead singleQuote (notP (whitespace<|>eof)))
        //         <|> notSingleQuote
        //     parser {
        //         let! _ = singleQuote
        //         let! chars = many strChar
        //         let! _ = singleQuote
        //         return chars |> List.toArray |> System.String
        //     }
        // better version using lookahead directly    
        let singleQuotedString =
            let strChar =
                choiceOf2 (lookAhead singleQuote (whitespace<|>eof)) anyPrintChar
                >>= (function | Choice1Of2 _ -> failwithP "found ' followed by whitespace" | Choice2Of2 r -> returnP r)
            parser {
                let! _ = singleQuote
                let! chars = many strChar
                let! _ = singleQuote
                return chars |> List.toArray |> System.String
            }
        let doubleQuotedString =
            let strChar =
                choiceOf2 (lookAhead doubleQuote (whitespace<|>eof)) anyPrintChar
                >>= (function | Choice1Of2 _ -> failwithP "found \" followed by whitespace" | Choice2Of2 r -> returnP r)
            parser {
                let! _ = doubleQuote
                let! chars = many strChar
                let! _ = doubleQuote
                return chars |> List.toArray |> System.String
            }

        let textField =
            let semiColonTextField =
                let prefixWhitespace = many (sp <|> ht) |>> listCharsToStr
                let innerText =
                    let line =
                        parser {
                            let! firstChar = textLeadChar
                            let! rest = many anyPrintChar
                            return firstChar::rest |> listCharsToStr
                        }
                    line .>> eol
                parser {
                    let! _ = lookAhead eol (prefixWhitespace.>>pchar ';')
                    let! prefix = (prefixWhitespace.>>pchar ';')
                    let! firstText = (many anyPrintChar).>>eol |>> listCharsToStr            
                    let! restText = many innerText
                    let! _ = prefixWhitespace .>> pchar ';'
                    let r = if System.String.IsNullOrWhiteSpace firstText then restText else firstText::restText
                    return r |> String.concat "\n"
                }
            semiColonTextField
        let unQuotedString =
            let eolUnQuotedString' =
                parser {
                    let! firstChar = ordinaryChar<|>pchar ';'
                    let! rest = many nonBlankChar
                    return firstChar::rest |> listCharsToStr
                }
            //let noteolUnQuotedString' =
            //    parser {
            //        let! firstChar = ordinaryChar <|> pchar ';'
            //        let! rest = many nonBlankChar
            //        return firstChar::rest |> listCharsToStr
            //    }

            //let eolUnQuotedString =
            //    parser {
            //        let! _ = lookAhead eol eolUnQuotedString'
            //        return! eolUnQuotedString'
            //    }
            //let noteolUnQuotedString =
            //    parser {
            //        let! c = lookAhead noteol noteolUnQuotedString'
            //        let! rest = noteolUnQuotedString'
            //        return (string c)+rest
            //    }
            parser {        
                let! value = eolUnQuotedString'(*<|>noteolUnQuotedString*)
                match value.ToLowerInvariant() with
                | "loop_"
                | "stop_"
                | "global_" -> return! failwithP "loop_, stop_ and global_ are reserved words"
                | v when value.StartsWith("data_") || value.StartsWith("save_") -> return! failwithP "%s is a reserved word" value
                | _ -> return value
            }

        let charString =
            singleQuotedString <|> doubleQuotedString <|> unQuotedString

        let digit = anyOf ['0'..'9']
        let unsignedInteger = many1 digit |>> listCharsToStr
        let exponent =
            let e = anyOf "eE"
            let sign = anyOf "+-"
            parser {
                let! e = choiceOf2 (e .>>. sign) e
                let! i = unsignedInteger
                match e with
                | Choice1Of2 (e,s) -> return (string e)+(string s)+i
                | Choice2Of2 e -> return (string e)+i
            }
        let integer' =
            parser {
                let! s = optP (anyOf "+-")
                let! i = unsignedInteger
                match s with
                | None -> return i
                | Some s -> return (string s) + i
            }
        let float' =
            let ie = integer' .>>. exponent |>> (fun (a,b) -> string a + string b)
            let sign = (optP (anyOf "+-") |>> (function | None -> "" | Some s -> string s))
            let exp = (optP exponent) |>> (function | None -> "" | Some e -> e)
            let di =
                (((many digit) |>> listCharsToStr) .>> pchar '.' .>>. unsignedInteger)
                |>> fun (prefix,rest) -> prefix + "." + rest
            let dd = (many1 digit) .>> pchar '.' |>> listCharsToStr
            let x = sign .>>. (di <|> dd) .>>. exp |>> (fun ((sign,n),e) -> sign+n+e)
            ie<|>x
        type Number =
            | Integer of int64
            | Float of float
            with
                member this.asInt = match this with | Integer i -> i | Float f -> int64 f
                member this.asFloat = match this with | Integer i -> float i | Float f -> f
        let number =
            choiceOf2 float' integer'
            |>> function
                | Choice1Of2 f -> System.Double.Parse f |> Float
                | Choice2Of2 i -> System.Int64.Parse i |> Integer
        type Numeric =
            | Indexed of number:Number*index:int
            | Base of number:Number
        let numeric =
            choiceOf2
                (number .>>. (between (pchar '(') unsignedInteger (pchar ')')))
                number
            |>> function
                | Choice1Of2 (number,index) -> Indexed (number,System.Int32.Parse index)
                | Choice2Of2 number -> Base number
        type Values =
            | NA
            | Unknown
            | StringValue of string
            | NumericValue of Numeric
            | ListValue of Values list
        let value =
            choiceOf5
                textField // put this here so that we find the \n; pattern
                (whitespace >>. (pchar '.'))
                (whitespace >>. (pchar '?'))
                (whitespace >>. numeric)
                (whitespace >>. charString)
            |>> function
                | Choice1Of5 s -> StringValue s
                | Choice2Of5 _ -> NA
                | Choice3Of5 _ -> Unknown
                | Choice4Of5 n -> NumericValue n
                | Choice5Of5 s -> StringValue s
        let tag =
            parser {
                let! _ = pchar '_'
                let! rest = many1 nonBlankChar
                return '_'::rest |> listCharsToStr
            }
        let loopBody = many value
        let loopHeader =
            parser {
                let! _ = loop_
                let! tags = many1 (whitespace >>. tag)
                return tags
            }
        type DataItem =
            | Tag of tag:string*value:Values
            | Records of Map<string,Values> list    
        let dataItems =
            choiceOf2
                (tag .>>. value)
                (loopHeader .>>. loopBody)
            >>= function
                | Choice1Of2 (tag,value) -> returnP (Tag (tag,value))
                | Choice2Of2 (headers,values) ->
                    // some context here
                    // there MUST BE header.length*n values
                    if (values.Length / headers.Length)*headers.Length <> values.Length then
                        let headerText = headers |> String.concat ", "
                        failwithP "Loop Body does not have enough values for header: %s" headerText
                    else                    
                        let rec loop values cont =
                            let rec loop2 headers values m =
                                match headers, values with
                                | [], values -> loop values (fun rs -> m::rs |> cont)
                                | header::headerRest, value::valueRest ->
                                    loop2 headerRest valueRest (Map.add header value m)
                                | _ -> failwithf "Opps this should have been caught by length check above"
                            match values with
                            | [] -> cont []
                            | _ ->
                                loop2 headers values Map.empty
                        loop values (Records >> returnP)
        let dataBlockHeading =
            parser {
                let! _ = data_
                let! heading = many1 nonBlankChar |>> listCharsToStr
                return heading;
            }
        let saveFrameHeading =
            parser {
                let! _ = save_
                let! heading = many1 nonBlankChar |>> listCharsToStr
                return heading;
            }
        let saveFrame =
            parser {
                let! saveHeading = saveFrameHeading
                let! saved = many1 (whitespace >>. dataItems)
                let! _ = whitespace
                let! _ = save_
                return saveHeading,saved
            }
        type DataBlock =
            {
                heading: string
                items: DataItem list
                fields: Map<string,Values>
                recordGroups: (string*Set<string>*(Map<string,Values> list)) list
            }
        let dataBlock =
            let items =
                choiceOf2 dataItems saveFrame
                |>> function | Choice1Of2 item -> Some item | Choice2Of2 frame -> None // filter out the save frames we don't care about them
            parser {
                let! heading = dataBlockHeading
                let! items = many (whitespace >>. items) |>> List.choose id
                let fields =
                    items
                    |> Seq.choose (function
                        | Tag (tag,value) -> Some (tag, value)
                        | Records records when records.Length = 0 -> None
                        | Records records when records.[0].Count <> 1 -> None
                        | Records records ->
                                let label = records.[0] |> Seq.head |> fun (KeyValue(k,_)) -> k
                                // only one field so we return the set of records as a list
                                let values = records |> List.map (fun m -> m.[label])
                                Some (label,ListValue(values)))
                    |> Map.ofSeq
                let recordGroups =
                    items
                    |> Seq.choose (function
                        | Tag _ -> None
                        | Records records when records.Length = 0 -> None
                        | Records records when records.[0].Count < 2 -> None
                        | Records records ->
                            let labels = records.[0] |> Seq.map (fun (KeyValue(k,_)) -> k) |> Seq.sort |> Seq.toList
                            let labelsTxt = labels |> String.concat ", "
                            let labelsSet = labels |> Set.ofSeq
                            Some (labelsTxt,labelsSet,records))
                    |> Seq.sortBy (fun (a,_,_) -> a)
                    |> Seq.toList
                return { heading = heading; items = items; fields = fields; recordGroups = recordGroups }
            }
        let cif =
            let dataBlocks =
                parser {
                    let! block = dataBlock
                    let! rest = many (whitespace >>. dataBlock)
                    return block::rest
                }
            parser {
                let! _ = optP comments
                let! _ = optP whitespace
                let! blocks = optP dataBlocks
                let! _ = optP whitespace
                let! _ = eof
                return match blocks with | None -> [] | Some blocks -> blocks
            }

    let parseString (txt:string) = runUnwrapped Internals.cif txt
    let parseFile (path:string) =
        let txt = System.IO.File.ReadAllText path
        runUnwrapped Internals.cif txt
    let parseStream (s:System.IO.Stream) =
        let txt =
            use sr = new System.IO.StreamReader(s)
            sr.ReadToEnd()
        runUnwrapped Internals.cif txt



