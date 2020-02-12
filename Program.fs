open System
#nowarn "40"
module Parser =
    
    //

    type Position = {
        line : int
        column : int
    }

    // Aliases 
    type ParserLabel = string
    type ParserError = {
        msg: string
        pos: Position
        sourceLine: string
        consumedInput : bool
    }

    type InputState =
        {
            text: string
            offset: int
            pos: Position
        }
        with
            member this.NextChar() =
                if this.offset >= this.text.Length then None
                else
                    let ch = this.text.[this.offset]
                    match ch with
                    | '\n' -> Some (ch,{this with offset = this.offset + 1; pos = { line = this.pos.line+1; column = 1} })
                    | _ -> Some (ch,{this with offset = this.offset + 1; pos = { line = this.pos.line; column = this.pos.column+1} })
            member this.NextRegex regex =
                let r = System.Text.RegularExpressions.Regex(regex)
                let m = r.Match(this.text,this.offset)
                if m.Success then
                    if m.Index = this.offset then
                        let lines = m.Value.Split('\n')
                        let pos =
                            {
                                line = this.pos.line + lines.Length-1
                                column = if 0 = lines.Length-1 then this.pos.column + lines.[lines.Length-1].Length else lines.[lines.Length-1].Length
                            }
                        Some(m,{this with offset = this.offset + m.Value.Length; pos = pos})
                    else None
                else None
            member this.SourceLine =
                let lineStart =
                    let rec loop i =
                        if i <= 0 then 0
                        elif i >= this.text.Length then loop (this.text.Length-1)
                        elif this.text.[i] = '\n' then i+1
                        else loop (i-1)
                    loop this.offset
                let lineEnd =                
                    let lineEnd = this.text.IndexOf('\n',this.offset)
                    if lineEnd = -1 then this.text.Length-1
                    else lineEnd-1
                this.text.[lineStart..lineEnd]
            member this.Position = this.pos            
    /// define an initial position
    let initialPos = {line=1; column=1}
    
    let fromStr str = 
        if String.IsNullOrEmpty(str) then
            {text=""; offset=0; pos=initialPos}
        else
            {text=str;offset=0; pos=initialPos}

    type Result<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError
    
    type Parser<'T> = Parser of label:ParserLabel*parseFn:(InputState -> Result<'T * InputState>)
        with 
            member this.Label = 
                match this with 
                | Parser(lbl,_) -> lbl 
            member this.ParseFn= 
                match this with 
                | Parser(_,p) -> p
        
    let printResult result =
        match result with 
        | Success (v,i) -> printfn "%A" v 
        | Failure (lbl,err) ->
            let offsetSpaces = String.replicate (err.pos.column-1) " "
            printfn "Error parsing %s\n%s\n%s\n%s^\nLine:%d,Column:%d" lbl err.msg err.sourceLine offsetSpaces err.pos.line err.pos.column

    let run (Parser(_,p)) input = p input                         //Parser(p) unwraps p this is called an "active pattern"


    //We are taking a function like; f:('a -> Parser<'b>) and making a function parser p:Parser<'a> and returning a parser of type Parser<'b>
    let bindP f (Parser(_,p)) =
        let label = "unknown"
        Parser( label, fun input ->
            match p input with
            | Failure (_,err) ->
                Failure (label,err)     // return error from parser
            | Success (v1,ri) ->                
                let (Parser(_,p'))= f v1  
                match p' ri with   //run a new parser (by applying f to v1) with the remaining input
                | Failure (lbl,err) ->
                    Failure(lbl,{err with consumedInput = ri.pos <> input.pos})
                | s -> s
        )

    let ( >>= ) p f = bindP f p //note: p and f are flipped this is so that we can use piping!
   
    let setLabel (Parser(_,p) ) newLabel = 
        Parser (newLabel, fun input -> 
            match p input with 
            |Failure (_,err) ->
                Failure (newLabel,err)
            | s -> s
        )
    let getLabel (Parser(lbl,_)) = lbl
    let (<?>) = setLabel

    let returnP (x:'a) =                             //Lifts a value into the parser world
        Parser (string (box x) ,fun input -> Success (x,input)) //parser always suceeds and gives you the value you gave it

    let pfail lbl msg =
        Parser(lbl,fun input ->
            Failure(lbl,{msg = msg; pos = input.Position; sourceLine = input.SourceLine; consumedInput = true})
        )

    let delayP f =
        Parser("delay",fun input ->
            let p = f()
            run p input
        )

    let ignoreP (Parser(lbl,p)) =
        Parser(lbl,fun input ->
            match p input with
            | Failure (lbl,err) -> Failure(lbl,err)
            | Success (_,input') -> Success((),input')
        )    

    type ParserBuilder() =
        member this.Return x = returnP x
        member this.Bind(p,f) = bindP f p

        member this.Delay f = delayP f
        member this.Zero() = returnP ()
        member this.ReturnFrom p = p
        member this.Combine (Parser(_,pUnit),Parser(_,pNext)) =
            Parser("combine",fun input ->
                match pUnit input with
                | Failure (lbl,err) -> Failure (lbl,err)
                | Success ((),input') -> pNext input'
            )

    let parser = ParserBuilder()

    // let andThen p1 p2 =
    //     p1 >>= (fun p1Result ->
    //     p2 >>= (fun p2Result ->
    //         returnP (p1Result,p2Result) ))
    //     //<?> sprintf"%s and %s" p1.Label p2.Label
    let andThen p1 p2 =
        parser {
            let! r1 = p1                    //let! acts like bind allowing us to pull the function out fo the parser world
            let! r2 = p2
            return r1,r2
        }

    let (.>>.) = andThen        //infix operator 

    // special case can not use parser builder because we are backtracking on input
    let orElse (p1:_ Parser) (p2:_ Parser) =
        Parser("unknown",fun input ->
            match p1.ParseFn input with
            | Failure (_,err) when not err.consumedInput ->
                p2.ParseFn input
            | s -> s
        )
        //<?> sprintf"%s or %s" p1.Label p2.Label

    let (<|>) = orElse

    // special case because we are implementing a base level parser
    let satisfy predicate label =
        Parser ( label, fun input ->
            match (input:InputState).NextChar() with
            | None ->
                let err = {
                    msg = "No more input"
                    pos = input.Position
                    sourceLine = input.SourceLine
                    consumedInput = false
                } 
                Failure (label, err)
            | Some (ch,remainingInput) ->
                if predicate ch then
                    Success (ch, remainingInput)
                else
                    let err = {
                        msg = sprintf "Unexpected '%c'" ch
                        pos = input.Position
                        sourceLine = input.SourceLine
                        consumedInput = false
                    } 
                    Failure (label,err)
        )
        
    let pchar charToMatch = 
        let predicate ch = (ch = charToMatch) 
        let label = sprintf "%c" charToMatch 
        satisfy predicate label

    let choice seqOfParsers =
        Seq.reduce (<|>) seqOfParsers

    let anyOf seqOfChar =
        seqOfChar
        |> Seq.map pchar
        |> choice
        <?> sprintf "anyOf %A" (seqOfChar |> Seq.toList)

    let parseWhitespace = 
        let predicate = Char.IsWhiteSpace 
        let label = "whitespace"
        satisfy predicate label

    let parseLowercase = 
        let predicate = Char.IsLower 
        let label = "lowercase"
        satisfy predicate label

    let parseDigit =
        let predicate = Char.IsDigit 
        let label = "digit"
        satisfy predicate label
    
    let parseHexDigit =
        let predicate c = '0'<=c && c<='9' || 'A'<=c && c<='F' || 'a'<=c && c<='f'
        let label = "hexdigit"
        satisfy predicate label

    let regexP regex =
        Parser(regex,fun input ->
            match input.NextRegex(regex) with
            | None ->
                let err = {
                    msg = sprintf "failure matching regex: %s" regex
                    pos = input.Position
                    sourceLine = input.SourceLine
                    consumedInput = false
                }
                Failure (regex, err)
            | Some (m,remainingInput) -> Success(m,remainingInput)
        )

    let rec parseZeroOrMore (Parser(_,p) as parser) input =
        match p input with
        |Failure _ -> ([], input)
        |Success (firstv, inputafterfirstParse) ->
            let (subv,ri) = parseZeroOrMore parser inputafterfirstParse
            let values = firstv :: subv 
            (values,ri)

    let allowBacktrack (Parser(lbl,p)) =
        Parser(lbl,fun input ->
            match p input with
            | Failure (lbl,err) -> Failure(lbl,{err with consumedInput = false})
            | s -> s
        )    

    let many parser =
        let label = sprintf "%s*" (parser:_ Parser).Label
        Parser(label,fun input -> Success(parseZeroOrMore parser input))
         
    let many1 p =
        parser {
            let! head = p
            let! tail = many p
            return head::tail
        }
        |> allowBacktrack        

    let mapP (fMap:'a->'b) p =
        parser {
            let! r = p
            return fMap r
        }
        <?> getLabel p

    let (<!>) = mapP

    let (|>>) x f = mapP f x

    let applyP fP xP =             //this will take a parser of a function and apply that function to xP
        parser {
            let! f = fP
            let! x = xP
            return f x
        }

    let (<*>) = applyP

    let lift2 f xP yP = 
        returnP f <*> xP <*> yP 

    let addP = lift2 (+)

    let startsWith (str:string) prefix = 
        str.StartsWith(prefix:string)
    let startsWithP = 
        lift2 startsWith       
    

    //take a list of parsers and get a parser of a list
    let sequence parserList =
        let lbl = sprintf "sequence of %A" (parserList |> List.map getLabel)
        let rec loop parsers input cont =
            match parsers with
            | [] -> cont ([],input)
            | (Parser (plbl,phead)) :: rest ->
                match phead input with
                | Failure (_,err) -> Failure (lbl,err)
                | Success (result,input') ->
                    loop rest input' (fun (results,input'') -> (result :: results,input'') |> cont)
        Parser(lbl,fun input -> loop parserList input Success)

    let charListToString charList =         //helper fn
        String(List.toArray charList)

    let pString str =                       //we can now parse a substring!
        str                                 //we are looking for str
        |> List.ofSeq                       //we make it a list of chars
        |> List.map pchar                   //we then make each char into a char parser
        |> sequence                         //we then make this list of char parsers into a parser of a char string
        |> allowBacktrack
        |> mapP charListToString            //then we use out mapP to convert the char list into a string! (LIKE A BOSS)

 
    let opt p = 
        let some = p |>> Some 
        let none = returnP None
        some <|> none
        <?> sprintf "%s?" p.Label

    let pint = 
        parser {
            let! sign = opt (pchar '-' <|> pchar '+')
            let digit = anyOf ['0'..'9']
            let! digits = many1 digit
            let value = digits |> List.toArray |> String |> int
            return
                match sign with
                | Some '-' -> -value
                | _ -> value
        }
        <?> "integer"


    /// Left side parser
    let (.>>) p1 p2 =
        p1 .>>. p2
        |> mapP (fun (a,b) -> a) // then only keep the first value
    
    /// Right side parser
    let (>>.) p1 p2 =
        p1 .>>. p2
        |> mapP (fun (a,b) -> b) // keep only the second value

    /// Keep only the result of the middle parser
    let between p1 p2 p3 =
        p1 >>. p2 .>> p3

    /// pausing for night pick up in morn

    /// Parses 1+ occurrences of p separated by sep
    let sepBy1 p sep =
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p,pList) -> p::pList
    
    /// Parses 0+ occurrences of p separated by sep
    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    let recursiveDefP lbl =
        let mutableDef = ref (pfail "default Recursive" (sprintf "recursiveDefP %s hasn't nor be assigned to yet" lbl))
        let p = Parser(lbl,fun input -> run !mutableDef input)
        mutableDef,p


module Calculator =
    open Parser

    type Expr =
        | Literal of int
        | Multiply of left:Expr*right:Expr
        | Add of left:Expr*right:Expr

    let pExpr',pExpr = recursiveDefP "expr"
    let pLiteral = pint |>> Literal
    let pParens = pchar '(' >>. pExpr .>> pchar ')'
    // BNF
    // RootExpression : literal | '(' Expression ')'
    let pRootExpression = pLiteral <|> pParens
    // BNF
    // Factor : rootExpression | rootExpression * Factor

    let rec pFactor =
        pRootExpression >>= (fun left ->
        opt (pchar '*' >>. pFactor) >>= (function
            | None -> left |> returnP
            | Some right -> Multiply(left,right) |> returnP))
    // BNF
    // Term : factor | factor + term

    let rec pTerm =
        pFactor >>= (fun left ->
        opt (pchar '+' >>. pTerm) >>= (function
            | None -> left |> returnP
            | Some right -> Add(left,right) |> returnP))
    pExpr' := pTerm
    
module Grammar = 
    open Parser

    type Token = 
        | IntegerLiteral of string 
        | FloatLiteral of string 
        | StringLiteral of string
        | Comment  
        | Whitespace 
        | Keyword of string
        | Operator of string 
        | Identifier of string


    let keywords = ["PROGRAM";"BEGIN";"FUNCTION";"READ";"WRITE";"ENDIF";"IF";"ELSE";"ENDWHILE";"WHILE";"CONTINUE";"END";"BREAK";"RETURN";"INT";"VOID";"STRING";"FLOAT"]
    let operators = [":=";"+";"-";"*";"/";"<=";">=";"=";"!=";"<";">";"(";")";";";","]
    
    let keyword = 
        keywords 
        |> Seq.map pString
        |> choice 
        |>> Keyword

    let operator = 
        operators 
        |> Seq.map pString
        |> choice 
        |>> Operator

    let numberLiteral = 
        let digits = many1 parseDigit |>> (List.toArray >> String)
        parser {
            match! opt (pchar '.' >>. digits) with 
            | Some fraction -> 
                return fraction |> sprintf ".%s"|> FloatLiteral
            | None -> 
                let! prefix = digits
                match! opt (pchar '.' >>. digits) with 
                | Some fraction -> 
                    return fraction |> sprintf "%s.%s" prefix |>FloatLiteral
                | None -> return prefix |> IntegerLiteral
                
        }
    let stringLiteral = 
        let nonDoubleQuote = satisfy (fun c -> c <> '\"') "notDoubleQuote"
        let insideString = many nonDoubleQuote |>> (List.toArray >> String)
        parser {
            do! pchar '\"' |> ignoreP
            let! str = insideString
            do! pchar '\"' |> ignoreP
            return str |> StringLiteral
        }
    let comment = 
        let nonNewLine = satisfy (fun c -> c <> '\n') "notNewLine"
        let insideComment = many nonNewLine|>> (List.toArray >> String)
        parser {            
            do! pString "--"    |>ignoreP
            do! insideComment   |>ignoreP
            do! pchar '\n'      |>ignoreP
            return Comment
        }
    let identifier = 
        let firstChar  = satisfy (System.Char.ToLower>>(fun c -> 'a'<=c && c<='z')) "firstChar"
        let restChar   = satisfy (System.Char.ToLower>>(fun c -> ('a'<=c && c<='z') || System.Char.IsDigit c)) "restChar"
        parser {
            let! first = firstChar
            let! rest = many restChar
            return first::rest |> List.toArray |> String |> Identifier 
        }
    let whitespace = 
        satisfy System.Char.IsWhiteSpace "whitespace" |> many1 |>> (fun _ -> Whitespace)

    let token =
        [
            whitespace
            comment
            keyword
            operator
            stringLiteral
            numberLiteral
            identifier
        ] |> choice

    let tokens = 
        many1 token

module CodeGen =
    open Calculator
    type Operations =
        | LoadConstant of int
        | AddInstr
        | MultiplyInstr

    let optimize (expr:Expr) =
        let rec loop (expr:Expr) cont =
            match expr with
            | Literal c -> cont (Literal c)
            | Add (left, right) ->
                loop left (fun newLeft ->
                    loop right (fun newRight ->
                        match newLeft, newRight with
                        | Literal 0, Literal 0 -> cont (Literal 0)
                        | Literal 0, right -> cont right
                        | left, Literal 0  -> cont left
                        | l,r -> cont (Add (l,r))
                    )
                )
            | Multiply (left, right) ->
                loop left (fun newLeft ->
                    loop right (fun newRight ->
                        match newLeft, newRight with
                        | Literal 1, Literal 1 -> cont (Literal 1)
                        | Literal 1, right -> cont right
                        | left, Literal 1  -> cont left
                        | Literal 0, _
                        | _, Literal 0 -> cont (Literal 0)
                        | l,r -> cont (Multiply (l,r))
                    )
                )
        loop expr id            


    let generateIL (expr:Expr) =
        let rec loop (expr:Expr) cont =
            match expr with
            | Literal i ->
                let instr = LoadConstant i
                Seq.singleton instr
                |> cont
            | Add (left,right) ->
                loop left (fun leftinstrs ->
                    loop right (fun rightinstrs ->
                        seq {
                            yield! leftinstrs
                            yield! rightinstrs
                            yield AddInstr
                        } |> cont
                    )
                )
            | Multiply (left,right) ->
                loop left (fun leftinstrs ->
                    loop right (fun rightinstrs ->
                        seq {
                            yield! leftinstrs
                            yield! rightinstrs
                            yield MultiplyInstr
                        } |> cont
                    )
                )
        loop expr (fun instrs ->
            seq {
                yield ".assembly extern mscorlib {}"
                yield ".assembly Hello {}"
                yield ".module Hello.exe"
                 
                yield ".method static void entrypoint()"
                yield "cil managed"
                yield "{"
                yield "    .entrypoint"
                yield "    .locals init (int32 v)"

                // our code goes here
                yield!
                    instrs
                    |> Seq.map (function
                        | LoadConstant c -> sprintf "    ldc.i4 %d" c
                        | AddInstr -> sprintf "    add"
                        | MultiplyInstr -> sprintf "    mul"
                    )

                yield "    stloc.s v"
                yield "    ldstr \"{0}\""
                yield "    ldloc.s v"
                yield "    box valuetype [mscorlib]System.Int32"
                yield "    call void [mscorlib]System.Console::WriteLine(string,object)"
                yield "    ret"
                yield "}"
            }
            |> String.concat "\n"
        )
        
     
    

open Parser
open Calculator
open Grammar

let printUsage () = 
    printfn "Usage: micro [-h|--help] <file_name>"
    printfn "     -h|--help:   displays usage"
    printfn "     file_name:   .micro source file to be processed"
    

[<EntryPoint>]
let main argv =
    if argv.Length<1 then 
        printUsage()
    elif argv.[0] = "-h" || argv.[0] = "--help" then 
        printUsage()
    else 
        let printToken token = 
            match token with 
            | Whitespace 
            | Comment -> ()
            | Keyword k -> printfn "Token Type: KEYWORD\nValue: %s" k
            | Operator o -> printfn "Token Type: OPERATOR\nValue: %s" o 
            | Identifier i -> printfn "Token Type: IDENTIFIER\nValue: %s" i
            | StringLiteral s -> printfn "Token Type: STRINGLITERAL\nValue: \"%s\"" s
            | IntegerLiteral i -> printfn "Token Type: INTLITERAL\nValue: %s" i
            | FloatLiteral f -> printfn "Token Type: FLOATLITERAL\nValue: %s" f

        let file = System.IO.File.ReadAllText argv.[0]
        let tokenList = file |> fromStr |> run tokens
        match tokenList with
        | Success (result,_) ->
            result |> Seq.iter printToken
            //printfn "lastToken: %A" result.[result.Length-2]
        | Failure _ -> printfn "%A" tokenList

    0 // return an integer exit code
