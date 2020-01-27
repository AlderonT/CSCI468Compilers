// Learn more about F# at http://fsharp.org

open System

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
        cut : bool
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

    let (<?>) = setLabel

    let returnP (x:'a) =                             //Lifts a value into the parser world
        Parser (string (box x) ,fun input -> Success (x,input)) //parser always suceeds and gives you the value you gave it

    let andThen p1 p2 =
        p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result,p2Result) ))
        //<?> sprintf"%s and %s" p1.Label p2.Label

    let (.>>.) = andThen        //infix operator 

    let orElse (p1:_ Parser) (p2:_ Parser) =
        Parser("unknown",fun input ->
            match p1.ParseFn input with
            | Failure (_,err) when not err.consumedInput ->
                p2.ParseFn input
            | s -> s
        )
        //<?> sprintf"%s or %s" p1.Label p2.Label

    let (<|>) = orElse

    let satisfy predicate label =
        Parser ( label, fun input ->
            match (input:InputState).NextChar() with
            | None ->
                let err = {
                    msg = "No more input"
                    pos = input.Position
                    sourceLine = input.SourceLine
                    consumedInput = false
                    cut = false
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
                        cut = false
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
                    cut = false
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
        p      >>= (fun head ->
        many p >>= (fun tail ->
            returnP (head::tail) ))
        |> allowBacktrack        

    //"a" |> fromStr |> run parseDigit|>printResult

    ////Pg 1

    let parseThreeDigits = 
        parseDigit .>>. parseDigit .>>. parseDigit 
    

    //"     \n12a3" |> fromStr |> run (many (pchar ' ') .>>. pchar '\n' .>>. parseThreeDigits) |>printResult
    //run parseThreeDigits "123A"

    let mapP (fMap:'a->'b) (Parser(_,p)) =
        Parser("unknown",fun input ->
            match p input with
            | Failure (lbl,err) -> Failure (lbl,err)
            | Success (v,r) -> Success(fMap v,r)
        )

    let (<!>) = mapP

    let (|>>) x f = mapP f x

    let parseThreeDigitsAsStr  = 
        (parseDigit .>>. parseDigit .>>. parseDigit)
        |>> fun ((c1,c2),c3) -> String [|c1;c2;c3|]
    
   
    let parseThreeDigitsAsInt  = mapP int parseThreeDigitsAsStr

    let applyP fP xP =             //this will take a parser of a function and apply that function to xP
        fP >>= (fun f ->               // (fP:Parser<('a -> 'b)>) -> (xP:Parser<'a>) -> Parser<'b>
        xP >>= (fun x ->
            returnP (f x) ))

    let (<*>) = applyP

    let lift2 f xP yP = 
        returnP f <*> xP <*> yP 

    let addP = lift2 (+)

    let startsWith (str:string) prefix = 
        str.StartsWith(prefix:string)
    let startsWithP = 
        lift2 startsWith       
    
    let rec sequence parserList =           //take a list of parsers and get a parser of a list 
        let cons head tail = head::tail     //Parser<'a> list -> Parser<'a list>
        let consP = lift2 cons 
        match parserList with 
        | [] -> returnP []
        | head::tail -> consP head (sequence tail)

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
        let resultToInt (sign,charList) = 
            let i = String(List.toArray charList) |> int 
            match sign with 
            | Some _ -> -i //if we have a character, then the result should be a negative number
            | None -> i 
        let digit = anyOf ['0'..'9']    //technically we have this defined way above already
        let digits = many1 digit        //defining a parser for 1 or more digits in a list

        opt (pchar '-') .>>. digits 
        |>> resultToInt
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

    let pfail lbl msg =
        Parser(lbl,fun input ->
            Failure(lbl,{msg = msg; pos = input.Position; sourceLine = input.SourceLine; consumedInput = true; cut = false})
        )

    let delayP f =
        Parser("delay",fun input ->
            let p = f()
            run p input
        )

    //// pg 3

    //////////Int Calculator/////////////
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
    
    

open Parser
open Calculator
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    "(23+1)+((3))*1" |> fromStr |> run pExpr |> printResult
    let pId = Parser.satisfy (System.Char.IsLetter) "letters" |> many1 <?> "id"
    let forStmt = (pString "for" >>. (many1 parseWhitespace) |> allowBacktrack) >>. pId <?> "for"
    let formatStmt = (pString "format" >>. (many1 parseWhitespace) |> allowBacktrack) >>. pId <?> "format"
    let ifStmt = pString "if" >>. (many1 parseWhitespace) >>. pId <?> "if"

    let stmt =  forStmt <|> formatStmt <|> ifStmt

    "format 123" |> fromStr |> run stmt |> printResult

    0 // return an integer exit code
