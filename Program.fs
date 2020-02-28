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
    //type NewInputState<'T>= 
    //    {
    //        Elements: ('T*Position*string) list
    //    }
    //    with member this.Next() = 
    //        match this.elements with 
    //        | [] -> None
    //        | (h,_,_)::rest ->  Some (h,{elements = })
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

    type Expr = //THIS IS OUR ABSTRACT SYNTAX TREE
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
    //type Token = 
    //    | IntegerLiteral of string 
    //    | FloatLiteral of string 
    //    | StringLiteral of string
    //    | Comment  
    //    | Whitespace 
    //    | Keyword of string
    //    | Operator of string 
    //    | Identifier of string
    //let keywords = ["PROGRAM";"BEGIN";"FUNCTION";"READ";"WRITE";"ENDIF";"IF";"ELSE";"ENDWHILE";"WHILE";"CONTINUE";"END";"BREAK";"RETURN";"INT";"VOID";"STRING";"FLOAT"]
    //let operators = [":=";"+";"-";"*";"/";"<=";">=";"=";"!=";"<";">";"(";")";";";","] 
    //let comOperators = ["<=";">=";"=";"!=";"<";">"] 
    ////Seperate the Keywords and operators into a set of parsers for each keyword/operator
    //let keyword = 
    //    keywords 
    //    |> Seq.map pString
    //    |> choice 
    //    |>> Keyword
    //let operator = 
    //    operators 
    //    |> Seq.map pString
    //    |> choice 
    //    |>> Operator

    type Literal =
        | StringLiteral of string
        | IntegerLiteral of string
        | FloatLiteral of string

    type VarType = | Float | Int
    type ReturnType = | Void | Float | Int
    type Decl =
        | String of id:string*literal:string
        | Float of ids: string list
        | Int of ids: string list

    type CondOperators =
        | LessThen
        | GreaterThen
        | Equal
        | NotEqual
        | LessThenOrEqual
        | GreaterThenOrEqual
        with
            override this.ToString() =
                match this with
                | LessThen -> "<"
                | GreaterThen -> ">"
                | Equal -> "="
                | LessThenOrEqual -> "<="
                | GreaterThenOrEqual -> ">="
                | NotEqual -> "!="

    type AddOperators =
        | Add 
        | Sub
        with
            override this.ToString() =
                match this with
                | Add -> "+"
                | Sub -> "-"

    type MulOperators =
        | Mul 
        | Div
        with
            override this.ToString() =
                match this with
                | Mul -> "*"
                | Div -> "/"

    type Expr =
        | IntegerLiteral of string
        | FloatLiteral of string
        | Identifer of string
        | ConditionalExpr of op:CondOperators*left:Expr*right:Expr
        | AddExpr of op:AddOperators*left:Expr*right:Expr
        | MulExpr of op:MulOperators*left:Expr*right:Expr
        | CallExpr of id: string * args:Expr list
    
    type Block =
        {
            decls : Decl list
            stmts : Stmt list
        }
    and Stmt =
        | Read of ids: string list
        | Write of ids: string list 
        | Assign of id: string * expr:Expr
        | Return of expr:Expr 
        | If of cond:Expr * trueBody:Block * elseBody:Block option
        | While of cond:Expr * body:Block

    type ParamDecl =
        | Float of id:string
        | Int of id:string

    type FuncDecl =
        {
            returnType: ReturnType
            name: string
            parameters: ParamDecl list
            decls: Decl list
            stmts: Stmt list
        }

    type Program =
        {
            programName: string
            decls: Decl list 
            functions: FuncDecl list 
        }    

    type AST = 
        | Decl of Decl list
        

    let numberLiteral = 
        let digits = many1 parseDigit |>> (List.toArray >> System.String)
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
        let insideString = many nonDoubleQuote |>> (List.toArray >> System.String)
        parser {
            do! pchar '\"' |> ignoreP
            let! str = insideString
            do! pchar '\"' |> ignoreP
            return (str : string)
        }
    let comment = 
        let nonNewLine = satisfy (fun c -> c <> '\n') "notNewLine"
        let insideComment = many nonNewLine|>> (List.toArray >> System.String)
        parser {            
            do! pString "--"    |>ignoreP
            do! insideComment   |>ignoreP
            do! pchar '\n'      |>ignoreP
            return ()
        }
    let identifier' = 
        let firstChar  = satisfy (System.Char.ToLower>>(fun c -> 'a'<=c && c<='z')) "firstChar"
        let restChar   = satisfy (System.Char.ToLower>>(fun c -> ('a'<=c && c<='z') || System.Char.IsDigit c)) "restChar"
        parser {
            let! first = firstChar
            let! rest = many restChar
            return first::rest |> List.toArray |> System.String
        }
    let whitespaceChars = 
        satisfy System.Char.IsWhiteSpace "whitespace" |> many1 |> ignoreP
    let whitespace =
        many1 (whitespaceChars <|> comment) |> ignoreP

    let FUNCTION = pString "FUNCTION" .>> (opt whitespace) |> ignoreP
    let PROGRAM = pString "PROGRAM" .>> (opt whitespace) |> ignoreP
    let BEGIN = pString "BEGIN" .>> (opt whitespace) |> ignoreP
    let END = pString "END" .>> (opt whitespace) |> ignoreP
    let STRING = pString "STRING" .>> (opt whitespace) |> ignoreP
    let FLOAT = pString "FLOAT" .>> (opt whitespace) |>> (fun _ -> VarType.Float)
    let INT = pString "INT" .>> (opt whitespace) |>> (fun _ -> VarType.Int)
    let VOID = pString "VOID" .>> (opt whitespace) |> ignoreP
    let ASSIGNMENTOP = pString ":=" .>> (opt whitespace) |> ignoreP
    let SEMICOLON = pchar ';' .>> (opt whitespace) |> ignoreP
    let COMMA = pchar ',' .>> (opt whitespace) |> ignoreP
    let LPAREN = pchar '(' .>> (opt whitespace) |> ignoreP
    let RPAREN = pchar ')' .>> (opt whitespace) |> ignoreP
    let READ = pString "READ" .>> (opt whitespace) |> ignoreP
    let WRITE = pString "WRITE" .>> (opt whitespace) |> ignoreP
    let RETURN = pString "RETURN" .>> (opt whitespace) |> ignoreP
    let IF = pString "IF" .>> (opt whitespace) |> ignoreP
    let ENDIF = pString "ENDIF" .>> (opt whitespace) |> ignoreP
    let ELSE = pString "ELSE" .>> (opt whitespace) |> ignoreP
    let WHILE = pString "WHILE" .>> (opt whitespace) |> ignoreP
    let ENDWHILE = pString "ENDWHILE" .>> (opt whitespace) |> ignoreP
    let LESSTHENOP = pchar '<' .>> (opt whitespace) |>> function _ -> LessThen
    let GREATERTHENOP = pchar '>' .>> (opt whitespace) |>> function _ -> GreaterThen
    let LESSTHENOREQUALOP = pString "<=" .>> (opt whitespace) |>> function _ -> LessThenOrEqual
    let GREATERTHENOREQUALOP = pString ">=" .>> (opt whitespace) |>> function _ -> GreaterThenOrEqual
    let EQUALOP = pchar '=' .>> (opt whitespace) |>> function _ -> Equal    
    let NOTEQUALOP = pString "!=" .>> (opt whitespace) |>> function _ -> NotEqual
    let ADDOP = pchar '+' .>> (opt whitespace) |>> function _ -> Add
    let SUBOP = pchar '-' .>> (opt whitespace) |>> function _ -> Sub
    let MULOP = pchar '*' .>> (opt whitespace) |>> function _ -> Mul
    let DIVOP = pchar '/' .>> (opt whitespace) |>> function _ -> Div
    let identifier = identifier' .>> (opt whitespace)
    

    let stringDecl =
        parser {
            do! STRING
            let! id = identifier
            do! ASSIGNMENTOP
            let! literal = stringLiteral
            do! SEMICOLON
            return Decl.String(id,literal)
        }
    let idListTail = many (COMMA >>. identifier)
    let idList =
        identifier .>>. idListTail
        |>> (fun (id,rest) -> id::rest)
    let varDecl =
        parser {
            let! varType = FLOAT <|> INT
            let! ids = idList
            do! SEMICOLON
            return
                match varType with
                | VarType.Float -> Decl.Float ids
                | VarType.Int -> Decl.Int ids
        }

    let decl =
        stringDecl <|> varDecl <?> "decl"
        
    let decls = many decl <?> "decls"

    let returnType =
        (FLOAT |>> (function _ -> ReturnType.Float))
        <|> (INT |>> (function _ -> ReturnType.Int))
        <|> (VOID |>> (function _ -> ReturnType.Void))

    let param_decl =
        parser {
            let! varType = FLOAT <|> INT
            let! id = identifier
            
            return
                match varType with
                | VarType.Float -> ParamDecl.Float id
                | VarType.Int -> ParamDecl.Int id
        }

    let expr', expr = recursiveDefP "expr"

    let parenExpression = LPAREN >>. expr .>> RPAREN 


    let expr_list = sepBy expr COMMA

    let call_expr = 
        parser {
            let! id = identifier 
            let! args = opt (LPAREN >>. expr_list .>> RPAREN)
            return
                match args with
                | None -> Identifer id
                | Some args -> CallExpr(id,args)
        }

    let primary = parenExpression <|> call_expr <|> numberLiteral

    let rec factor = 
        parser {
            let! left = primary
            match! opt ((MULOP <|> DIVOP) .>>. factor) with 
            | None -> return left  
            | Some (op,right) -> return MulExpr(op,left,right)
        }

    let rec term =
        parser {
            let! left = factor
            match! opt ((ADDOP <|> SUBOP) .>>. term) with 
            | None -> return left  
            | Some (op,right) -> return AddExpr(op,left,right)
        }


    expr' := term

    // stmt
    let stmt',stmt = recursiveDefP "stmt"

    let read_stmt = 
        parser {
            do! READ
            do! LPAREN
            let! ids = idList
            do! RPAREN
            do! SEMICOLON
            return Stmt.Read ids
        }

    let assign_stmt = 
        parser {
            let! id = identifier
            do! ASSIGNMENTOP
            let! expr = expr
            do! SEMICOLON
            return Stmt.Assign (id,expr)
        }

    let write_stmt = 
        parser {
            do! WRITE
            do! LPAREN
            let! ids = idList
            do! RPAREN
            do! SEMICOLON
            return Stmt.Write ids
        }

    let return_stmt = 
        parser {
            do! RETURN
            let! expr = expr
            do! SEMICOLON
            return Stmt.Return expr
        }

    let base_stmt = read_stmt <|> write_stmt <|> return_stmt <|> assign_stmt

    
    let condop = 
        LESSTHENOREQUALOP <|> GREATERTHENOREQUALOP <|> NOTEQUALOP <|> LESSTHENOP <|> GREATERTHENOP <|> EQUALOP
        

    let cond = 
        parser {
            let! left = expr
            let! op = condop
            let! right = expr 
            return Expr.ConditionalExpr (op,left,right)
        }

    let stmt_list', stmt_list = recursiveDefP "stmt_list"

    let block =
        parser {
            let! d = decls
            let! sl = stmt_list
            return {decls = d; stmts = sl}
        }

    let elseBlock =
        parser {
            do! ELSE
            return! block
        }

    let if_stmt = 
        parser {
            do! IF
            do! LPAREN
            let! cond = cond
            do! RPAREN
            let! block = block 
            let! elseBlock = opt elseBlock
            do! ENDIF
            return Stmt.If (cond,block,elseBlock)
        }


    let while_stmt =
        parser {
            do! WHILE
            do! LPAREN
            let! cond = cond
            do! RPAREN
            let! block = block 
            do! ENDWHILE
            return Stmt.While (cond,block)
        }

    stmt' := if_stmt <|> while_stmt <|> base_stmt

    stmt_list' := many stmt

    let param_decl_list = sepBy param_decl COMMA

    let funcParameters = between LPAREN param_decl_list RPAREN

    let func_declaration =
        parser {
            do! FUNCTION
            let! returnType = returnType
            let! name = identifier
            let! parameters = funcParameters
            do! BEGIN
            let! decls = decls
            let! stmts = stmt_list
            do! END
            return {
                returnType = returnType
                name = name
                parameters = parameters
                decls = decls
                stmts = stmts
            }
        }

    let func_declarations = many func_declaration

    let pgm_body = 
        parser {
            let! decls = decls
            let! functions = func_declarations
            return decls, functions
        }

    let program = 
        parser {
            do! PROGRAM
            let! id = identifier
            do! BEGIN
            let! decls, functions = pgm_body 
            do! END
            return { programName = id; decls = decls; functions = functions }
        }       
        
    let rec exprToString (expr:Expr) =
        match expr with
        | IntegerLiteral i -> i
        | FloatLiteral f -> f
        | Identifer i -> i
        | ConditionalExpr (o,l,r) ->
            let o = o.ToString()
            sprintf "%s %s %s" (exprToString l) o (exprToString r)
        | AddExpr (o,l,r) ->
            let o = o.ToString()
            sprintf "%s %s %s" (exprToString l) o (exprToString r)
        | MulExpr (o,l,r) ->
            let o = o.ToString()
            sprintf "%s %s %s" (exprToString l) o (exprToString r)
        | CallExpr (i,args) ->
            let args = args |> Seq.map exprToString |> String.concat ", "
            sprintf "%s(%s)" i args

    let rec stmtToString (indent:string) (stmt:Stmt) =
        let bodyToString indent (body:Block) =
            seq {
                let indent' = indent + "   "
                yield!
                    body.decls
                    |> Seq.map (fun decl ->
                        match decl with
                        | Decl.String (i,v) -> sprintf "%sSTRING %s := %A;" indent' i v
                        | Decl.Int ids -> sprintf "%sINT %s;" indent' (ids |> String.concat ", ")
                        | Decl.Float ids -> sprintf "%sFLOAT %s;" indent' (ids |> String.concat ", ")
                    )
                yield!
                    body.stmts
                    |> Seq.map (stmtToString indent')
            }
            |> String.concat "\n"
        match stmt with
        | Stmt.Assign (id,expr) -> sprintf "%s%s := %s;" indent id (exprToString expr)
        | Stmt.Read ids -> sprintf "%sREAD (%s);" indent (ids |> String.concat ",")
        | Stmt.Write ids -> sprintf "%sWRITE (%s);" indent (ids |> String.concat ",")
        | Stmt.Return expr -> sprintf "%sRETURN %s;" indent (exprToString expr)
        | Stmt.If (cond,trueBody,None) ->
            sprintf "%sIF (%s)\n%s\n%sENDIF" indent (exprToString cond) (bodyToString indent trueBody) indent
        | Stmt.If (cond,trueBody,Some elseBody) ->
            sprintf "%sIF (%s)\n%s\n%sELSE\n%s\n%sENDIF" indent (exprToString cond) (bodyToString indent trueBody) indent (bodyToString indent elseBody) indent
        | Stmt.While (cond,body) ->
            sprintf "%sWHILE (%s)\n%s\n%sENDWHILE" indent (exprToString cond) (bodyToString indent body) indent



    let printProgram (p:Program) =
        printfn "PROGRAM %s BEGIN" p.programName
        p.decls
        |> Seq.iter (fun decl ->
            match decl with
            | Decl.String (i,v) -> printfn "   STRING %s := %A;" i v
            | Decl.Int ids -> printfn "   INT %s;" (ids |> String.concat ", ")
            | Decl.Float ids -> printfn "   FLOAT %s;" (ids |> String.concat ", ")
        )
        p.functions
        |> Seq.iter (fun func ->
            let retType =
                match func.returnType with
                | ReturnType.Float -> "FLOAT"
                | ReturnType.Int -> "INT"
                | ReturnType.Void -> "VOID"
            
            printf "   FUNCTION %s %s" retType func.name
            func.parameters
            |> Seq.map (fun param ->
                match param with
                | ParamDecl.Int id -> sprintf "INT %s" id
                | ParamDecl.Float id -> sprintf "FLOAT %s" id)
            |> String.concat ","
            |> printfn "(%s)"
            printfn "   BEGIN"
            func.decls
            |> Seq.iter (fun decl ->
                match decl with
                | Decl.String (i,v) -> printfn "      STRING %s := %A;" i v
                | Decl.Int ids -> printfn "      INT %s;" (ids |> String.concat ", ")
                | Decl.Float ids -> printfn "      FLOAT %s;" (ids |> String.concat ", ")
            )
            func.stmts
            |> Seq.iter (fun stmt -> stmtToString "      " stmt |> printfn "%s")
            printfn "   END"
        )
        printfn "END"

    let printParseTree (p:Program) =
        printfn "%A" p
        //printfn "PROGRAM %s" p.programName
        //printfn "decls ["
        //p.decls
        //|> Seq.iter (fun decl ->
        //    match decl with
        //    | Decl.String (i,v) -> printfn "   DECL STRING id:%s, value: %s;" i v
        //    | Decl.Int ids -> printfn "   DECL INT ids: [ %s ];" (ids |> String.concat "; ")
        //    | Decl.Float ids -> printfn "   DECL FLOAT ids: [ %s ];" (ids |> String.concat "; ")
        //)
        //printfn "]"
        //printfn "functions ["
        //p.functions
        //|> Seq.iter (fun func ->
        //    let retType =
        //        match func.returnType with
        //        | ReturnType.Float -> "FLOAT"
        //        | ReturnType.Int -> "INT"
        //        | ReturnType.Void -> "VOID"
            
        //    printf "   FUNCTION %s %s" retType func.name
        //    func.parameters
        //    |> Seq.map (fun param ->
        //        match param with
        //        | ParamDecl.Int id -> sprintf "INT %s" id
        //        | ParamDecl.Float id -> sprintf "FLOAT %s" id)
        //    |> String.concat ","
        //    |> printfn "(%s)"
        //    printfn "   BEGIN"
        //    func.decls
        //    |> Seq.iter (fun decl ->
        //        match decl with
        //        | Decl.String (i,v) -> printfn "      STRING %s := %A;" i v
        //        | Decl.Int ids -> printfn "      INT %s;" (ids |> String.concat ", ")
        //        | Decl.Float ids -> printfn "      FLOAT %s;" (ids |> String.concat ", ")
        //    )
        //    func.stmts
        //    |> Seq.iter (fun stmt -> stmtToString "      " stmt |> printfn "%s")
        //    printfn "   END"
        //)
        //printfn "END"

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
        //let printToken token = 
        //    match token with 
        //    | Whitespace 
        //    | Comment -> ()
        //    | Keyword k -> printfn "Token Type: KEYWORD\nValue: %s" k
        //    | Operator o -> printfn "Token Type: OPERATOR\nValue: %s" o 
        //    | Identifier i -> printfn "Token Type: IDENTIFIER\nValue: %s" i
        //    | StringLiteral s -> printfn "Token Type: STRINGLITERAL\nValue: \"%s\"" s
        //    | IntegerLiteral i -> printfn "Token Type: INTLITERAL\nValue: %s" i
        //    | FloatLiteral f -> printfn "Token Type: FLOATLITERAL\nValue: %s" f

        let file = System.IO.File.ReadAllText argv.[0]
        //let tokenList = file |> fromStr |> run tokens
        //match tokenList with
        //| Success (result,_) ->
        //    result |> Seq.iter printToken
        //    //printfn "lastToken: %A" result.[result.Length-2]
        //| Failure _ -> printfn "%A" tokenList
        let program = file |> fromStr |> run program
        match program with
        | Success (result,_) ->
            //printProgram result
            //printParseTree result
            //printfn "lastToken: %A" result.[result.Length-2]
            printfn "Accepted"
        | Failure _ ->
            //printfn "%A" program
            printfn "Not Accepted"
    0 // return an integer exit code
