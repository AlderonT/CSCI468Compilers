namespace micro

open System
#nowarn "40"  
    
module Grammar = 
    open Parser
    open AST

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
                
        } .>> (opt whitespace) 
    let stringLiteral = 
        let nonDoubleQuote = satisfy (fun c -> c <> '\"') "notDoubleQuote"
        let insideString = many nonDoubleQuote |>> (List.toArray >> System.String)
        parser {
            do! pchar '\"' |> ignoreP
            let! str = insideString
            do! pchar '\"' |> ignoreP
            return (str : string)
        } .>> (opt whitespace) 



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
            return [Decl.String(id,literal)]
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
                | VarType.Float -> ids |> List.map Decl.Float
                | VarType.Int -> ids |> List.map Decl.Int
        }

    let decl =
        stringDecl <|> varDecl <?> "decl"
        
    let decls = many decl |>> List.collect id <?> "decls"

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

    let factor =
        let rec factor reciprocal = 
            parser {
                let! left = if reciprocal then primary |>> ReciprocalExpr else primary
                match! opt (((MULOP >>. returnP false) <|> (DIVOP >>. returnP true)) >>= factor) with 
                | None -> return left  
                | Some (right) -> return MulExpr(left,right)
            }
        factor false

    let term =
        let rec term negate =
            parser {
                let! left = if negate then factor |>> NegateExpr else factor
                match! opt (((ADDOP >>. returnP false) <|> (SUBOP >>. returnP true)) >>= term) with 
                | None -> return left  
                | Some (right) -> return AddExpr(left,right)
            }
        term false

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
            return {decls = d; stmts = sl; symbolTable = None}
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
                symbolTable = None
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
            return { programName = id; decls = decls; functions = functions; symbolTable = None }
        }       
        
    let rec exprToString (expr:Expr) =
        match expr with
        | IntegerLiteral i -> i
        | FloatLiteral f -> f
        | Identifer i -> i
        | ConditionalExpr (o,l,r) ->
            let o = o.ToString()
            sprintf "%s %s %s" (exprToString l) o (exprToString r)
        | NegateExpr expr ->
            sprintf "(-%s)" (exprToString expr)
        | ReciprocalExpr expr ->
            sprintf "(1/%s)" (exprToString expr)
        | AddExpr (l,r) ->
            sprintf "%s + %s" (exprToString l) (exprToString r)
        | MulExpr (l,r) ->
            sprintf "%s * %s" (exprToString l) (exprToString r)
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
                        | Decl.Int id -> sprintf "%sINT %s;" indent' id
                        | Decl.Float id -> sprintf "%sFLOAT %s;" indent' id
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
            | Decl.Int id -> printfn "   INT %s;" id
            | Decl.Float id -> printfn "   FLOAT %s;" id
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
                | Decl.Int id -> printfn "      INT %s;" id
                | Decl.Float id -> printfn "      FLOAT %s;" id
            )
            func.stmts
            |> Seq.iter (fun stmt -> stmtToString "      " stmt |> printfn "%s")
            printfn "   END"
        )
        printfn "END"

    let printParseTree (p:Program) =
        printfn "%A" p
