namespace micro

open System
#nowarn "40"

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
