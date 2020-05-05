namespace micro

open System
open AST

module TinyCode = 

    type InstructionOperand = 
        | Memory of id:string 
        | Register of idx:int
        | IntConstant of int
        | FloatConstant of float
        | StringID of id:string
        with 
            member this.Text = 
                match this with 
                | IntConstant d -> sprintf "%d" d
                | FloatConstant f -> sprintf "%f" f
                | Register r -> sprintf "r%d" r 
                | Memory i 
                | StringID i -> i

    type Instruction =
        | Var   of name   :string 
        | Str   of name   :InstructionOperand*value:string
        | Label of target :string
        | Move  of source :InstructionOperand*destination:InstructionOperand
        | AddI  of opmrl  :InstructionOperand*reg:InstructionOperand
        | AddR  of opmrl  :InstructionOperand*reg:InstructionOperand
        | SubI  of opmrl  :InstructionOperand*reg:InstructionOperand
        | SubR  of opmrl  :InstructionOperand*reg:InstructionOperand
        | MulI  of opmrl  :InstructionOperand*reg:InstructionOperand
        | MulR  of opmrl  :InstructionOperand*reg:InstructionOperand
        | DivI  of opmrl  :InstructionOperand*reg:InstructionOperand
        | DivR  of opmrl  :InstructionOperand*reg:InstructionOperand
        | IncI  of reg    :InstructionOperand
        | DecI  of reg    :InstructionOperand
        | CmpI  of opmrl  :InstructionOperand*reg:InstructionOperand
        | Push  of src    :InstructionOperand
        | Pop   of dest   :InstructionOperand
        | JSR   of tgt    :InstructionOperand
        | Ret 
        | Link  of pf     :InstructionOperand
        | Unlnk
        | CmpR  of opmrl  :InstructionOperand*reg:InstructionOperand
        | Jmp   of target :string
        | Jgt   of target :string
        | Jlt   of target :string
        | Jge   of target :string
        | Jeq   of target :string
        | Jne   of target :string
        | SysReadI  of opmr  :InstructionOperand
        | SysReadR  of opmr  :InstructionOperand
        | SysWriteI of source:InstructionOperand
        | SysWriteR of source:InstructionOperand
        | SysWriteS of source:InstructionOperand
        | SysHalt 
        | End
        with 
            member this.Text = 
                match this with 
                | Var n      -> sprintf "var %s" n 
                | Str (n,s)  -> sprintf "str %s \"%s\"" n.Text s 
                | Label n    -> sprintf "label %s" n 
                | Move (s,d) -> sprintf "move %s %s" s.Text d.Text 
                | AddI (o,r) -> sprintf "addi %s %s" o.Text r.Text 
                | AddR (o,r) -> sprintf "addr %s %s" o.Text r.Text 
                | SubI (o,r) -> sprintf "subi %s %s" o.Text r.Text 
                | SubR (o,r) -> sprintf "subr %s %s" o.Text r.Text 
                | MulI (o,r) -> sprintf "muli %s %s" o.Text r.Text 
                | MulR (o,r) -> sprintf "mulr %s %s" o.Text r.Text 
                | DivI (o,r) -> sprintf "divi %s %s" o.Text r.Text 
                | DivR (o,r) -> sprintf "divr %s %s" o.Text r.Text 
                | IncI r     -> sprintf "inci %s" r.Text
                | DecI r     -> sprintf "deci %s" r.Text
                | CmpI (o,r) -> sprintf "cmpi %s %s" o.Text r.Text 
                | Push s     -> sprintf "push %s" s.Text
                | Pop  d     -> sprintf "pop %s" d.Text
                | JSR  t     -> sprintf "push %s" t.Text
                | Ret        -> "ret"
                | Link pf    -> sprintf "link %s" pf.Text
                | Unlnk      -> "unlnk"
                | CmpR (o,r) -> sprintf "cmpr %s %s" o.Text r.Text 
                | Jmp  t     -> sprintf "jmp %s" t
                | Jgt  t     -> sprintf "jgt %s" t
                | Jlt  t     -> sprintf "jlt %s" t
                | Jge  t     -> sprintf "jge %s" t
                | Jeq  t     -> sprintf "jeq %s" t
                | Jne  t     -> sprintf "jne %s" t
                | SysReadI o -> sprintf "sys readi %s" o.Text
                | SysReadR o -> sprintf "sys readr %s" o.Text
                | SysWriteI s-> sprintf "sys writei %s" s.Text
                | SysWriteR s-> sprintf "sys writer %s" s.Text
                | SysWriteS s-> sprintf "sys writes %s" s.Text
                | SysHalt    -> "sys halt"
                | End        -> "end"

    type Code = seq<Instruction>

    let printInstructions (is:Code) = 
        is
        |>Seq.iter (fun i -> printfn "%s" i.Text)

    let concatInstructions (is:Code) = 
        is
        |>Seq.map (fun i -> sprintf "%s" i.Text)
        |>String.concat "\n"

    let writeCodeToFile file (code:string)   = 
        System.IO.File.WriteAllText(file,code)

    let writeCodeToConsole (code:string)   = 
        printfn "%s" code

    type CodeBlock =
        {
            instrs: Instruction list
            nextRegister: int
            exprType: VarType
        }
        with
            static member New instrs nextRegister exprType = { instrs = instrs; nextRegister = nextRegister; exprType = exprType }

    type ExprIR = 
        {
            returnType  : InstructionOperand
            instructions: List<Instruction>
            returnRegister: int 
        }
    
    //OPTIMIZATIONS:
    
    let (|IsMoveSafe|_|) (op:InstructionOperand)= 
        match op with 
        | Memory _ 
        | StringID _ -> None
        | _ -> Some op

    let (|Instr|_|) (instr:Instruction)= 
        match instr with 
        //| Move (a,IsMoveSafe b) 
        //| Move (IsMoveSafe a,b) -> Some (Move,a,b)
        | AddI (a,b) -> Some (AddI,a,b)
        | AddR (a,b) -> Some (AddR,a,b)
        | SubI (a,b) -> Some (SubI,a,b)
        | SubR (a,b) -> Some (SubR,a,b)
        | MulI (a,b) -> Some (MulI,a,b)
        | MulR (a,b) -> Some (MulR,a,b)
        | DivI (a,b) -> Some (DivI,a,b)
        | DivR (a,b) -> Some (DivR,a,b)
        | _ -> None

    let peepholeOptimize (code:Code) =
        let rec peepholeOptimize (code:List<Instruction>) cont = 
            match code with 
            | (Move (a,b) as m1) :: (Move (c,d) as m2) :: (Instr (_,e,IsMoveSafe f) as i) :: rest when (b = e) && (d = f) -> 
                peepholeOptimize (m2::m1::i::rest) cont
            | (Move (IsMoveSafe s,d) as m1) :: (Move (s', d') as i) :: rest  
            | (Move (s,d) as m1) :: (Move (s', IsMoveSafe d') as i) :: rest when d=s'-> 
                peepholeOptimize (Move (s,d')::rest) cont // need to fix Instr to check if something is one of several expressions
            | (Move (s,d) as m1) :: (Instr (c,s', d') as i) :: rest when d=s'-> 
                peepholeOptimize (c (s,d')::rest) cont // need to fix Instr to check if something is one of several expressions
            | instr::rest -> peepholeOptimize rest (fun rs -> instr::rs |> cont)
            | [] -> cont []
        peepholeOptimize (code |> List.ofSeq) Seq.ofList

    //CODE GEN

    let removeCommonElements (la:'a list) (lb:'a list) =
        let removeFirstMatching (x:'a) (l:'a List) =
            let rec loop lst cont =
                match lst with
                | [] -> cont []
                | e :: rest when e = x -> cont rest
                | h :: rest -> loop rest (fun rs -> h::rs |> cont)
            loop l id
        let rec loop (la:_ list) (lb:_ list) =
            let sa = la |> Set.ofList
            let sb = lb |> Set.ofList
            let commonSubset = Set.intersect sa sb
            if commonSubset.Count = 0 then la,lb
            else
                let la',lb' =
                    commonSubset
                    |> Seq.fold (fun (la,lb) element -> removeFirstMatching element la, removeFirstMatching element lb) (la,lb)
                loop la' lb'
        loop la lb

    let rec (|IntValue|_|) (expr:Expr) =
        match expr with
        | IntegerLiteral s -> System.Int32.Parse s |> Some
        | NegateExpr (IntValue i) -> -i |> Some
        | _ -> None
    let rec (|FloatValue|_|) (expr:Expr) =
        match expr with
        | FloatLiteral s -> System.Double.Parse s |> Some
        | NegateExpr (FloatValue f) -> -f |> Some
        | _ -> None
    let (|Zero|_|) (expr:Expr) =
        match expr with
        | IntValue 0 -> Some ()
        | FloatValue 0. -> Some()
        | _ -> None
    let (|One|_|) (expr:Expr) =
        match expr with
        | IntValue 1 -> Some ()
        | FloatValue 1.0 -> Some()
        | _ -> None


    let rec reduceExpr (expr:Expr) =
        match expr with
        | NegateExpr inner ->
            let inner' = reduceExpr inner
            match inner' with
            | NegateExpr expr -> expr // if the inner expression is now a Negate then we pass expr up since Negate(Negate ...) = ...
            | IntegerLiteral s -> IntegerLiteral (string (-(int s)))
            | FloatLiteral s -> FloatLiteral (string (-(float s)))
            | expr -> NegateExpr expr // if anything else then wrapped the fixed up expr in the NegateExpr container
        | ReciprocalExpr inner ->
            let inner' = reduceExpr inner
            match inner' with
            | ReciprocalExpr expr -> expr // if the inner expression is now a Negate then we pass expr up since 1/(1/a) = a
            // we are not going to do integer
            | FloatValue f -> FloatLiteral (string (1.0/f))
            | expr -> ReciprocalExpr expr
        | IntegerLiteral _
        | FloatLiteral _
        | Identifer _ -> expr
        | AddExpr (NegateExpr a, NegateExpr b) -> reduceExpr (AddExpr (a,b)) |> NegateExpr
        | MulExpr (NegateExpr a, NegateExpr b) -> reduceExpr (MulExpr (a,b))
        | MulExpr (NegateExpr a, b) -> reduceExpr (MulExpr (a,b)) |> NegateExpr
        | MulExpr (a, NegateExpr b) -> reduceExpr (MulExpr (a,b)) |> NegateExpr
        | AddExpr _ as addExpr ->
            let rec loop expr (literalExpr:Expr option,positives:Expr list,negatives:Expr list) =
                let addLiteral (litExpr:Expr option) (other:Expr) =
                    match litExpr, other with
                    | None, IntValue _ -> Some other
                    | None, FloatValue _ -> Some other
                    | Some (IntValue a), IntValue b ->
                        let c = a+b
                        if c >= 0 then IntegerLiteral(string c)
                        else NegateExpr(IntegerLiteral(string  (-c)))
                        |> Some
                    | Some (FloatValue a), FloatValue b ->
                        let c = a+b
                        if c >= 0. then FloatLiteral(string c)
                        else NegateExpr(FloatLiteral(string  (-c)))
                        |> Some
                    | _ -> failwithf "Can't Add %A to %A" litExpr other                    
                match expr with
                | AddExpr (lh,rh) ->
                    let state =
                        let lh' = reduceExpr lh
                        match lh' with
                        | Zero -> (literalExpr,positives,negatives)
                        | IntValue _
                        | FloatValue _ -> (addLiteral literalExpr lh'),positives,negatives
                        | NegateExpr inner -> literalExpr,positives,inner::negatives
                        | expr -> literalExpr,expr::positives,negatives
                    loop rh state
                | expr ->
                    let expr' = reduceExpr expr
                    match expr' with
                    | Zero -> (literalExpr,positives,negatives)
                    | IntValue _
                    | FloatValue _ -> (addLiteral literalExpr expr'),positives,negatives
                    | NegateExpr inner -> literalExpr,positives,inner::negatives
                    | expr -> literalExpr,expr::positives,negatives
            let literalExpr,positives,negatives = loop addExpr (None,[],[])
            // remove all the common sub expressions
            let positives,negatives =
                let p,n = removeCommonElements positives negatives
                // put list back in declaration order
                p |> List.rev, n |> List.rev
            let reduceExprListToAdd (exprs:Expr list) =
                match exprs with
                | [] -> failwithf "Shouldn't happen"
                | [z] -> z
                | exprs -> exprs |> Seq.reduce (fun lh rh -> AddExpr(lh,rh))
            let postiveExpr = if positives.Length = 0 then None else positives |> reduceExprListToAdd |> Some
            let negativeExpr = if negatives.Length = 0 then None else negatives |> reduceExprListToAdd |> NegateExpr |> Some
            match literalExpr, postiveExpr, negativeExpr with
            | None, None, None -> failwithf "No expressions in Add, shouldn't ever happen"
            | Some litExpr, None, None -> litExpr
            | Some Zero, Some postiveExpr, None
            | None, Some postiveExpr, None -> postiveExpr
            | Some Zero, None, Some negativeExpr
            | None, None, Some negativeExpr -> negativeExpr
            | Some literalExpr, Some postiveExpr, None -> AddExpr(literalExpr,postiveExpr)
            | Some literalExpr, None, Some negativeExpr -> AddExpr(literalExpr,negativeExpr)
            | None, Some postiveExpr, Some negativeExpr
            | Some Zero, Some postiveExpr, Some negativeExpr -> AddExpr(postiveExpr,negativeExpr)
            | Some literalExpr, Some postiveExpr, Some negativeExpr -> AddExpr(literalExpr,AddExpr(postiveExpr,negativeExpr))
        | MulExpr _ as mulExpr ->
            let rec loop expr (exprs:Expr list) =
                match expr with
                | MulExpr (lh,rh) ->
                    let lh' = reduceExpr lh
                    match lh' with
                    | Zero as z -> [z]
                    | One -> loop rh exprs
                    | NegateExpr One -> loop (NegateExpr rh) exprs
                    | expr -> loop rh (expr::exprs)
                | expr ->
                    let expr' = reduceExpr expr
                    match expr' with
                    | Zero as z -> [z]
                    | One -> exprs
                    | expr -> expr::exprs
            let exprs = loop mulExpr [] |> List.rev
            match exprs with
            | [] -> failwithf "Shouldn't Happen"
            | [z] -> z
            | exprs ->
                let negCount = exprs |> Seq.sumBy (function | NegateExpr _ -> 1 | _ -> 0)
                let simplifiedExpr =
                    exprs
                    |> Seq.choose (function | NegateExpr One -> None | NegateExpr inner -> Some inner | expr -> Some expr)
                    |> Seq.reduce (fun lh rh -> MulExpr(lh,rh))
                if negCount % 2 = 0 then
                    // we have a postive result
                    simplifiedExpr
                else
                    NegateExpr simplifiedExpr
        | expr -> expr

    let rec exprToCode (symTable:SymbolTable) (expr:Expr) regNum =
        match expr with 
        | IntegerLiteral i -> 
            let instr = Move (IntConstant (int i),Register regNum)
            CodeBlock.New [instr] (regNum+1) VarType.Int
        | FloatLiteral f -> 
            let instr = Move (FloatConstant (float f),Register regNum)
            CodeBlock.New [instr] (regNum+1) VarType.Float
        | Identifer n ->
            let exprType =
                match symTable.TryFind n with
                | None -> failwithf "Undefined reference %s encountered" n
                | Some entry ->
                    match entry with
                    | FunctionReference _ -> failwithf "using a function reference as variable reference: %s" n
                    | ArgumentVariable _
                    | StackVariable _ -> failwithf "Unsupported reference %s:%A" n entry
                    | GlobalVariable decl ->
                        match decl with
                        | String _ -> failwithf "Can not reference a string in an expression"
                        | Decl.Int _ -> VarType.Int
                        | Decl.Float _ -> VarType.Float
            let instr = Move (Memory n,Register regNum)
            CodeBlock.New [instr] (regNum+1) exprType
        | AddExpr (l,r) ->
            match l,r with
            | NegateExpr l, NegateExpr r ->
                let left = exprToCode symTable l regNum
                let right = exprToCode symTable r left.nextRegister
                if left.exprType = right.exprType then
                    let addInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> AddI args
                        | VarType.Float -> AddR args
                    let subInstr =
                        match left.exprType with
                        | VarType.Int -> MulI (InstructionOperand.IntConstant -1,Register regNum)
                        | VarType.Float -> MulR (InstructionOperand.FloatConstant -1.0,Register regNum)
                    let instrs =
                        left.instrs @ right.instrs @ [addInstr; subInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
            | d, NegateExpr s
            | NegateExpr s, d ->
                let left = exprToCode symTable d regNum
                let right = exprToCode symTable s left.nextRegister
                if left.exprType = right.exprType then
                    let subInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> SubI args
                        | VarType.Float -> SubR args
                    let instrs =
                        left.instrs @ right.instrs @ [subInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
            | l, r ->
                let left = exprToCode symTable l regNum
                let right = exprToCode symTable r left.nextRegister
                if left.exprType = right.exprType then
                    let addInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> AddI args
                        | VarType.Float -> AddR args
                    let instrs =
                        left.instrs @ right.instrs @ [addInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
        | MulExpr (l,r) ->
            match l, r with
            | ReciprocalExpr l, ReciprocalExpr r ->
                let left = exprToCode symTable l regNum
                let right = exprToCode symTable r left.nextRegister
                if left.exprType = right.exprType then
                    let mulInstr =
                        let args = Register regNum, Register left.nextRegister
                        match left.exprType with
                        | VarType.Int -> MulI args
                        | VarType.Float -> MulR args
                    let move1 =
                        match left.exprType with
                        | VarType.Int -> Move (InstructionOperand.IntConstant 1, Register regNum)
                        | VarType.Float -> DivR (InstructionOperand.FloatConstant 1.0, Register regNum)
                    let divInstr =
                        match left.exprType with
                        | VarType.Int -> DivI (Register left.nextRegister, Register regNum)
                        | VarType.Float -> DivR (Register left.nextRegister, Register regNum)
                    let instrs =
                        match left.exprType with
                        | VarType.Int -> [Move (InstructionOperand.IntConstant 0, Register regNum)]
                        | VarType.Float ->
                            left.instrs @ right.instrs @ [mulInstr; move1; divInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
            | d, ReciprocalExpr s
            | ReciprocalExpr s, d ->
                let left = exprToCode symTable d regNum
                let right = exprToCode symTable s left.nextRegister
                if left.exprType = right.exprType then
                    let divInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> DivI args
                        | VarType.Float -> DivR args
                    let instrs =
                        left.instrs @ right.instrs @ [divInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
            | l, r ->
                let left = exprToCode symTable l regNum
                let right = exprToCode symTable r left.nextRegister
                if left.exprType = right.exprType then
                    let mulInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> MulI args
                        | VarType.Float -> MulR args
                    let instrs =
                        left.instrs @ right.instrs @ [mulInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"                
        | NegateExpr inner ->
            let left = exprToCode symTable inner regNum
            let subInstr =
                match left.exprType with
                | VarType.Int -> MulI (InstructionOperand.IntConstant -1,Register regNum)
                | VarType.Float -> MulR (InstructionOperand.FloatConstant -1.0,Register regNum)
            let instrs =
                left.instrs @ [subInstr]
            CodeBlock.New instrs (regNum+1) left.exprType
        | ReciprocalExpr inner ->
            let left = exprToCode symTable inner (regNum+1)
            let move1 =
                match left.exprType with
                | VarType.Int -> Move (InstructionOperand.IntConstant 1, Register regNum)
                | VarType.Float -> Move (InstructionOperand.FloatConstant 1.0, Register regNum)
            let divInstr =
                match left.exprType with
                | VarType.Int -> DivI (Register (regNum+1), Register regNum)
                | VarType.Float -> DivR (Register (regNum+1), Register regNum)
            let instrs =
                left.instrs @ [move1; divInstr]
            CodeBlock.New instrs (regNum+1) left.exprType
        | notSupportedYet -> failwithf "%A not supported yet" notSupportedYet

    let genStmt (symTable:SymbolTable) (stmt:Stmt) regNum = 
        match stmt with 
        | Read ids -> 
            ids |> Seq.collect (fun id ->
            match (symTable.TryFind id) with 
            | None -> failwithf "Symbol %s not found" id 
            | Some decl -> 
                match decl with 
                | GlobalVariable var -> 
                    match var with 
                    | Decl.Int id-> [SysReadI (Memory id)]
                    | Decl.Float id-> [SysReadR (Memory id)]
                    | Decl.String (id,_)-> failwithf "Unable to read the type of %s" id
                | _ -> failwithf "Only Global variables are accepted"
            )
        | Write ids -> 
            ids |> Seq.collect (fun id ->
            match (symTable.TryFind id) with 
            | None -> failwithf "Symbol %s not found" id 
            | Some decl -> 
                match decl with 
                | GlobalVariable var -> 
                    match var with 
                    | Decl.Int id-> [SysWriteI (Memory id)]
                    | Decl.Float id-> [SysWriteR (Memory id)]
                    | Decl.String (id,_)-> [SysWriteS (Memory id)]
                | _ -> failwithf "Only Global variables are accepted"            
            )
        | Assign (lh,rh) -> 
            match symTable.TryFind lh with 
            | None -> failwithf "Symbol %s not found" lh 
            | Some decl -> 
                match decl with 
                | GlobalVariable id -> 
                    Seq.concat [(exprToCode symTable (reduceExpr rh) regNum).instrs;[Move ((Register regNum),(Memory lh))]] 
                    |> peepholeOptimize
                | _ -> failwithf "Only Global variables are accepted"
        | _ -> failwithf "Unsupported Operation %A" stmt

    let genFuncDeclInstrs (symTable:SymbolTable) (func:FuncDecl) = 
        seq {
            yield!
                func.stmts
                |> Seq.collect (fun x -> genStmt symTable x 0)
            yield SysHalt
        }

    let genProgram (prog:Program) = 
        let globalDecls = 
            prog.decls
            |> Seq.map (fun decl ->
                match decl with 
                | String (id,lit) -> Str (Memory id,lit)
                | Decl.Int i -> Var i 
                | Decl.Float f -> Var f
            )
        let mainFn = 
            prog.functions |> List.tryFind (fun f -> f.name = "main") |> (fun d -> if d.IsNone then failwithf "No Main Function Found!" else d.Value)
        match prog.symbolTable with 
        | Some symTbl -> seq {globalDecls; genFuncDeclInstrs symTbl mainFn} |> Seq.concat
        | None -> failwithf "Symbol Table Failed to generate!"


    