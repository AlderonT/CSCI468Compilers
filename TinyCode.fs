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

    let writeCodeToFile (code:string)  = 
        System.IO.File.WriteAllText("D:\Spring2020\SmallC\\tinyTest.tiny",code)

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
        | AddExpr (op,l,r) ->
            match op with
            | Add ->
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
            | Sub ->
                // we re-order subtraction so that we can minimize the number of used registers
                let left = exprToCode symTable r regNum
                let right = exprToCode symTable l left.nextRegister
                if left.exprType = right.exprType then
                    let addInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> SubI args
                        | VarType.Float -> SubR args
                    let instrs =
                        left.instrs @ right.instrs @ [addInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"
        | MulExpr (op,l,r) ->
            match op with
            | Mul ->
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
            | Div ->
                // we re-order subtraction so that we can minimize the number of used registers
                let left = exprToCode symTable r regNum
                let right = exprToCode symTable l left.nextRegister
                if left.exprType = right.exprType then
                    let mulInstr =
                        let args = Register left.nextRegister, Register regNum
                        match left.exprType with
                        | VarType.Int -> DivI args
                        | VarType.Float -> DivR args
                    let instrs =
                        left.instrs @ right.instrs @ [mulInstr]
                    CodeBlock.New instrs (regNum+1) left.exprType
                else
                    failwithf "Attempting to mix types in a math expression"
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
                    Seq.concat [(exprToCode symTable rh regNum).instrs;[Move ((Register regNum),(Memory lh))]]
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

