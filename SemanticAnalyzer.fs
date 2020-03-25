namespace micro

open System
open AST
#nowarn "40"

module rec SemanticAnalyzer =
    let rec fixupStmts symTable blockIdx stmts cont =
        match stmts with
        | [] -> Result.Ok (cont [],blockIdx)
        | stmt :: rest ->
            match stmt with
            | If (cond,trueBlock,None) -> 
                match populateBlockSymbolTables blockIdx symTable trueBlock with
                | Error msg -> Error msg
                | Ok (trueBlock',nextBlockId) ->
                    fixupStmts symTable nextBlockId rest (fun rs -> If(cond,trueBlock',None)::rs |> cont)
            | If (cond,trueBlock,Some elseBlock) -> 
                match populateBlockSymbolTables blockIdx symTable trueBlock with
                | Error msg -> Error msg
                | Ok (trueBlock',nextBlockId) ->
                    match populateBlockSymbolTables nextBlockId symTable elseBlock with
                    | Error msg -> Error msg
                    | Ok (elseBlock',nextBlockId) ->
                        fixupStmts symTable nextBlockId rest (fun rs -> If(cond,trueBlock',Some elseBlock')::rs |> cont)
            | While (cond,block) -> 
                match populateBlockSymbolTables blockIdx symTable block with
                | Error msg -> Error msg
                | Ok (block',nextBlockId) ->
                    fixupStmts symTable nextBlockId rest (fun rs -> While(cond,block')::rs |> cont)
            | stmt ->
                fixupStmts symTable blockIdx rest (fun rs -> stmt::rs |> cont)

    let rec populateBlockSymbolTables (blockIdx:int) (parent:SymbolTable) (b:Block): Result<Block*int,_> = 
        let blockSymbolTable = SymbolTable.Create (sprintf "%d" blockIdx,parent)

        let rec insertDecls decls =
            match decls with
            | [] -> Result.Ok ()
            | d :: rest ->
                let id = (d:Decl).Id
                if blockSymbolTable.IsLocallyDefined id then
                    Result.Error (sprintf "DECLARATION ERROR %s" id)
                else
                    let entry = StackVariable d
                    blockSymbolTable.addSymbol id entry
                    insertDecls rest

        match insertDecls b.decls with 
        | Error msg -> Error msg 
        | Ok _ ->
            match fixupStmts blockSymbolTable (blockIdx+1) b.stmts id with // move the blockId to the next number
            | Error msg -> Error msg
            | Ok (stmt',nextBlockId) ->
                Ok ({b with symbolTable = Some blockSymbolTable; stmts = stmt'},nextBlockId)
        
    let populateFunctionSymbolTables startingBlockIdx (parent:SymbolTable) (f:FuncDecl) : Result<FuncDecl*int,_> = 
        let funcSymbolTable = SymbolTable.Create (f.name, parent)
            
        let rec insertParamDecls decls =
            match decls with
            | [] -> Result.Ok ()
            | d :: rest ->
                let id = (d:ParamDecl).Id
                if funcSymbolTable.IsLocallyDefined id then
                    Result.Error (sprintf "DECLARATION ERROR %s" id)
                else
                    let entry = ArgumentVariable d
                    funcSymbolTable.addSymbol id entry
                    insertParamDecls rest                    
        let rec insertDecls decls =
            match decls with
            | [] -> Result.Ok ()
            | d :: rest ->
                let id = (d:Decl).Id
                if funcSymbolTable.IsLocallyDefined id then
                    Result.Error (sprintf "DECLARATION ERROR %s" id)
                else
                    let entry = StackVariable d
                    funcSymbolTable.addSymbol id entry
                    insertDecls rest
        match insertParamDecls f.parameters with
        | Error msg -> Error msg
        | Ok _ ->
            match insertDecls f.decls with
            | Error msg -> Error msg
            | Ok _ ->
                match fixupStmts funcSymbolTable startingBlockIdx f.stmts id with
                | Error msg -> Error msg
                | Ok (stmts',nextBlockId) ->
                    Result.Ok ({f with symbolTable = Some funcSymbolTable; stmts = stmts'},nextBlockId)
        
    let populateProgramSymbolTables (p:Program) = 
        let globalSymbolTable = SymbolTable.Create "GLOBAL"

        let rec insertDecls decls =
            match decls with
            | [] -> Result.Ok ()
            | d :: rest ->
                let id = (d:Decl).Id
                if globalSymbolTable.IsLocallyDefined id then
                    Result.Error (sprintf "DECLARATION ERROR %s" id)
                else
                    let entry = GlobalVariable d
                    globalSymbolTable.addSymbol id entry
                    insertDecls rest
        let rec insertFuncDecls startingBlockId funcs cont =
            match funcs with
            | [] -> Result.Ok (cont [])
            | f :: rest ->
                let id = (f:FuncDecl).name
                if globalSymbolTable.IsLocallyDefined id then
                    Result.Error (sprintf "DECLARATION ERROR %s" id)
                else
                    let tempEntry = FunctionReference f
                    globalSymbolTable.addSymbol id tempEntry // we want the entry in case we reference this inside this function
                    match populateFunctionSymbolTables startingBlockId globalSymbolTable f with
                    | Error msg -> Error msg
                    | Ok (f',nextBlockId) ->
                        // now we have the updated FuncDecl we replace the old entry
                        let entry = FunctionReference f'
                        globalSymbolTable.addSymbol id entry // we want to new replace the temporary entry with the correct one
                        insertFuncDecls nextBlockId rest (fun rs -> f' :: rs |> cont) // then examine the next function and add this fixed up on to the replacement functionDecls list
        match insertDecls p.decls with
        | Error msg -> Error msg
        | Ok _ ->
            match insertFuncDecls 1 p.functions id with
            | Error msg -> Error msg
            | Ok functions' ->
                Ok {p with symbolTable = Some globalSymbolTable; functions = functions'}
