namespace micro

open System
#nowarn "40"

module rec AST = 
    //////////////////
    // SYMBOL TABLE
    //////////////////

    type SymbolTableEntry =
        | GlobalVariable of Decl
        | ArgumentVariable of ParamDecl
        | StackVariable of Decl
        | FunctionReference of FuncDecl
    type SymbolTable = 
        {
            name: string
            parent: SymbolTable option
            mutable symbols: list<string*SymbolTableEntry>
        }
        with
            static member Create name = 
                {
                    name = name 
                    parent = None 
                    symbols = List.empty
                }
            static member Create (name,parent)= 
                {
                    name = name 
                    parent = Some parent
                    symbols = List.empty
                }
            member this.IsDefined name =
                if this.IsLocallyDefined name then true
                else
                    match this.parent with
                    | None -> false
                    | Some p -> p.IsDefined name
            member this.IsLocallyDefined name =
                this.symbols |>List.exists (fun (n,_) -> n = name)
            member this.addSymbol name entry = 
                this.symbols <- (name,entry) :: this.symbols
            
            ////////////////////////////
            // PRINT OUR SYMBOL TABLE //
            ////////////////////////////
            
            member this.print () =                                                                  
                printfn "Symbol table %s" this.name 
                
                this.symbols |> List.rev |> List.iter (fun (s,ste) -> 
                    match ste with  
                    | GlobalVariable (String (id,v)) -> printfn "name %s type STRING value %A" s v 
                    | GlobalVariable (Decl.Int id) -> printfn "name %s type INT" s  
                    | GlobalVariable (Decl.Float id) -> printfn "name %s type FLOAT" s  
                    | ArgumentVariable (Int id) -> printfn "name %s type INT" s  
                    | ArgumentVariable (Float id) -> printfn "name %s type FLOAT" s  
                    | StackVariable (String (id,v)) -> printfn "name %s type STRING value %A" s v 
                    | StackVariable (Decl.Int id) -> printfn "name %s type INT" s  
                    | StackVariable (Decl.Float id) -> printfn "name %s type FLOAT" s  
                    | FunctionReference f -> 
                        match f.symbolTable with 
                        |None -> ()
                        |Some st -> 
                            printfn ""
                            st.print()
                            let rec loop stmts = 
                                match stmts with 
                                | [] -> () 
                                | stmt'::rest -> 
                                    match stmt' with 
                                    | If (_,tb,None) -> 
                                        match tb.symbolTable with 
                                        |None -> ()
                                        |Some st -> 
                                            st.print()
                                            loop tb.stmts
                                    | If (_,tb,Some eb) -> 
                                        match tb.symbolTable with 
                                        |None -> ()
                                        |Some st -> 
                                            st.print()
                                            loop tb.stmts
                                        match eb.symbolTable with 
                                        |None -> ()
                                        |Some st -> 
                                            st.print()
                                            loop eb.stmts
                                    |While (_,wb) -> 
                                        match wb.symbolTable with 
                                        |None -> ()
                                        |Some st -> 
                                            st.print()
                                            loop wb.stmts
                                    | _ -> ()
                                    loop rest 
                            loop f.stmts
                ) 
                printfn ""

    ///////////////////
    // AST
    ///////////////////

    type Literal =
        | StringLiteral of string
        | IntegerLiteral of string
        | FloatLiteral of string

    type VarType = | Float | Int
    type ReturnType = | Void | Float | Int
    type Decl =
        | String of id:string*literal:string
        | Float of id: string
        | Int of id: string
        with
            member this.Id = match this with | String (id,_) -> id | Int id -> id | Float id -> id

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
            symbolTable: SymbolTable option
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
        with
            member this.Id = match this with | Int id -> id | Float id -> id

    type FuncDecl =
        {
            returnType: ReturnType
            name: string
            parameters: ParamDecl list
            decls: Decl list
            stmts: Stmt list
            symbolTable: SymbolTable option
        }

    type Program =
        {
            programName: string
            decls: Decl list 
            functions: FuncDecl list 
            symbolTable: SymbolTable option
        }    

    type AST = 
        | Decl of Decl list
        
