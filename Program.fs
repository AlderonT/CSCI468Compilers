namespace micro 

open System
#nowarn "40"
module Program =      
    open Parser
    open Grammar
    open SemanticAnalyzer
    open TinyCode


    let printUsage () = 
        printfn "Usage: micro [-h|--help] [-d|--display] <file_name>"
        printfn "     -h|--help   :   displays usage"
        printfn "     -d|--display:   displays the abstract syntax tree returned by the parser"
        printfn "     file_name   :   .micro source file to be processed"
    
    type Options =
        {
            displayHelp : bool
            errorMsg : string
            displayParseTree: bool
            filename : string
        }

    let processArgs (args:string[]) =
        args
        |> Seq.mapi (fun i x -> i,x)
        |> Seq.fold (fun options (i,arg) ->
            match arg.ToLowerInvariant() with
            | "-h" | "--help" ->  { options with displayHelp = true}
            | "-d" | "--display" -> {options with displayParseTree = true}
            | x when i = args.Length-1 -> {options with filename = x}
            | x -> {options with displayHelp = true; errorMsg = sprintf "Unknown option '%s'" x}
        ) { displayHelp = args.Length = 0; displayParseTree = false; filename = null; errorMsg = null }

    [<EntryPoint>]
    let main argv =
        let options = processArgs argv
        if options.errorMsg <> null then printfn "%s" options.errorMsg
        if options.displayHelp then 
            printUsage()
        else 
            let filename = System.IO.Path.ChangeExtension(options.filename,"tiny")
            let file = System.IO.File.ReadAllText options.filename
            let program = file |> fromStr |> run program
            match program with
            | Success (result,_) ->
                //printProgram result
                //printfn "Accepted"
                if options.displayParseTree then //display the parse tree
                    printParseTree result
                match SemanticAnalyzer.populateProgramSymbolTables result with
                | Error msg -> printfn "%s" msg
                | Ok newProgram -> genProgram newProgram |> concatInstructions |>  writeCodeToConsole //writeCodeToFile filename 
            | Failure _ as result ->
                //printfn "%A" program
                printfn "Not accepted"
                if options.displayParseTree then
                    printResult result //Print Result is currently broken 
                                       //(showing the top-level element containing an error) keep in mind
        0 // return an integer exit code

        //let a = 3
        //let b = 30 
        //let c = -33 
        //let d = -20 
        //printfn "%d" ((a+b)*(d+c)-(a+b+c+d)/a)