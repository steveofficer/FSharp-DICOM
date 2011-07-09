// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.
#load "Tags.fs"
#load "Lexer.fs"
#load "Parser.fs"
    
let data = System.IO.File.ReadAllBytes(@"C:\Dropbox\DICOM\samples\brain_001.dcm")

#time
let result = Lexer.read data (fun x -> (false, false)) Map.empty

let parsed_result = 
    match result with
        | Lexer.Result.Success (preamble, dataset) -> 
            printfn "%s" "parsing"
            Parser.parse(preamble, dataset)
        | Lexer.Result.Failure reason -> failwith reason