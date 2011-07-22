// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.
#load "Tags.fs"
#load "Lexer.fs"
#load "Parser.fs"
    
let data_stream = new System.IO.StreamReader(@"C:\Users\stof\Dropbox\DICOM\samples\brain_001.dcm")

let result = Lexer.read data_stream.BaseStream (fun x -> (false, false)) Map.empty

let parsed_result = 
    match result with
        | Lexer.Result.Success (preamble, dataset) -> Parser.parse(preamble, dataset)
        | Lexer.Result.Failure reason -> failwith reason