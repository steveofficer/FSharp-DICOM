// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.
#load "Utils.fs"
#load "Tags.fs"
#load "Lexer.fs"
#load "Parser.fs"
    
//let data_stream = new System.IO.StreamReader(@"C:\Users\stof\Dropbox\DICOM\samples\BEEVR\brain_001.dcm")
//let data_stream = new System.IO.StreamReader(@"C:\Users\stof\Dropbox\DICOM\samples\LEEVR\brain_001.dcm")
let data_stream = new System.IO.StreamReader(@"C:\Dropbox\DICOM\samples\BEEVR\brain_001.dcm")

let timer = System.Diagnostics.Stopwatch()
timer.Start()
let result = Lexer.read data_stream.BaseStream (fun x -> Lexer.Failure "Doesn't exist")
timer.Stop()
printfn "%A" timer.Elapsed.TotalMilliseconds

let parsed_result = 
    match result with
        | Lexer.Result.Success (preamble, dataset) -> 
            Parser.parse(preamble, dataset) |> ignore
            printfn "Parsed"
        | Lexer.Result.Failure reason -> printfn "%A" reason