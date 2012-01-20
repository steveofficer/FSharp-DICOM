let read_result (stream : System.IO.StreamReader) = 
    let result = Lexer.read stream.BaseStream (fun x -> Lexer.Failure (sprintf "Tag %i is not recognized" x))
    stream.Dispose()
    result

let parse = 
    function
        | Lexer.Success (preamble, data_set) -> Parser.parse(preamble, data_set)
        | _ -> failwith "Oops failure"
        
let files = System.IO.Directory.GetFiles(@"D:\NewData")
let streams = Array.map (fun (x : string) -> new System.IO.StreamReader(x)) files
let timer = System.Diagnostics.Stopwatch()
timer.Start()
let results = Array.map read_result streams
let parsed = Array.map parse results
timer.Stop()

//Array.iter (printfn "%A") results
//Array.iter (printfn "%A") parsed
printfn "%i" results.Length
printfn "%i" parsed.Length
printfn "%A" timer.Elapsed.TotalMilliseconds