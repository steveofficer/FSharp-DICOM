open System.IO

let parse = 
    function
        | Lexer.Data (preamble, data_set) -> Parser.parse(preamble, data_set)
        | Lexer.Error reason -> failwithf "Failed: %s" reason

let read_file (file_path : string) = async {
    let tag_dictionary tag = Lexer.Error (sprintf "Tag %i is not recognized" tag)
    use stream = new StreamReader(file_path)
    return Lexer.Read stream.BaseStream tag_dictionary |> parse
}

[<EntryPoint>]
let main (args : string[]) =   
    let files = Directory.GetFiles(args.[0])
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let parsed = Array.map read_file files 
                |> Async.Parallel 
                |> Async.RunSynchronously
    timer.Stop()

    printfn "Parsed : %i files" parsed.Length
    printfn "Took %A ms in total" timer.Elapsed.TotalMilliseconds

    //parsed.[0].Values |> Map.iter (fun k v -> printfn "%A %A" k v)
    let ``patient orientation`` = 2097184ul
    let ``image position (patient)`` = 2097202ul
    let ``image orientation (patient)`` = 2097207ul
    parsed.[0].Values.[``image orientation (patient)``] |> printfn "%A"
    
    
    
    0
    