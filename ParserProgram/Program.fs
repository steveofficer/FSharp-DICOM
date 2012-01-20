open System.IO

let parse = 
    function
        | Lexer.Success (preamble, data_set) -> Parser.parse(preamble, data_set)
        | Lexer.Failure reason -> failwith (sprintf "Failed: %s" reason)

let read_file (file_path : string) = async {
    use stream = new StreamReader(file_path)
    return Lexer.read stream.BaseStream (fun x -> Lexer.Failure (sprintf "Tag %i is not recognized" x)) |> parse
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
    0