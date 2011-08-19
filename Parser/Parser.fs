module public Parser
open System

//------------------------------------------------------------------------------------------------------------

type Date = { Day: uint16; Month: uint16; Year: uint16 } 

//------------------------------------------------------------------------------------------------------------

type Time = { Hours: uint16; Minutes: uint16; Seconds: uint16; FSeconds: uint16 }

//------------------------------------------------------------------------------------------------------------

type 'a VM = 
    | Single of 'a
    | Multi of 'a list

//------------------------------------------------------------------------------------------------------------

type VR =
    | AE of VM<string> Option
    | AS of VM<string> Option
    | AT of VM<string> Option
    | CS of VM<string> Option
    | DA of VM<Date> Option
    | DS of VM<string> Option
    | DT of VM<Date * Time> Option
    | FL of VM<float> Option
    | FD of VM<double> Option
    | IS of VM<int> Option
    | LO of VM<string> Option
    | LT of VM<string> Option
    | OB of string Option
    | OF of string Option
    | OW of byte[] Option
    | PN of VM<string> Option
    | SH of VM<string> Option
    | SL of VM<string> Option
    | SQ of Map<uint32, VR> list Option
    | SS of VM<int16> Option
    | ST of VM<string> Option
    | TM of VM<Time> Option
    | UI of VM<string> Option
    | UL of VM<uint32> Option
    | UN of string Option
    | US of VM<uint16> Option
    | UT of VM<string> Option

//------------------------------------------------------------------------------------------------------------

type DicomObject = { Preamble : byte[]; Values : Map<uint32, VR> }

//------------------------------------------------------------------------------------------------------------

let private to_date (s : string) = 
    {Day = UInt16.Parse(s.[5..6]); Month = UInt16.Parse(s.[4..5]); Year = UInt16.Parse(s.[0..3])}

//------------------------------------------------------------------------------------------------------------

let private to_time (s : string) = 
    {Hours = UInt16.Parse(s.[0..1]); Minutes = UInt16.Parse(s.[2..3]); Seconds = UInt16.Parse(s.[4..5]); FSeconds = uint16(1)}

//------------------------------------------------------------------------------------------------------------

let private to_date_time (s : string) = 
    let parts = s.Split([|' '|])
    let date = to_date parts.[0]
    let time = to_time parts.[1]
    (date, time)

//------------------------------------------------------------------------------------------------------------

let private split_string (value : string) =
    if value.IndexOf('\\') = -1
    then Single value
    else 
        let rec string_splitter acc (s : string) =
            let p = s.IndexOf("\\")
            if p = -1
            then s::acc
            else string_splitter (s.Substring(0, p)::acc) (s.Substring(p + 1)) 
        Multi (string_splitter [] value)
            
//------------------------------------------------------------------------------------------------------------

let private string_parser (a : byte[]) = 
    if a.Length = 0
    then None
    else Some (Utils.decode_string a |> split_string)

//------------------------------------------------------------------------------------------------------------        

let private string_parser2 (a : byte[]) f = 
    if a.Length = 0
    then None
    else 
        Utils.decode_string a
        |> split_string
        |> function
            | Single x -> Single (f x)
            | Multi x -> Multi (List.map f x)
        |> Some

//------------------------------------------------------------------------------------------------------------

let private binary_parser (a : byte[]) size f =
    let rec split_bytes acc (values : byte[]) =
        if values.Length < size
        then acc
        else
            let x = values.[0..size-1]
            split_bytes (x::acc) (values.[size..values.Length - 1])

    match a.Length with
    | 0 -> None
    | x when x = size -> Some (Single(f a))
    | _ -> Some (Multi(List.map f (split_bytes [] a)))

//------------------------------------------------------------------------------------------------------------

let parse (preamble, elements) = 
    let parse_simple_element value = function
        | Lexer.VR.AE -> string_parser value |> AE
        | Lexer.VR.AS -> string_parser value |> AS
        | Lexer.VR.AT -> string_parser value |> AT
        | Lexer.VR.CS -> string_parser value |> CS
        | Lexer.VR.DA -> string_parser2 value to_date |> DA
        | Lexer.VR.DS -> string_parser value |> DS
        | Lexer.VR.DT -> string_parser2 value to_date_time |> DT
        | Lexer.VR.FD -> binary_parser value 4 (fun x -> BitConverter.ToDouble(x, 0)) |> FD
        | Lexer.VR.FL -> binary_parser value 4 (fun x -> BitConverter.ToDouble(x, 0)) |> FL
        | Lexer.VR.IS -> string_parser2 value (fun x -> Int32.Parse x) |> IS
        | Lexer.VR.LO -> string_parser value |> LO
        | Lexer.VR.LT -> string_parser value |> LT
        | Lexer.VR.OB -> value |> Utils.decode_string |> Some |> OB
        | Lexer.VR.OF -> value |> Utils.decode_string |> Some |> OF
        | Lexer.VR.OW -> value |> Some |> OW
        | Lexer.VR.PN -> string_parser value |> PN
        | Lexer.VR.SH -> string_parser value |> SH
        | Lexer.VR.SL -> string_parser value |> SL
        | Lexer.VR.SS -> binary_parser value 2 (fun x -> BitConverter.ToInt16(x,0)) |> SS
        | Lexer.VR.ST -> string_parser value |> ST
        | Lexer.VR.TM -> string_parser2 value to_time |> TM
        | Lexer.VR.UI -> string_parser value |> UI
        | Lexer.VR.UL -> binary_parser value 4 (fun x -> BitConverter.ToUInt32(x, 0)) |> UL
        | Lexer.VR.UN -> value |> Utils.decode_string |> Some |> UN
        | Lexer.VR.US -> binary_parser value 2 (fun x -> BitConverter.ToUInt16(x,0)) |> US
        | Lexer.VR.UT -> string_parser value |> UT
        | _ -> failwith "Unknown Simple VR"
    
    let rec converter = function
        | Lexer.Simple (tag, vr, value) -> (tag, parse_simple_element value vr)
        | Lexer.Complex (tag, elements) -> (tag, List.map create_vr_map elements |> Some |> SQ)
    and create_vr_map elements = new Map<uint32, VR>(List.map converter elements)
    
    { Preamble = preamble; Values = create_vr_map elements }