module public Parser
open System
open System.Linq
open System.Text.RegularExpressions

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

let private to_string data = Text.ASCIIEncoding.UTF8.GetString(data).Trim([|char(0); ' '|])

let private to_date (s : string) = 
    {Day = UInt16.Parse(s.[5..6]); Month = UInt16.Parse(s.[4..5]); Year = UInt16.Parse(s.[0..3])}

let private to_time (s : string) = 
    {Hours = UInt16.Parse(s.[0..1]); Minutes = UInt16.Parse(s.[2..3]); Seconds = UInt16.Parse(s.[4..5]); FSeconds = uint16(1)}

let private to_date_time (s : string) = 
    let parts = s.Split([|' '|])
    let date = to_date parts.[0]
    let time = to_time parts.[1]
    (date, time)

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
            
let private string_parser (a : byte[]) = 
    let value = 
        if a.Length = 0
        then None
        else Some (to_string a |> split_string)
    value
        
let private string_parser2 (a : byte[]) f = 
    let value = 
        if a.Length = 0
        then None
        else 
            to_string a
            |> split_string
            |> function
                | Single x -> Single (f x)
                | Multi x -> Multi (List.map f x)
            |> Some
    value

let private binary_parser (a : byte[]) size f =
    let rec split_bytes acc (values : byte[]) =
        if values.Length < size
        then acc
        else
            let x = values.[0..size-1]
            split_bytes (x::acc) (values.[size..values.Length - 1])

    let value = 
        match a.Length with
        | 0 -> None
        | x when x = size -> Some (Single(f a))
        | _ -> Some (Multi(List.map f (split_bytes [] a)))
                
    value

let parse (preamble, elements) = 
    let parse_simple_element value = function
        | Lexer.Type.AE -> string_parser value |> AE
        | Lexer.Type.AS -> string_parser value |> AS
        | Lexer.Type.AT -> string_parser value |> AT
        | Lexer.Type.CS -> string_parser value |> CS
        | Lexer.Type.DA -> string_parser2 value to_date |> DA
        | Lexer.Type.DS -> string_parser value |> DS
        | Lexer.Type.DT -> string_parser2 value to_date_time |> DT
        | Lexer.Type.FD -> binary_parser value 4 (fun x -> BitConverter.ToDouble(x, 0)) |> FD
        | Lexer.Type.FL -> binary_parser value 4 (fun x -> BitConverter.ToDouble(x, 0)) |> FL
        | Lexer.Type.IS -> string_parser2 value (fun x -> Int32.Parse x) |> IS
        | Lexer.Type.LO -> string_parser value |> LO
        | Lexer.Type.LT -> string_parser value |> LT
        | Lexer.Type.OB -> value |> to_string |> Some |> OB
        | Lexer.Type.OF -> value |> to_string |> Some |> OF
        | Lexer.Type.OW -> value |> Some |> OW
        | Lexer.Type.PN -> string_parser value |> PN
        | Lexer.Type.SH -> string_parser value |> SH
        | Lexer.Type.SL -> string_parser value |> SL
        | Lexer.Type.SS -> binary_parser value 2 (fun x -> BitConverter.ToInt16(x,0)) |> SS
        | Lexer.Type.ST -> string_parser value |> ST
        | Lexer.Type.TM -> string_parser2 value to_time |> TM
        | Lexer.Type.UI -> string_parser value |> UI
        | Lexer.Type.UL -> binary_parser value 4 (fun x -> BitConverter.ToUInt32(x, 0)) |> UL
        | Lexer.Type.UN -> value |> to_string |> Some |> UN
        | Lexer.Type.US -> binary_parser value 2 (fun x -> BitConverter.ToUInt16(x,0)) |> US
        | Lexer.Type.UT -> string_parser value |> UT
        | _ -> failwith "Unknown Simple VR"
    
    let rec converter = function
        | Lexer.Simple (tag, vr, value) -> (tag, parse_simple_element value vr)
        | Lexer.Complex (tag, elements) -> (tag, List.map create_vr_map elements |> Some |> SQ)
    and create_vr_map elements = new Map<uint32, VR>(List.map converter elements)
    
    { Preamble = preamble; Values = create_vr_map elements }