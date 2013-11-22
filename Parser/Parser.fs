module public Parser
    open System

    //--------------------------------------------------------------------------------------------------------

    type Date = { Day: uint16; Month: uint16; Year: uint16 } 

    //--------------------------------------------------------------------------------------------------------

    type Time = { Hours: uint16; Minutes: uint16; Seconds: uint16; FSeconds: uint16 }

    //--------------------------------------------------------------------------------------------------------

    type 'a VM = 
        | Single of 'a
        | Multi of 'a list

    //--------------------------------------------------------------------------------------------------------

    type VR =
        | AE of VM<string> Option
        | AS of VM<string> Option
        | AT of VM<string> Option
        | CS of VM<string> Option
        | DA of VM<Date> Option
        | DS of VM<string> Option // this should actually be float or double or something
        | DT of VM<Date * Time> Option
        | FL of VM<single> Option
        | FD of VM<double> Option
        | IS of VM<int> Option
        | LO of VM<string> Option
        | LT of string Option
        | OB of string Option
        | OF of string Option
        | OW of byte[] Option
        | PN of VM<string> Option
        | SH of VM<string> Option
        | SL of VM<string> Option
        | SQ of Map<uint32, VR> list Option
        | SS of VM<int16> Option
        | ST of string Option
        | TM of VM<Time> Option
        | UI of VM<string> Option
        | UL of VM<uint32> Option
        | UN of byte[] Option
        | US of VM<uint16> Option
        | UT of VM<string> Option

    //--------------------------------------------------------------------------------------------------------

    type OpBuilder() =
        member this.Bind(a, f) =
            match a with
            | Some value -> f value
            | None -> None
        member this.Return(a) = Some a

    let op = OpBuilder()

    //--------------------------------------------------------------------------------------------------------

    type DicomObject = { Preamble : byte[]; Values : Map<uint32, VR> }

    //--------------------------------------------------------------------------------------------------------

    let private ``read as date`` (s : string) = 
        {Day = UInt16.Parse(s.[5..6]); Month = UInt16.Parse(s.[4..5]); Year = UInt16.Parse(s.[0..3])}

    //--------------------------------------------------------------------------------------------------------

    let private ``read as time`` (s : string) = 
        // The time may be in one of various formats. Need to add the format checking code
        let hours = UInt16.Parse(s.[0..1])
        let minutes = UInt16.Parse(s.[2..3])
        let seconds = UInt16.Parse(s.[4..5])
        let fractional = uint16(1)
        { Hours = hours; Minutes = minutes; Seconds = seconds; FSeconds = fractional }

    //--------------------------------------------------------------------------------------------------------

    let private ``read as datetime`` (s : string) = 
        // Need to check the format of a DT object. We can't just to read Date, read Time because it allows
        // a more general syntax (only Year, only Year and Month, only Year, Month and Day and so on getting
        // more and more precise)
        let parts = s.Split([|' '|])
        let date = ``read as date`` parts.[0]
        let time = ``read as time`` parts.[1]
        (date, time)

    //--------------------------------------------------------------------------------------------------------

    let private ``parse string`` (a : byte[]) = 
        // We should check the encoding attribute (if it exists) and then use that to decode the string.
        // We might also need 2 ``parse string`` functions, because not all string decoding is subject to the
        // encoding attribute (dates, integers, decimals etc. aren't, LO, ST, UT etc. are)
        match a.Length with
        | 0 -> None
        | _ -> Some (Utils.decode_string a)

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued string`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        return (
            let values = string_value.Split([|'\\'|]) |> Array.toList
            match values with
            | h::[] -> Single h
            | _ -> Multi values
        )
    }

    //--------------------------------------------------------------------------------------------------------        

    let private ``parse multi valued date`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        let values = string_value.Split([|'\\'|]) |> Array.toList
        let dates = List.map ``read as date`` values
        return (
            match dates with
            | h::[] -> Single h
            | x -> Multi x
        )
    }
    
    //--------------------------------------------------------------------------------------------------------        

    let private ``parse multi valued decimal`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        let values = string_value.Split([|'\\'|])
        let decimals = Array.map id values // id should actually be a decimal parsing function
        return (
            match decimals.Length with
            | 1 -> Single decimals.[0]
            | _ -> decimals |> Array.toList |> Multi
        )
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued datetime`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        let values = string_value.Split([|'\\'|])
        let datetimes = Array.map ``read as datetime`` values
        return (
            match datetimes.Length with
            | 1 -> Single datetimes.[0]
            | _ -> datetimes |> Array.toList |> Multi
        )
    }

    //--------------------------------------------------------------------------------------------------------

    let private binary_parser (a : byte[]) chunk_size f =
        let rec read_bytes acc (values : byte[]) =
            if values.Length < chunk_size
            then acc
            else
                let item = f values.[0..chunk_size-1]
                read_bytes (item::acc) (values.[chunk_size..values.Length - 1])

        match a.Length with
        | 0 -> None
        | x when x = chunk_size -> f a |> Single |> Some
        | _ -> read_bytes [] a |> Multi |> Some

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued integer string`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        let values = string_value.Split([|'\\'|])
        let integers = Array.map System.Int32.Parse values
        return (
            match integers.Length with
            | 1 -> Single integers.[0]
            | _ -> integers |> Array.toList |> Multi
        )
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued time`` (a : byte[]) = op {
        let! string_value = ``parse string`` a
        let values = string_value.Split([|'\\'|])
        let times = Array.map ``read as time`` values
        return (
            match times.Length with
            | 1 -> Single times.[0]
            | _ -> times |> Array.toList |> Multi 
        )
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued double`` (a : byte[]) = binary_parser a 4 (fun x -> BitConverter.ToDouble(x, 0))

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued single`` (a : byte[]) = binary_parser a 4 (fun x -> BitConverter.ToSingle(x, 0))

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued int16`` (a : byte[]) = binary_parser a 2 (fun x -> BitConverter.ToInt16(x, 0))

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued uint16`` (a : byte[]) = binary_parser a 2 (fun x -> BitConverter.ToUInt16(x, 0))

    //--------------------------------------------------------------------------------------------------------

    let private ``parse multi valued uint32`` (a : byte[]) = binary_parser a 4 (fun x -> BitConverter.ToUInt32(x, 0))

    //--------------------------------------------------------------------------------------------------------

    let parse (preamble, elements) = 
        let (|MultiString|_|) = function
            | Lexer.VR.AE -> Some (AE)
            | Lexer.VR.AS -> Some (AS)
            | Lexer.VR.AT -> Some (AT)
            | Lexer.VR.CS -> Some (CS)
            | Lexer.VR.PN -> Some (PN)
            | Lexer.VR.SH -> Some (SH)
            | Lexer.VR.SL -> Some (SL)
            | Lexer.VR.LO -> Some (LO)
            | Lexer.VR.UT -> Some (UT)
            | Lexer.VR.UI -> Some (UI)
            | _ -> None

        let (|String|_|) = function
            | Lexer.VR.LT -> Some (LT)
            | Lexer.VR.OB -> Some (OB)
            | Lexer.VR.OF -> Some (OF)
            | Lexer.VR.ST -> Some (ST)
            | _ -> None

        let (|Binary|_|) = function
            | Lexer.VR.UN -> Some (UN)
            | Lexer.VR.OW -> Some (OW)
            | _ -> None

        let parse_simple_element value = function
            | MultiString vr_type -> ``parse multi valued string`` value |> vr_type
            | String vr_type -> ``parse string`` value |> vr_type
            | Binary vr_type -> Some (value) |> vr_type
            | Lexer.VR.DA -> ``parse multi valued date`` value |> DA
            | Lexer.VR.DS -> ``parse multi valued decimal`` value |> DS
            | Lexer.VR.DT -> ``parse multi valued datetime`` value |> DT
            | Lexer.VR.FD -> ``parse multi valued double`` value |> FD
            | Lexer.VR.FL -> ``parse multi valued single`` value |> FL
            | Lexer.VR.IS -> ``parse multi valued integer string`` value |> IS
            | Lexer.VR.SS -> ``parse multi valued int16`` value |> SS
            | Lexer.VR.TM -> ``parse multi valued time`` value |> TM
            | Lexer.VR.UL -> ``parse multi valued uint32`` value |> UL
            | Lexer.VR.US -> ``parse multi valued uint16`` value |> US
            | unknown -> failwithf "Unknown Simple VR %A" unknown
    
        let rec converter = function
            | Lexer.Simple (tag, vr, value) -> (tag, parse_simple_element value vr)
            | Lexer.Sequence (tag, elements) -> (tag, List.map create_vr_map elements.Value |> Some |> SQ)
        and create_vr_map elements = new Map<uint32, VR>(List.map converter elements)
    
        { Preamble = preamble; Values = create_vr_map elements }