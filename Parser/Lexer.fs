module public Lexer
open System.IO
open System.Text
open System

//------------------------------------------------------------------------------------------------------------

type private ByteReader(source_stream : System.IO.Stream, little_endian : bool) =
    let read_byte() = byte(source_stream.ReadByte())
    
    member this.ReadBytes number = 
        let buff = Array.create number (byte(0))
        source_stream.Read(buff, 0, number) |> ignore
        buff

    member this.ReadUInt16() = uint16(read_byte()) ||| (uint16(read_byte()) <<< 8)
    
    member this.ReadInt32() = 
        let l1 = int(read_byte()) 
        let l2 = int(read_byte()) <<< 8 
        let l3 = int(read_byte()) <<< 16
        let l4 = int(read_byte()) <<< 24
        l1 ||| l2 ||| l3 ||| l4
    
    member this.ReadTag() = uint32(this.ReadUInt16()) <<< 16 ||| uint32(this.ReadUInt16())
    
    member this.EOS = source_stream.Position >= source_stream.Length
        
//------------------------------------------------------------------------------------------------------------

type VR =
| AE = 0 
| AS = 1 
| AT = 2 
| CS = 3 
| DA = 4 
| DS = 5 
| DT = 6 
| FL = 7 
| FD = 8 
| IS = 9 
| LO = 10 
| LT = 11
| OB = 12 
| OF = 13 
| OW = 14 
| PN = 15 
| SH = 16 
| SL = 17 
| SQ = 18 
| SS = 19 
| ST = 20 
| TM = 21 
| UI = 22
| UL = 23 
| UN = 24 
| US = 25 
| UT = 26 

//------------------------------------------------------------------------------------------------------------

type 'a Result =
| Success of 'a
| Failure of string

//------------------------------------------------------------------------------------------------------------

type DataElement = 
| Simple of uint32 * VR * byte[] 
| Complex of uint32 * DataElement list list

//------------------------------------------------------------------------------------------------------------

let private decode_string data = Encoding.ASCII.GetString(data)

//------------------------------------------------------------------------------------------------------------  

let is_dicom (content : byte[]) = content.[128..131] |> decode_string  = "DICM"

//------------------------------------------------------------------------------------------------------------

let private read_implicit_vr_element (tag_dict : Map<uint32, VR>) (reader : ByteReader) =
    let tag = reader.ReadTag()
    let vr = tag_dict.[tag]
    let value = reader.ReadInt32() |> reader.ReadBytes
    (tag, vr, value)
    
//------------------------------------------------------------------------------------------------------------

let private read_explicit_vr_element (reader : ByteReader) =
    let (|PaddedSpecialLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
        | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedSpecialLengthVR
        | VR.UT -> PaddedExplicitLengthVR
        | _ -> UnpaddedVR

    let tag = reader.ReadTag()
    let vr = 
        decode_string(reader.ReadBytes 2)
        |> function
            | "AE" -> VR.AE
            | "AS" -> VR.AS
            | "AT" -> VR.AT
            | "CS" -> VR.CS
            | "DA" -> VR.DA
            | "DS" -> VR.DS
            | "DT" -> VR.DT
            | "FL" -> VR.FL
            | "FD" -> VR.FD
            | "IS" -> VR.IS 
            | "LO" -> VR.LO
            | "LT" -> VR.LT
            | "OB" -> VR.OB
            | "OF" -> VR.OF
            | "OW" -> VR.OW 
            | "PN" -> VR.PN
            | "SH" -> VR.SH
            | "SL" -> VR.SL
            | "SQ" -> VR.SQ
            | "SS" -> VR.SS 
            | "ST" -> VR.ST
            | "TM" -> VR.TM
            | "UI" -> VR.UI
            | "UL" -> VR.UL
            | "UN" -> VR.UN 
            | "US" -> VR.US
            | "UT" -> VR.UT
            | _ -> failwith "unknown VR Type"
        
    let value = 
        match vr with
        | PaddedExplicitLengthVR ->
            reader.ReadBytes(2) |> ignore
            // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
            // However we cannot allocate objects larger than 2GB, so we might need to
            // do 2 reads and return a sequence of bytes?
            reader.ReadInt32() |> reader.ReadBytes
        | PaddedSpecialLengthVR -> 
            reader.ReadBytes(2) |> ignore
            // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
            // Additionally, it might not be an explicit length, for example for SQ it might be delimited
            // values until the End Of Sequence marker.
            reader.ReadInt32() |> reader.ReadBytes
        | UnpaddedVR -> int(reader.ReadUInt16()) |> reader.ReadBytes
        
        
    (tag, vr, value)
    
//------------------------------------------------------------------------------------------------------------

let rec private read_elements read_element acc (reader : ByteReader) = 
    if reader.EOS
    then acc
    else
        let tag, vr, value = read_element reader
        match vr with
        | VR.SQ -> []
        | _ -> read_elements read_element (Simple(tag, vr, value)::acc) reader
        
//------------------------------------------------------------------------------------------------------------

let private read_meta_information data = 
    let reader = ByteReader(data, false)
    let preamble, length_element = 
        let preamble = reader.ReadBytes(128)
        reader.ReadBytes(4) |> ignore
        (preamble, Simple(read_explicit_vr_element reader))
        
    use meta_info_stream = 
        new MemoryStream(
            length_element 
            |> function
                | Simple(_,_,value) -> BitConverter.ToInt32(value, 0)
                | _ -> invalidArg "element" "Length Element must be a Simple element" 
            |> reader.ReadBytes
        )
        
    let meta_info = read_elements read_explicit_vr_element [] (ByteReader(meta_info_stream, false))
    (preamble, length_element::meta_info)
    
//------------------------------------------------------------------------------------------------------------

let read (data : byte[]) transfer_syntax_decoder tag_dict = 
    if data |> is_dicom  
    then
        use data_stream = new MemoryStream(data)
        let preamble, meta_info = read_meta_information data_stream
        
        let transfer_syntax = 
            List.find
                (function
                    | Simple(tag,_,_) -> tag = Tags.TransferSyntaxUID
                    | _ -> false
                )
                meta_info
                
        let little_endian, implicit_vr = transfer_syntax_decoder transfer_syntax
        let reader = ByteReader(data_stream, little_endian)
        
        let data_set = 
            read_elements 
                (if little_endian 
                    then read_implicit_vr_element tag_dict 
                    else read_explicit_vr_element
                ) 
                meta_info
                reader 
        Success (preamble, data_set)
    else 
        Failure "The DICM tag was not found."