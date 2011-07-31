module public Lexer
open System.IO
open System.Text
open System

type 'a Result =
| Success of 'a
| Failure of string

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

let private decode_string data = Encoding.ASCII.GetString(data).Trim([|char(0); ' '|])

//------------------------------------------------------------------------------------------------------------

type ReaderBuilder()=
    member this.Bind(a, f) = 
        match a with
        | -1 -> Failure "Tried to read beyond end of stream"
        | _ -> f a
    
    member this.Bind(a : 'a Result, f : 'a -> 'b Result) = 
        match a with
        | Failure x -> Failure x
        | Success x -> f x

    member this.Using(a : 'a , f : ('a -> Result<'b>)) : Result<'b> when 'a :> IDisposable  =
        try f a
        finally a.Dispose()

    member this.Return(a) = Success a
    
    member this.ReturnFrom(a : 'a Result) = a

let reader = ReaderBuilder()

//------------------------------------------------------------------------------------------------------------

[<AbstractClass>]
type private ByteReader(source_stream : System.IO.Stream) =
    abstract member ReadBytes : int -> byte[]
    abstract member ReadUInt16 : unit -> uint16 Result
    abstract member ReadInt32 : unit -> int Result
  
    member this.ReadTag() = reader {
        let! group = this.ReadUInt16() 
        let! element = this.ReadUInt16()
        return uint32(group) <<< 16 ||| uint32(element)
    }  

    member this.EOS = source_stream.Position >= source_stream.Length

    member this.ReadVR() = reader {
        let! char1 = source_stream.ReadByte()
        let! char2 = source_stream.ReadByte()
        return! decode_string ([| byte(char1); byte(char2) |])
        |> function
            | "AE" -> Success VR.AE
            | "AS" -> Success VR.AS
            | "AT" -> Success VR.AT
            | "CS" -> Success VR.CS
            | "DA" -> Success VR.DA
            | "DS" -> Success VR.DS
            | "DT" -> Success VR.DT
            | "FL" -> Success VR.FL
            | "FD" -> Success VR.FD
            | "IS" -> Success VR.IS 
            | "LO" -> Success VR.LO
            | "LT" -> Success VR.LT
            | "OB" -> Success VR.OB
            | "OF" -> Success VR.OF
            | "OW" -> Success VR.OW 
            | "PN" -> Success VR.PN
            | "SH" -> Success VR.SH
            | "SL" -> Success VR.SL
            | "SQ" -> Success VR.SQ
            | "SS" -> Success VR.SS 
            | "ST" -> Success VR.ST
            | "TM" -> Success VR.TM
            | "UI" -> Success VR.UI
            | "UL" -> Success VR.UL
            | "UN" -> Success VR.UN 
            | "US" -> Success VR.US
            | "UT" -> Success VR.UT
            | x -> Failure ("unknown VR Type " + x)
        }

//------------------------------------------------------------------------------------------------------------

type private LittleEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)
    
    override this.ReadBytes number = [| for x in [1..number] do yield byte(source_stream.ReadByte()) |]

    override this.ReadUInt16() = reader {
        let! lower = source_stream.ReadByte() 
        let! upper = source_stream.ReadByte()
        return uint16(lower) ||| (uint16(upper) <<< 8)
    }

    override this.ReadInt32() = reader {
        let! l1 = source_stream.ReadByte() 
        let! l2 = source_stream.ReadByte()
        let! l3 = source_stream.ReadByte()
        let! l4 = source_stream.ReadByte()
        return l1 ||| (l2 <<< 8) ||| (l3 <<< 16) ||| (l4 <<< 24)
    }

//------------------------------------------------------------------------------------------------------------
       
type private BigEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)

    override this.ReadBytes number = 
        [| for x in [1..number] do yield byte(source_stream.ReadByte()) |]
        

    override this.ReadUInt16() = reader {
        let! upper = source_stream.ReadByte() 
        let! lower = source_stream.ReadByte()
        return uint16(lower) ||| (uint16(upper) <<< 8)
    }

    override this.ReadInt32() = reader {
        let! l1 = source_stream.ReadByte()
        let! l2 = source_stream.ReadByte()
        let! l3 = source_stream.ReadByte()
        let! l4 = source_stream.ReadByte()
        return (l1 <<< 24) ||| (l2 <<< 16) ||| (l3 <<< 8) ||| l4
    }
     
//------------------------------------------------------------------------------------------------------------

type DataElement = 
| Simple of uint32 * VR * byte[] 
| Complex of uint32 * DataElement list list

//------------------------------------------------------------------------------------------------------------

let private read_implicit_vr_element (tag_dict : Map<uint32, VR>) (r : ByteReader) = reader {
    let! tag = r.ReadTag()
    let! length = r.ReadInt32()
    let vr = tag_dict.[tag]
    let value = r.ReadBytes length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let private read_explicit_vr_element (r : ByteReader) = reader {
    let (|PaddedSpecialLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
        | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedSpecialLengthVR
        | VR.UT -> PaddedExplicitLengthVR
        | _ -> UnpaddedVR

    let! tag = r.ReadTag()
    let! vr = r.ReadVR()
        
    let! length = 
        match vr with
        | PaddedExplicitLengthVR -> 
            reader {
                let reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // However we cannot allocate objects larger than 2GB, so we might need to
                // do 2 reads and return a sequence of bytes?
                return! r.ReadInt32()
            }
        | PaddedSpecialLengthVR -> 
            reader {
                let reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // Additionally, it might not be an explicit length, for example for SQ it might be delimited
                // values until the End Of Sequence marker.
                return! r.ReadInt32()
            }
        | UnpaddedVR -> 
            reader { 
                let! result = r.ReadUInt16()
                return int(result)
            }      
    
    let value =  r.ReadBytes length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let rec private read_elements read_element (acc : DataElement list) (r : ByteReader) : DataElement list Result = reader {
    if r.EOS
    then return acc
    else
        let! tag, vr, value = read_element r
        match vr with
        | VR.SQ -> return []
        | _ -> return! read_elements read_element (Simple(tag, vr, value)::acc) r
}

//------------------------------------------------------------------------------------------------------------

let private read_meta_information data = reader {
    let reader = LittleEndianByteReader data
    let! (length_tag, length_vr, length_value : byte[]) = read_explicit_vr_element reader
    let length = 
        int(length_value.[3]) <<< 24 ||| int(length_value.[2]) <<< 16 ||| int(length_value.[1]) <<< 8 ||| int(length_value.[0])
    use meta_info_stream = new MemoryStream(reader.ReadBytes(length))
    let! meta_info = read_elements read_explicit_vr_element [] (LittleEndianByteReader(meta_info_stream))
    return Simple(length_tag, length_vr, length_value)::meta_info
}

//------------------------------------------------------------------------------------------------------------

let private read_preamble (data_stream : Stream) = 
    if data_stream.Length > 132L
    then 
        let preamble = [| for x in [0..127] do yield byte(data_stream.ReadByte()) |]
        match decode_string [| for x in [128..131] do yield byte(data_stream.ReadByte()) |] with
        | "DICM" -> Success(preamble)
        | _ -> Failure("The DICM tag was not found.")
    else Failure("The stream does not contain enough data to be a valid DICOM file.")

//------------------------------------------------------------------------------------------------------------

let read (data_stream : Stream) tag_dict = reader {
    data_stream.Position <- 0L
    
    match read_preamble data_stream with
    | Failure x -> return! Failure x
    | Success preamble ->
        let (|LittleEndianExplicitVR|LittleEndianImplicitVR|BigEndianExplicitVR|Unknown|) = 
            function
            | "1.2.840.10008.1.2.1" -> LittleEndianExplicitVR
            | "1.2.840.10008.1.2" -> LittleEndianImplicitVR
            | "1.2.840.10008.1.2.2" -> BigEndianExplicitVR
            | _ -> Unknown
        
        let! meta_info = read_meta_information data_stream
        
        let! transfer_syntax = 
            List.find
                (function
                    | Simple(tag,_,_) -> tag = Tags.TransferSyntaxUID
                    | _ -> false
                )
                meta_info
            |> function
            | Simple (_,_,value) -> Success (decode_string value)
            | _ -> Failure "Transfer Syntax must be simple"
        
        let! little_endian, implicit_vr = 
            match transfer_syntax with
            | BigEndianExplicitVR -> Success (false, false)
            | LittleEndianExplicitVR -> Success (true, false)
            | LittleEndianImplicitVR -> Success (true, true)
            | Unknown -> Failure "Unknown transfer syntax"
        let! data_set = 
            read_elements 
                (if implicit_vr 
                    then read_implicit_vr_element tag_dict 
                    else read_explicit_vr_element
                ) 
                meta_info
                (if little_endian 
                    then (LittleEndianByteReader data_stream) :> ByteReader 
                    else (BigEndianByteReader data_stream) :> ByteReader
                ) 
                
        return (preamble, data_set)
}