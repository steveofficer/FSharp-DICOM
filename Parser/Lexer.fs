﻿module public Lexer
open System.IO
open System.Text
open System

//------------------------------------------------------------------------------------------------------------

/// Result has 2 cases :
///    1. Success: It contains the resulting value of the operation
///    2. Failure: It contains a string which identifies the reason for failure.
/// This is used as an alternative to throwing an exception because the compiler will force
/// you to handle the Failure case. Whereas, the compiler wont force you to handle the exception.
/// Additionally, parsing a malformed DICOM source is not considered to be "exceptional" or unexpected.
type 'a Result =
| Success of 'a
| Failure of string

//------------------------------------------------------------------------------------------------------------

/// This represents the type of the data that has been read from the source.
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

/// DataElement has 2 cases:
///    1. Simple: A basic DICOM value consisting of a Tag, a VR and a byte[] that represents the underlying value.
///    2. Comple: The equivalent so an SQ DICOM VR, it is a list of sub-DICOM datasets.
type DataElement = 
| Simple of uint32 * VR * byte[] 
| Complex of uint32 * DataElement list list

//------------------------------------------------------------------------------------------------------------

/// A computation expression used for reading bytes from the stream. As long as bytes can be read the computation
/// will progress. If a byte cannot be read then Failure is returned by the computation.
type ByteReaderBuilder()=
    member this.Bind(a, f) = 
        match a with
        | -1 -> Failure "Tried to read beyond end of stream"
        | _ -> f a

    member this.Return(a) = Success a
    
    member this.ReturnFrom(a : 'a Result) = a

//------------------------------------------------------------------------------------------------------------

/// A computation expression used for progressing through a series of expressions that return Result. 
/// As long as the operations return Success, the computation will proceed.
type ResultReaderBuilder() = 
    member this.Using(a : 'a , f : ('a -> 'b Result)) : 'b Result when 'a :> IDisposable = 
        try f a
        finally a.Dispose()
    
    member this.Bind(a : 'a Result, f : 'a -> 'b Result) = 
        match a with
        | Failure x -> Failure x
        | Success x -> f x
    member this.Return(a) = Success a
    
    member this.ReturnFrom(a : 'a Result) = a

//------------------------------------------------------------------------------------------------------------
    
let byte_reader = ByteReaderBuilder()
let result_reader = ResultReaderBuilder()

//------------------------------------------------------------------------------------------------------------

/// Contains functions for reading bytes and other required types from DICOM data
[<AbstractClass>]
type private ByteReader(source_stream : System.IO.Stream) =
    abstract member ReadUInt16 : unit -> uint16 Result
    abstract member ReadInt32 : unit -> int Result
   
    member this.ReadBytes number = 
        let bytes = [|
            let counter = ref 0
            while !counter < number do
                let value = source_stream.ReadByte()
                if value = -1
                then counter := number
                else 
                    counter := !counter + 1
                    yield byte(value)
        |]
        if bytes.Length <> number
        then Failure "Tried to read beyond the end of the stream"
        else Success bytes
    
    member this.ReadSwappedBytes number = result_reader {
        if number % 2 = 1
        then return! Failure "The number of bytes to read must be even"
        else 
            let! bytes = this.ReadBytes number
            return [|
                let counter = ref 0
                while !counter < number do
                    yield bytes.[!counter+1]
                    yield bytes.[!counter]
                    counter := !counter + 2
            |]
    }
    
    member this.ReadTag() = result_reader {
        let! group = this.ReadUInt16() 
        let! element = this.ReadUInt16()
        return uint32(group) <<< 16 ||| uint32(element)
    }  

    member this.EOS = source_stream.Position >= source_stream.Length

    member this.ReadVR() = byte_reader {
        let! char1 = source_stream.ReadByte()
        let! char2 = source_stream.ReadByte()
        return! Utils.decode_string ([| byte(char1); byte(char2) |])
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
            | x -> Failure (sprintf "%A is an unknown VR Type" x)
        }
    
    member this.ReadVRValue vr size = result_reader {
        let (|ByteSwapped|NotByteSwapped|) = 
            function
            | VR.AT| VR.OB | VR.OF | VR.OW -> ByteSwapped
            | _ -> NotByteSwapped
            
        return! 
            match vr with
            | ByteSwapped -> this.ReadSwappedBytes size
            | NotByteSwapped -> this.ReadBytes size
    }
    
//------------------------------------------------------------------------------------------------------------

/// An implementation of ByteReader that is used for reading data stored in Little Endian format
type private LittleEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)
    
    override this.ReadUInt16() = byte_reader {
        let! lower = source_stream.ReadByte() 
        let! upper = source_stream.ReadByte()
        return (uint16(upper) <<< 8) ||| uint16(lower) 
    }

    override this.ReadInt32() = byte_reader {
        let! b1 = source_stream.ReadByte() 
        let! b2 = source_stream.ReadByte()
        let! b3 = source_stream.ReadByte()
        let! b4 = source_stream.ReadByte()
        return b1 ||| (b2 <<< 8) ||| (b3 <<< 16) ||| (b4 <<< 24)
    }

//------------------------------------------------------------------------------------------------------------

/// An implementation of ByteReader that is used for reading data stored in Big Endian format       
type private BigEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)
    
    override this.ReadUInt16() = byte_reader {
        let! upper = source_stream.ReadByte() 
        let! lower = source_stream.ReadByte()
        return (uint16(upper) <<< 8) ||| uint16(lower)
    }

    override this.ReadInt32() = byte_reader {
        let! b1 = source_stream.ReadByte()
        let! b2 = source_stream.ReadByte()
        let! b3 = source_stream.ReadByte()
        let! b4 = source_stream.ReadByte()
        return (b1 <<< 24) ||| (b2 <<< 16) ||| (b3 <<< 8) ||| b4
    }
     
//------------------------------------------------------------------------------------------------------------

let private read_implicit_vr_element (tag_dict : Map<uint32, VR>) (r : ByteReader) = result_reader {
    let! tag = r.ReadTag()
    let! length = r.ReadInt32()
    let vr = tag_dict.[tag]
    let! value = r.ReadVRValue vr length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let private read_explicit_vr_element (r : ByteReader) = result_reader {
    let (|PaddedSpecialLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
        | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedSpecialLengthVR
        | VR.UT -> PaddedExplicitLengthVR
        | _ -> UnpaddedVR

    let! tag = r.ReadTag()
    let! vr = r.ReadVR()
        
    let! length = 
        match vr with
        | PaddedExplicitLengthVR -> 
            result_reader {
                let reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // However we cannot allocate objects larger than 2GB, so we might need to
                // do 2 reads and return a sequence of bytes?
                return! r.ReadInt32()
            }
        | PaddedSpecialLengthVR -> 
            result_reader {
                let reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // Additionally, it might not be an explicit length, for example for SQ it might be delimited
                // values until the End Of Sequence marker.
                return! r.ReadInt32()
            }
        | UnpaddedVR -> 
            result_reader { 
                let! result = r.ReadUInt16()
                return int(result)
            }      
    
    let! value =  r.ReadVRValue vr length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let rec private read_elements f (acc : DataElement list) (r : ByteReader) : DataElement list Result = result_reader {
    if r.EOS
    then return acc
    else
        let! tag, vr, value = f r
        match vr with
        | VR.SQ -> return []
        | _ -> return! read_elements f (Simple(tag, vr, value)::acc) r
}

//------------------------------------------------------------------------------------------------------------

let private read_meta_information data = result_reader {
    let reader = LittleEndianByteReader data
    let! (length_tag, length_vr, length_value : byte[]) = read_explicit_vr_element reader
    let length = 
        int(length_value.[3]) <<< 24 ||| int(length_value.[2]) <<< 16 ||| int(length_value.[1]) <<< 8 ||| int(length_value.[0])
    let! value = reader.ReadBytes(length)
    use meta_info_stream = new MemoryStream(value)
    let! meta_info = read_elements read_explicit_vr_element [] (LittleEndianByteReader(meta_info_stream))
    return Simple(length_tag, length_vr, length_value)::meta_info
}

//------------------------------------------------------------------------------------------------------------

let private read_preamble (data_stream : Stream) = 
    if data_stream.Length > 132L
    then 
        let preamble = [| for x in [0..127] do yield byte(data_stream.ReadByte()) |]
        match Utils.decode_string [| for x in [128..131] do yield byte(data_stream.ReadByte()) |] with
        | "DICM" -> Success(preamble)
        | _ -> Failure "The DICM tag was not found."
    else Failure "The stream does not contain enough data to be a valid DICOM file."

//------------------------------------------------------------------------------------------------------------

let read (data_stream : Stream) tag_dict = result_reader {
    data_stream.Position <- 0L
    
    let! preamble = read_preamble data_stream
    
    let! meta_info = read_meta_information data_stream
    
    let! transfer_syntax = 
        List.find
            (function
                | Simple(tag,_,_) -> tag = Tags.TransferSyntaxUID
                | _ -> false
            )
            meta_info
        |> function
            | Simple (_,_,value) -> Success (Utils.decode_string value)
            | _ -> Failure "Transfer Syntax must be simple"
    
    // Set up an active pattern so that we can do some pattern matching on the TransferSyntax
    let (|LittleEndianExplicitVR|LittleEndianImplicitVR|BigEndianExplicitVR|Unknown|) = 
        function
            | "1.2.840.10008.1.2.1" -> LittleEndianExplicitVR
            | "1.2.840.10008.1.2" -> LittleEndianImplicitVR
            | "1.2.840.10008.1.2.2" -> BigEndianExplicitVR
            | _ -> Unknown
            
    let! little_endian, implicit_vr = 
        match transfer_syntax with
        | BigEndianExplicitVR -> Success (false, false)
        | LittleEndianExplicitVR -> Success (true, false)
        | LittleEndianImplicitVR -> Success (true, true)
        | Unknown -> Failure (sprintf "%s is an unknown transfer syntax" transfer_syntax)
    printfn "Little Endian %A" little_endian
    printfn "Implicit VR %A" implicit_vr
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