module public Lexer
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
///    2. Complex: The equivalent of an SQ DICOM VR, it is a list of "sub" datasets.
type DataElement = 
    | Simple of uint32 * VR * byte[] 
    | Sequence of uint32 * DataElement list list

//------------------------------------------------------------------------------------------------------------

/// A computation expression used for reading bytes from the stream. As long as bytes can be read the computation
/// will progress. If a byte cannot be read then Failure is returned by the computation.
type ByteReaderBuilder()=
    member this.Bind(a, f) = 
        match a with
            | -1 -> Failure "Tried to read beyond the end of the stream"
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
    let stream_length = source_stream.Length
    
    abstract member ReadUInt16 : unit -> uint16 Result
    abstract member ReadInt32 : unit -> int Result
    abstract member ReadVRValue : VR -> int -> byte[] Result
    
    member this.ReadByte() = byte_reader {
        let! b = source_stream.ReadByte()
        return byte(b)
    }
    
    member this.ReadBytes number = 
        let buffer = Array.create number 0uy
        if source_stream.Read(buffer, 0, number) = number
        then Success buffer
        else Failure (sprintf "Could not read %i bytes from the stream" number)
    
    member this.ReadTag() = result_reader {
        let! group = this.ReadUInt16() 
        let! element = this.ReadUInt16()
        return uint32(group) <<< 16 ||| uint32(element)
    }  

    member this.EOS = source_stream.Position >= stream_length

    member this.ReadVR() = result_reader {
        let! char1 = this.ReadByte()
        let! char2 = this.ReadByte()
        return! Utils.decode_string ([| char1; char2 |])
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
    
    default this.ReadVRValue vr size = this.ReadBytes size
    
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
    
    member private this.ReadSwappedBytes number = result_reader {
        let rec swapper (src : byte[]) i =
            if i = number
            then src
            else 
                let temp = src.[i]
                src.[i] <- src.[i+1]
                src.[i+1] <- temp
                swapper src (i + 2)

        match number &&& 1 with
            | 0 ->
                let! bytes = this.ReadBytes number
                return swapper bytes 0
            | _ -> return! Failure "The number of bytes to read must be even when doing byte swapping"
    }
    
    member private this.ReadLittleEndianBytes number = result_reader {
        let! bytes = this.ReadBytes number
        return Array.rev bytes
    }
    
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
    
    override this.ReadVRValue vr size = result_reader {
        let (|ByteSwapped|ToLittleEndian|Unmodified|) = 
            function
                | VR.AT | VR.OB | VR.OF | VR.OW -> ByteSwapped
                | VR.SL | VR.SS | VR.US | VR.UL  -> ToLittleEndian
                | _ -> Unmodified 
            
        return! 
            match vr with
                | ByteSwapped -> this.ReadSwappedBytes size
                | ToLittleEndian -> this.ReadLittleEndianBytes size
                | Unmodified -> this.ReadBytes size
    }
     
//------------------------------------------------------------------------------------------------------------

let private implicit_vr_data_element tag_dict (r : ByteReader) = result_reader {
    let! tag = r.ReadTag()
    let! vr = tag_dict tag
    let! length = r.ReadInt32()
    let! value = r.ReadVRValue vr length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let private explicit_vr_data_element (r : ByteReader) = result_reader {
    let (|PaddedSpecialLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
        | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedSpecialLengthVR
        | VR.UT -> PaddedExplicitLengthVR
        | _ -> UnpaddedVR
    let determine_value_length = function
        | PaddedExplicitLengthVR -> 
            result_reader {
                let! reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // However we cannot allocate objects larger than 2GB on the CLR.
                // For now we fail.
                let! length = r.ReadInt32()
                if length < 0
                then return! Failure "The parser only supports data element values that are less than 2GB in size."
                else return length
            }
        | PaddedSpecialLengthVR -> 
            result_reader {
                let! reserved = r.ReadBytes(2)
                // According to Part5 7.1.2 this length is actually an unsigned 32 bit integer.
                // Additionally, it might not be an explicit length, for example for SQ it might be delimited
                // values until the End Of Sequence marker.
                let! length = r.ReadInt32()
                if length < 0
                then return! Failure "The parser only supports data element values that are less than 2GB in size."
                else return length
            }
        | UnpaddedVR -> 
            result_reader { 
                let! result = r.ReadUInt16()
                return int(result)
            }
            
    let! tag = r.ReadTag()
    let! vr = r.ReadVR()
    let! length = determine_value_length vr
    let! value =  r.ReadVRValue vr length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let private read_elements read_item (existing_items : DataElement list) (source : ByteReader) : DataElement list Result = 
    let rec perform_read result =
        if source.EOS
        then Success result
        else
            match read_item source with
                | Failure reason -> Failure reason
                | Success (tag, vr, value) ->
                    if vr = VR.SQ 
                    then 
                        // 1. iterate through each item (each item is a set of dicom tags) this is done by looking for
                        //    delimeters and reading the bytes between each delimeter this is done until the 
                        //    end of sequence marker is reached.
                        // 2. perform read_elements on each item
                        // 3. collect the result of read_elements into a list
                        // 4. Create a Complex(tag, resulting_list)::result
                        perform_read (Sequence(tag, [])::result)
                    else perform_read (Simple(tag, vr, value)::result)
    perform_read existing_items
            
//------------------------------------------------------------------------------------------------------------

let private read_meta_information data = result_reader {
    let inline to_int (a : byte[]) = (int(a.[3]) <<< 24) ||| (int(a.[2]) <<< 16) ||| (int(a.[1]) <<< 8) ||| int(a.[0])
    
    // Find out what the length of the header is and then read the bytes that comprise the header
    let reader = LittleEndianByteReader data
    let! (length_tag, length_vr, raw_length : byte[]) = explicit_vr_data_element reader
    let length = raw_length |> to_int
        
    let! value = reader.ReadBytes(length)
    
    // Now read the bytes that represent the header
    use meta_info_stream = new MemoryStream(value)
    let! meta_info = read_elements explicit_vr_data_element [] (LittleEndianByteReader(meta_info_stream))
    return Simple(length_tag, length_vr, raw_length)::meta_info
}

//------------------------------------------------------------------------------------------------------------

let private read_preamble (data_stream : Stream) = 
    if data_stream.Length > 132L
    then 
        let preamble = Array.create 128 0uy
        data_stream.Read(preamble, 0, 128) |> ignore
        let marker = Array.create 4 0uy
        data_stream.Read(marker, 0, 4) |> ignore
        match Utils.decode_string marker with
            | "DICM" -> Success(preamble)
            | _ -> Failure "The DICM tag was not found."
    else Failure "The stream does not contain enough data to be a valid DICOM file."

//------------------------------------------------------------------------------------------------------------

let read (data_stream : Stream) tag_dict = result_reader {
    let! preamble = read_preamble data_stream
    
    let! meta_info = read_meta_information data_stream
    
    let! transfer_syntax = 
        List.tryFind (function | Simple(tag,_,_) -> tag = 131088u | _ -> false) meta_info
        |> function
            | Some x ->
                match x with
                    | Simple (_,_,value) -> Success (Utils.decode_string value)
                    | Sequence(_,_) -> Failure "Transfer Syntax is not a Sequence VR Type"
            | None -> Failure "The Transfer Syntax tag could not be found"
    
    let! little_endian, implicit_vr = 
        let (|LittleEndianExplicitVR|LittleEndianImplicitVR|BigEndianExplicitVR|Unknown|) = function
            | "1.2.840.10008.1.2.1" -> LittleEndianExplicitVR
            | "1.2.840.10008.1.2" -> LittleEndianImplicitVR
            | "1.2.840.10008.1.2.2" -> BigEndianExplicitVR
            | _ -> Unknown
        match transfer_syntax with
            | BigEndianExplicitVR -> Success (false, false)
            | LittleEndianExplicitVR -> Success (true, false)
            | LittleEndianImplicitVR -> Success (true, true)
            | Unknown -> Failure (sprintf "%s is an unknown transfer syntax" transfer_syntax)
        
    let! data_set = 
        read_elements 
            (if implicit_vr 
                then implicit_vr_data_element tag_dict 
                else explicit_vr_data_element
            ) 
            meta_info
            (if little_endian 
                then (LittleEndianByteReader data_stream) :> ByteReader 
                else (BigEndianByteReader data_stream) :> ByteReader
            ) 
            
    return (preamble, data_set)
}