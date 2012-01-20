module public Lexer
open System.IO
open System.Text
open System

//------------------------------------------------------------------------------------------------------------

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

type DataElement = 
    | Simple of uint32 * VR * byte[] 
    | Sequence of uint32 * DataElement list list

//------------------------------------------------------------------------------------------------------------

type ResultReaderBuilder() = 
    member this.Using(a : 'a , f : ('a -> 'b Result)) : 'b Result when 'a :> IDisposable = 
        try f a
        finally a.Dispose()
    
    member this.Bind(a : 'a Result, f : 'a -> 'b Result) = 
        match a with
            | Failure reason -> Failure reason
            | Success x -> f x

    member this.Return(a) = Success a
    
    member this.ReturnFrom(a : 'a Result) = a

//------------------------------------------------------------------------------------------------------------
    
let operation = ResultReaderBuilder()

//------------------------------------------------------------------------------------------------------------

[<AbstractClass>]
type private ByteReader(source_stream : System.IO.Stream) =
    let stream_length = source_stream.Length
    
    abstract member ReadUInt16 : unit -> uint16 Result
    abstract member ReadInt32 : unit -> int Result
    abstract member ReadVRValue : VR -> int -> byte[] Result
    
    default this.ReadVRValue vr size = this.ReadBytes size
    
    member this.ReadByte() = 
        match source_stream.ReadByte() with
            | -1 -> Failure "An attempt was made to read beyond the end of the stream"
            | b -> Success (byte b)
    
    member this.ReadBytes number = 
        let buffer = Array.create number 0uy
        match source_stream.Read(buffer, 0, number) with
            | x when x = number -> Success buffer
            | _ -> Failure "Could not read the requested number of bytes from the stream"
    
    member this.ReadTag() = operation {
        let! group = this.ReadUInt16() 
        let! element = this.ReadUInt16()
        return uint32(group) <<< 16 ||| uint32(element)
    }  

    member this.EOS = source_stream.Position >= stream_length

    member this.ReadVR() = operation {
        let to_vr s = 
            let result = ref VR.AE
            if Enum.TryParse(s, result)
            then Success !result
            else Failure (sprintf "%s is not a recognized VR type" s)
        let! char1 = this.ReadByte()
        let! char2 = this.ReadByte()
        return! Utils.decode_string ([| char1; char2 |]) |> to_vr
    }
    
//------------------------------------------------------------------------------------------------------------

type private LittleEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)
    
    override this.ReadUInt16() = operation {
        let! lower = this.ReadByte() 
        let! upper = this.ReadByte()
        return (uint16(upper) <<< 8) ||| uint16(lower) 
    }

    override this.ReadInt32() = operation {
        let! b1 = this.ReadByte() 
        let! b2 = this.ReadByte()
        let! b3 = this.ReadByte()
        let! b4 = this.ReadByte()
        return int32(b1) ||| (int32(b2) <<< 8) ||| (int32(b3) <<< 16) ||| (int32(b4) <<< 24)
    }

//------------------------------------------------------------------------------------------------------------

type private BigEndianByteReader(source_stream : System.IO.Stream) =
    inherit ByteReader(source_stream)
    
    member private this.ReadSwappedBytes number = operation {
        let rec swapper (src : byte[]) i =
            if i = number
            then src
            else 
                src.[i] <- src.[i] ^^^ src.[i+1]
                src.[i+1] <- src.[i] ^^^ src.[i+1]
                src.[i] <- src.[i] ^^^ src.[i+1]
                swapper src (i + 2)

        match number &&& 1 with
            | 0 ->
                let! bytes = this.ReadBytes number
                return swapper bytes 0
            | _ -> return! Failure "The number of bytes to read must be even when doing byte swapping"
    }
    
    member private this.ReadLittleEndianBytes number = operation {
        let! bytes = this.ReadBytes number
        return Array.rev bytes
    }
    
    override this.ReadUInt16() = operation {
        let! upper = this.ReadByte() 
        let! lower = this.ReadByte()
        return (uint16(upper) <<< 8) ||| uint16(lower)
    }

    override this.ReadInt32() = operation {
        let! b1 = this.ReadByte()
        let! b2 = this.ReadByte()
        let! b3 = this.ReadByte()
        let! b4 = this.ReadByte()
        return (int32(b1) <<< 24) ||| (int32(b2) <<< 16) ||| (int32(b3) <<< 8) ||| int32(b4)
    }
    
    override this.ReadVRValue vr size = operation {
        let (|ByteSwapped|ToLittleEndian|Direct|) = 
            function
                | VR.AT | VR.OB | VR.OF | VR.OW -> ByteSwapped
                | VR.SL | VR.SS | VR.US | VR.UL  -> ToLittleEndian
                | _ -> Direct 
            
        return! 
            match vr with
                | Direct -> this.ReadBytes size
                | ByteSwapped -> this.ReadSwappedBytes size
                | ToLittleEndian -> this.ReadLittleEndianBytes size
    }
     
//------------------------------------------------------------------------------------------------------------

let private implicit_vr_data_element tag_dict (r : ByteReader) = operation {
    let! tag = r.ReadTag()
    let! vr = tag_dict tag
    let! length = r.ReadInt32()
    let! value = r.ReadVRValue vr length
    return (tag, vr, value)
}

//------------------------------------------------------------------------------------------------------------

let private explicit_vr_data_element (r : ByteReader) = operation {
    let (|PaddedLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
        | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedLengthVR
        | VR.UT -> PaddedExplicitLengthVR
        | _ -> UnpaddedVR
    
    let (|TooLong|Undefined|Explicit|) = function
        | x when x = UInt32.MaxValue -> Undefined
        | x when x = uint32(Int32.MaxValue) -> TooLong
        | _ -> Explicit
        
    let parse_sequence() = operation {
        let rec reader acc = 
            match r.ReadByte() with
                | Success b ->
                    let result = b::acc
                    match result with
                        | 221uy::224uy::238uy::255uy::t -> Success (List.rev t)
                        | _ -> reader result
                | Failure reason -> Failure reason
        let! result = reader []
        return List.toArray result
    }
    
    let get_value vr = 
        match vr with
            | PaddedExplicitLengthVR -> 
                operation {
                    let! reserved = r.ReadBytes(2)
                    let! length = r.ReadInt32()
                    match uint32 length with
                        | Explicit -> return! r.ReadVRValue vr length
                        | TooLong -> return! Failure "The current implementation only supports data element values that are less than 2GB in size."
                        | Undefined -> return! Failure (sprintf "The %A VR does not support undefined length" vr)
                }
            | PaddedLengthVR -> 
                operation {
                    let! reserved = r.ReadBytes(2)
                    let! length = r.ReadInt32()
                    match uint32 length with
                        | Explicit -> return! r.ReadVRValue vr length
                        | Undefined -> return! parse_sequence()
                        | TooLong -> return! Failure "The current implementation only supports data element values that are less than 2GB in size."
                }
            | UnpaddedVR -> 
                operation { 
                    let! result = r.ReadUInt16()
                    return! r.ReadVRValue vr (int(result))
                }
            
    let! tag = r.ReadTag()
    let! vr = r.ReadVR()
    let! value =  get_value vr
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
                        // 5. This can be done lazily because we already have the byte[], we aren't dependent 
                        // on the stream
                        perform_read (Sequence(tag, [])::result)
                    else perform_read (Simple(tag, vr, value)::result)
    perform_read existing_items
            
//------------------------------------------------------------------------------------------------------------

let private read_meta_information data = operation {
    let inline to_int (a : byte[]) = (int(a.[3]) <<< 24) ||| (int(a.[2]) <<< 16) ||| (int(a.[1]) <<< 8) ||| int(a.[0])
    
    // Find out what the length of the header is and then read the bytes that comprise the header
    let reader = LittleEndianByteReader data
    let! (length_tag, length_vr, raw_length : byte[]) = explicit_vr_data_element reader
    let length = raw_length |> to_int
        
    let! value = reader.ReadBytes(length)
    
    // Now read the bytes that represent the header
    use meta_info_stream = new MemoryStream(value)
    let! meta_info = read_elements explicit_vr_data_element [] (LittleEndianByteReader meta_info_stream)
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

let read (data_stream : Stream) tag_dict = operation {
    let (|LittleEndianExplicitVR|LittleEndianImplicitVR|BigEndianExplicitVR|Unknown|) = function
        | "1.2.840.10008.1.2.1" -> LittleEndianExplicitVR
        | "1.2.840.10008.1.2" -> LittleEndianImplicitVR
        | "1.2.840.10008.1.2.2" -> BigEndianExplicitVR
        | _ -> Unknown
    
    let TransferSyntax = 131088u
    
    let! preamble = read_preamble data_stream
    
    let! meta_info = read_meta_information data_stream
    
    let! transfer_syntax = 
        List.tryFind (function | Simple(tag,_,_) -> tag = TransferSyntax | _ -> false) meta_info
        |> function
            | Some x ->
                match x with
                    | Simple (_,_,value) -> Success (Utils.decode_string value)
                    | Sequence(_,_) -> Failure "Transfer Syntax is expected to be a Simple VR Type"
            | None -> Failure "The Transfer Syntax tag could not be found"
    
    let! little_endian, implicit_vr = 
        match transfer_syntax with
            | BigEndianExplicitVR -> Success (false, false)
            | LittleEndianExplicitVR -> Success (true, false)
            | LittleEndianImplicitVR -> Success (true, true)
            | Unknown -> Failure (sprintf "%s is an unknown transfer syntax" transfer_syntax)
        
    let vr_reader = 
        if implicit_vr 
        then implicit_vr_data_element tag_dict 
        else explicit_vr_data_element
    let byte_reader = 
        if little_endian 
        then (LittleEndianByteReader data_stream) :> ByteReader 
        else (BigEndianByteReader data_stream) :> ByteReader
    let! data_set = read_elements vr_reader meta_info byte_reader
    return (preamble, data_set)
}