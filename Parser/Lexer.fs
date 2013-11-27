module public Lexer
    open System.IO
    open System.Text
    open System
    open Utils

    //--------------------------------------------------------------------------------------------------------

    type 'a ParseResult =
        | Data of 'a
        | Error of string

    //--------------------------------------------------------------------------------------------------------

    type VR =
        | AE = 16709 // "AE"
        | AS = 16723 // "AS"
        | AT = 16724 // "AT"
        | CS = 17235 // "CS"
        | DA = 17473 // "DA"
        | DS = 17491 // "DS"
        | DT = 17492 // "DT"
        | FL = 17996 // "FL"
        | FD = 17988 // "FD"
        | IS = 18771 // "IS"
        | LO = 19535 // "LO"
        | LT = 19540 // "LT"
        | OB = 20290 // "OB"
        | OF = 20294 // "OF"
        | OW = 20311 // "OW"
        | PN = 20558 // "PN"
        | SH = 21320 // "SH"
        | SL = 21324 // "SL"
        | SQ = 21329 // "SQ"
        | SS = 21331 // "SS"
        | ST = 21332 // "ST"
        | TM = 21581 // "TM"
        | UI = 21833 // "UI"
        | UL = 21836 // "UL"
        | UN = 21838 // "UN"
        | US = 21843 // "US"
        | UT = 21844 // "UT"

    //--------------------------------------------------------------------------------------------------------

    type Endianness = 
        | Big = 0
        | Little = 1

    //--------------------------------------------------------------------------------------------------------

    type VRType =
        | Implicit = 0
        | Explicit = 1

    //--------------------------------------------------------------------------------------------------------

    type DataElement = 
        | Simple of uint32 * VR * byte[] 
        | Sequence of uint32 * Lazy<DataElement list list>

    //--------------------------------------------------------------------------------------------------------

    type ParseResultBuilder() = 
        member this.Using(a : 'a , f : ('a -> 'b ParseResult)) : 'b ParseResult when 'a :> IDisposable = 
            try f a
            finally a.Dispose()
    
        member this.Bind(a : 'a ParseResult, f : 'a -> 'b ParseResult) = 
            match a with
                | Data x -> f x
                | Error reason -> Error reason

        member this.Return(a) = Data a
    
        member this.ReturnFrom(a : 'a ParseResult) = a

    //--------------------------------------------------------------------------------------------------------
    
    let parser = ParseResultBuilder()

    //--------------------------------------------------------------------------------------------------------

    [<AbstractClass>]
    type private ByteReader(source_stream : System.IO.Stream) =
        let stream_length = source_stream.Length
    
        abstract member ReadUInt16 : unit -> uint32 ParseResult
        abstract member ReadUInt32 : unit -> uint32 ParseResult
        abstract member ReadVRValue : VR -> uint32 -> byte[] ParseResult
    
        default this.ReadVRValue vr size = this.ReadBytes size
    
        member this.ReadByte() = 
            match source_stream.ReadByte() with
                | -1 -> Error "An attempt was made to read beyond the end of the stream"
                | b -> Data (byte b)
    
        member this.ReadBytes number = 
            let count = int number
            let buffer = Array.zeroCreate count
            match source_stream.Read(buffer, 0, count) with
                | x when x = count -> Data buffer
                | _ -> Error "Could not read the requested number of bytes from the stream"
    
        member this.ReadTag() = parser {
            let! group = this.ReadUInt16() 
            let! element = this.ReadUInt16()
            return group <<< 16 ||| element
        }  

        member this.Ended = source_stream.Position >= stream_length

        member this.ReadVR() = parser {
            let! bytes = this.ReadBytes 2ul
            let vr_type = (int32(bytes.[0]) <<< 8) ||| int32(bytes.[1])
            if System.Enum.IsDefined(typeof<VR>, vr_type)
            then return enum<VR>(vr_type)
            else return! bytes |> decode_string |> sprintf "Unknown VR type %s" |> Error
        }
    
    //--------------------------------------------------------------------------------------------------------

    type private LittleEndianByteReader(source_stream : System.IO.Stream) = 
        inherit ByteReader(source_stream)
    
        override this.ReadUInt16() = parser {
            let! bytes = this.ReadBytes 2ul 
            return (uint32(bytes.[1]) <<< 8) ||| uint32(bytes.[0]) 
        }

        override this.ReadUInt32() = parser {
            let! bytes = this.ReadBytes 4ul
            return (uint32(bytes.[3]) <<< 24) ||| (uint32(bytes.[2]) <<< 16) ||| (uint32(bytes.[1]) <<< 8) ||| uint32(bytes.[0])
        }

    //--------------------------------------------------------------------------------------------------------

    type private BigEndianByteReader(source_stream : System.IO.Stream) =
        inherit ByteReader(source_stream)
    
        member private this.ReadSwappedBytes number = parser {
            let count = int number
            let rec swapper (src : byte[]) i =
                if i = count
                then src
                else 
                    src.[i] <- src.[i] ^^^ src.[i+1]
                    src.[i+1] <- src.[i] ^^^ src.[i+1]
                    src.[i] <- src.[i] ^^^ src.[i+1]
                    swapper src (i + 2)

            if count &&& 1 = 0
            then
                let! bytes = this.ReadBytes number
                return swapper bytes 0
            else
                return! Error "The number of bytes to read must be even when doing byte swapping"
        }
    
        member private this.ReadLittleEndianBytes number = parser {
            let! bytes = this.ReadBytes number
            return Array.rev bytes
        }
    
        override this.ReadUInt16() = parser {
            let! bytes = this.ReadBytes 2ul 
            return (uint32(bytes.[0]) <<< 8) ||| uint32(bytes.[1])
        }

        override this.ReadUInt32() = parser {
            let! bytes = this.ReadBytes 4ul
            return (uint32(bytes.[0]) <<< 24) ||| (uint32(bytes.[1]) <<< 16) ||| (uint32(bytes.[2]) <<< 8) ||| uint32(bytes.[3])
        }

        override this.ReadVRValue vr size = parser {
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
     
    //--------------------------------------------------------------------------------------------------------

    let private implicit_vr_data_element tag_lookup (r : ByteReader) = parser {
        let! tag = r.ReadTag()
        let! vr = tag_lookup tag
        let! length = r.ReadUInt32()
        let! value = r.ReadVRValue vr length
        return (tag, vr, value)
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``explicit vr data element`` (r : ByteReader) = parser {
        let (|PaddedLengthVR|PaddedExplicitLengthVR|UnpaddedVR|) = function
            | VR.OB | VR.OW | VR.OF | VR.SQ | VR.UN -> PaddedLengthVR
            | VR.UT -> PaddedExplicitLengthVR
            | _ -> UnpaddedVR
        
        let (|TooLong|Undefined|Explicit|) = function
            | UInt32.MaxValue -> Undefined
            | x when x = uint32(Int32.MaxValue) -> TooLong
            | _ -> Explicit
        
        let parse_sequence() = parser {
            let rec reader acc = parser {
                let! b = r.ReadByte()
                let result = b::acc
                match result with
                    | 221uy::224uy::238uy::255uy::t -> return (List.rev t)
                    | _ -> return! reader result
            }
            let! result = reader []
            return List.toArray result
        }
    
        let failure_message = "The current implementation only supports values that are less than 2GB in size."
        let read_value vr = 
            match vr with
                | PaddedExplicitLengthVR -> 
                    parser {
                        let! reserved = r.ReadBytes 2ul
                        let! length = r.ReadUInt32()
                        match length with
                            | Explicit -> return! r.ReadVRValue vr length
                            | TooLong -> return! Error failure_message
                            | Undefined -> return! Error (sprintf "The %A VR does not support undefined length" vr)
                    }
                | PaddedLengthVR -> 
                    parser {
                        let! reserved = r.ReadBytes 2ul
                        let! length = r.ReadUInt32()
                        match length with
                            | Explicit -> return! r.ReadVRValue vr length
                            | Undefined -> return! parse_sequence()
                            | TooLong -> return! Error failure_message
                    }
                | UnpaddedVR -> 
                    parser { 
                        let! result = r.ReadUInt16()
                        return! r.ReadVRValue vr result
                    }
            
        let! tag = r.ReadTag()
        let! vr = r.ReadVR()
        let! value =  read_value vr
        return (tag, vr, value)
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``read elements`` ``read element from`` (existing_items : DataElement list) (source : ByteReader) = parser {
        let rec ``read next`` elements = parser {
            if source.Ended
            then return elements
            else
                let! (tag, vr, value) = ``read element from`` source
                let element = 
                    match vr with
                        | VR.SQ -> Sequence(tag, lazy([])) // TODO: Extract the items from the byte sequence
                        | _ -> Simple(tag, vr, value)
                return! element::elements |> ``read next`` 
        }
        return! ``read next`` existing_items
    }
            
    //--------------------------------------------------------------------------------------------------------

    let private ``read meta_information`` data = parser {
        let inline le_to_int (a : byte[]) = (uint32(a.[3]) <<< 24) ||| (uint32(a.[2]) <<< 16) ||| (uint32(a.[1]) <<< 8) ||| uint32(a.[0])
    
        // Find out what the length of the header is and then read the bytes that comprise the header
        let reader = LittleEndianByteReader data
        let! (length_tag, length_vr, raw_length : byte[]) = ``explicit vr data element`` reader
        let value_length = raw_length |> le_to_int
        
        let! value = reader.ReadBytes value_length
    
        // Now read the bytes that represent the header
        use meta_info_stream = new MemoryStream(value)
        let! meta_info = ``read elements`` ``explicit vr data element`` [] (LittleEndianByteReader meta_info_stream)
        return Simple(length_tag, length_vr, raw_length)::meta_info
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``read preamble`` (data_stream : Stream) = 
        if data_stream.Length > 132L
        then 
            let preamble = Array.zeroCreate 128
            data_stream.Read(preamble, 0, 128) |> ignore
            let dicm_marker = Array.zeroCreate 4
            data_stream.Read(dicm_marker, 0, 4) |> ignore
            match dicm_marker with
                | [| 68uy; 73uy; 67uy; 77uy |] -> Data preamble
                | _ -> Error "The DICM tag was not found."
        else Error "The stream does not contain enough data to be a valid DICOM file."

    //--------------------------------------------------------------------------------------------------------

    let private ``determine format`` elements = parser {
        let find_transfer_syntax() = 
            let is_transfer_syntax = function 
                | Simple(tag,_,_) -> tag = 131088u 
                | _ -> false
            
            match List.tryFind is_transfer_syntax elements with
                | None -> Error "The Transfer Syntax element could not be found"
                | Some transfer_syntax_element -> 
                    match transfer_syntax_element with
                        | Simple (_, _, transfer_syntax) -> Data (decode_string transfer_syntax)
                        | _ -> Error "Transfer Syntax is expected to be a Simple VR Type"
            
        let! transfer_syntax = find_transfer_syntax()
    
        match transfer_syntax with
            | "1.2.840.10008.1.2.2" -> return (Endianness.Big, VRType.Explicit)
            | "1.2.840.10008.1.2.1" -> return (Endianness.Little, VRType.Explicit)
            | "1.2.840.10008.1.2" -> return (Endianness.Little, VRType.Implicit)
            | unknown -> return! Error (sprintf "%s is an unknown transfer syntax" unknown)
    }

    //--------------------------------------------------------------------------------------------------------

    let private ``get element reader`` tag_dict data_stream endianness vr_type = parser {
        let! byte_reader = 
            match endianness with
                | Endianness.Little -> Data ((LittleEndianByteReader data_stream) :> ByteReader)
                | Endianness.Big -> Data ((BigEndianByteReader data_stream) :> ByteReader)
                | _ -> Error "Unknown endianness value"

        let! element_reader = 
            match vr_type with
                | VRType.Implicit -> Data (implicit_vr_data_element tag_dict)
                | VRType.Explicit -> Data ``explicit vr data element``
                | _ -> Error "Unknown vr type"

        return (byte_reader, element_reader)
    }

    //--------------------------------------------------------------------------------------------------------

    let Read (data_stream : Stream) tag_dict = parser {
        let! preamble = ``read preamble`` data_stream
        let! meta_info = ``read meta_information`` data_stream
        let! endianness, vr_type = ``determine format`` meta_info
        let! byte_reader, element_reader = ``get element reader`` tag_dict data_stream endianness vr_type
        let! data_set = ``read elements`` element_reader meta_info byte_reader
        return (preamble, data_set)
    }