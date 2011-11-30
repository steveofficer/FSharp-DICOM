FSharp-DICOM
============

This is a work in progress library for reading DICOM files, following the DICOM chapter 10 format.
It also serves as a platform for me to learn F# while writing something useful.

The library consists of 2 main parts.

Lexer.fs
--------
The Lexer performs a first pass over the DICOM file in order to split it up into the Preamble, and the set of DataElements. 
The DataElements either represent a simple tuple of VR, tag and value, or they represent an SQ element in which case they
are a tuple of tag and list DataElement. The value of a simple DataElement is a byte[] in little endian format read from the DICOM file.

Parser.fs
---------
The Parser transforms the DataElements produced by the Lexer into a stronger typed VR objects. This means that each DataElement's 
byte[] value has been translated into a specific type as defined by the DataElement's VR code and the DICOM specification. These
types are either a single value or a list of values.
