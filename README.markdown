FSharp-DICOM
============

This is a work in progress library for reading DICOM files, following the DICOM chapter 10 format.
It also serves as a platform for me to learn F# while writing something useful.

The library consists of 3 main parts.

Tags.fs
-------
This is a simple dictionary for mapping a tag name to a uint32 value. It is for developers to use
so that they don't have to remember the hundreds of values that are used to represent the various tags.

Lexer.fs
--------
The Lexer performs a first pass over the DICOM file in order to split it up into the Preamble, and the set of DataElements. 
The DataElements either represent a simple tuple of VR, tag and value, or they represent an SQ element in which case they
are a tuple of tag and list list DataElement. The value of a simple DataElement is a byte[] read straight from the DICOM file.

Parser.fs
---------
The Parser transforms the DataElements produced by the Lexer into stronger typed VR objects. This means that each DataElement's 
byte[] value has been translated into a specific type as defined by the DataElement's VR code and the DICOM specification. These
types are either a single value or a list of values.
