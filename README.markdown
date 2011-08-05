FSharp-DICOM
============

This is a work in progress library for reading DICOM files, following the DICOM chapter 10 format.
It also serves as a platform for me to learn F# while writing something useful.

The library consists of 3 main parts.

TagDictionary
-------------
This is a map that is used for Implicit VR formatted DICOM files. When implicit format is used the lexer needs to
perform a lookup against the map to determine what the VR for the Data Element should be.

Lexer
-----
The Lexer performs a first pass over the DICOM file in order to split it up into the Preamble, Meta-Information Header
and the set of DataElements. The lexed data then consists of a Preamble, and a set of DataElements where the DataElements
are simple constructs containing the VR, tag and byte[] containing the value. An exception to this is SQ VRs where the 
DataElement will actually be a list of DataElements.

Parser
------
The Parser transforms the DataElements produced by the Lexer into a stronger typed versions. This means that the DataElements no 
longer contain byte[] as their value and are now represented by strings or ints etc. It also supports value multiplicity as 
specified in the DICOM standard.
