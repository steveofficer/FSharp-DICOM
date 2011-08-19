module Utils
open System.Text

/// Returns an ASCII string of the provided byte[]. Also trims any blank characters from the string.
let decode_string data = Encoding.ASCII.GetString(data).Trim([|char(0); ' '|])