module Dict
    type Dictionary =
        | Node of Map<char, Dictionary> * bool // Node with children nodes containing characters and a boolean for "end of the word"
    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool
    val step : char -> dict: Dictionary -> (bool * Dictionary) option
    val mkDict : seq<string> -> Dictionary -> Dictionary