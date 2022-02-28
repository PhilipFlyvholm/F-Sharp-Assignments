module Assignment4.Dictionary

type Dictionary

val empty : unit -> Dictionary
val insert : string -> Dictionary -> Dictionary
val step : char -> Dictionary -> (bool * Dictionary) option