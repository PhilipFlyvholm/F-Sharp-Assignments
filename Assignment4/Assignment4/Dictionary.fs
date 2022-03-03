module Dictionary

type Dict = Dictionary of Set<string>
    
let hashtag = char 0
let empty () = (Dictionary Set.empty)
let insert s (Dictionary d) = Dictionary (Set.add s d)
let lookup s (Dictionary d) = d.Contains s