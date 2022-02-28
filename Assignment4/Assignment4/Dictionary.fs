module Assignment4.Dictionary

open Assignment4

type Dictionary =
    | Leaf of bool
    | Node of bool * System.Collections.Generic.Dictionary<char, Dictionary>
    
let hashtag = char 0
let empty () = Leaf false

let rec insertChars (chars : char list) =
    function
    | Leaf _ when chars.IsEmpty -> Leaf true
    | Leaf b ->
        let d = System.Collections.Generic.Dictionary<char, Dictionary>()
        d.Add(chars.Head, empty ())
        Node(b, d)
    | Node (b, d) when d.ContainsKey(chars.Head) ->
        d.Add(chars.Head, (insertChars chars.[1..chars.Length] (snd( d.TryGetValue(chars.Head)))))
        Node(b, d)
    | Node (b, d) ->
        d.Add(chars.Head, (insertChars chars.[1..chars.Length] (empty ())))
        Node(b, d)
        
let insert (s:string) (d:Dictionary) =
    Array.fold (fun acc chars -> insertChars chars acc) d (Array.init s.Length (fun i -> (
            Array.toList (s.Substring(0,i).ToCharArray()) @ hashtag :: Array.toList (s.Substring(i).ToCharArray())
    )))
        
let step (c:char) (d:Dictionary) = Some (true, d)