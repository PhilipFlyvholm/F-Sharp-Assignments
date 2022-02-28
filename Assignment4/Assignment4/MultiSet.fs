module Assignment4.MultiSet


type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>

let empty = MS(Map.empty)
let isEmpty (MS(ms)) = Map.isEmpty ms
let size (MS(ms)) = (Map.fold (fun acc _ value -> value+acc) 0u ms)
let contains a (MS(ms)) =  ms.ContainsKey a
let numItems a (MS(ms)) = if contains a (MS(ms)) then ms.[a] else 0u
let add a n (MS ms) = MS(ms.Add (a, (numItems a (MS(ms)))+n))
let addSingle a (MS ms) = add a 1u (MS ms)
let remove a n (MS ms) = if (numItems a (MS(ms))) <= n then MS(ms.Remove a) else MS(ms.Add (a, (numItems a (MS(ms)))-n))
let removeSingle a (MS ms) = remove a 1u (MS ms)
let fold (f: 'a -> 'b -> uint32 -> 'a) acc (MS ms) = Map.fold f acc ms
let foldBack (f: 'a -> uint32 -> 'b -> 'b) (MS ms) acc = Map.foldBack f ms acc
let ofList lst = List.fold (fun acc a -> addSingle a acc) empty lst
let toList (MS ms) = fold (fun acc key value -> [for _ in 1 .. int value -> key] @ acc) List.empty (MS ms)
let map (f: 'a -> 'b) (MS ms) = fold (fun acc key value -> add (f key) value acc) empty (MS ms)
let max i i' = if i > i' then i else i'
let union (MS ms1 : MultiSet<'a>) (MS ms2 : MultiSet<'a>) =
    fold (fun acc key value -> if contains key acc then acc else add key value acc) 
    <| (fold (fun acc key value -> add key (max value (numItems key (MS ms1))) acc) empty (MS ms2))
    <| (MS ms1)
let sum (MS ms1 : MultiSet<'a>) (MS ms2 : MultiSet<'a>) = fold (fun acc key value -> add key value acc) (MS ms1) (MS ms2)
let subtract (MS ms1 : MultiSet<'a>) (MS ms2 : MultiSet<'a>) = fold (fun acc key value -> remove key value acc) (MS ms1) (MS ms2)
let intersection (MS ms1 : MultiSet<'a>) (MS ms2 : MultiSet<'a>) = fold (fun acc key value -> if contains key (MS ms2) then add key value acc else acc) empty (MS ms1)