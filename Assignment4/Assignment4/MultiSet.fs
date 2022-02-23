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
