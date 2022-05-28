module _202108.Question1

//Question 1.1

type binList<'a, 'b> =
| Nil
| Cons1 of 'a * binList<'a, 'b>
| Cons2 of 'b * binList<'a, 'b>

let rec length =
    function
    | Nil -> 0
    | Cons1(_, binList) | Cons2(_, binList) -> 1 + length binList
    
let split lst =
    let rec aux lst acc =
        match lst with
        | Nil -> acc
        | Cons1 (a, binList) -> aux binList ((fst acc)@[a], snd acc)
        | Cons2 (b, binList) -> aux binList (fst acc, (snd acc)@[b])
    aux lst ([],[])
    
let length2 lst = split lst |> (fun (l, r) -> (List.length l, List.length r))

//Question 1.3
let map f g lst =
    let rec aux lst =
        match lst with
        | Nil -> Nil
        | Cons1 (a, binList) -> Cons1 ((f a), aux binList)
        | Cons2 (b, binList) -> Cons2 ((g b), aux binList)
    aux lst
    
//Question 1.4
let filter f g lst =
    let rec aux lst =
        match lst with
        | Nil -> Nil
        | Cons1 (a, binList) when (f a) -> Cons1 (a, aux binList)
        | Cons2 (b, binList) when (g b) -> Cons2 (b, aux binList)
        | Cons1 (_, binList) | Cons2 (_, binList)-> aux binList
    aux lst
    
//Question 1.5
let fold f g acc lst =
    let rec aux lst acc =
        match lst with
        | Nil -> acc
        | Cons1 (a, binList) -> aux binList (f acc a)
        | Cons2 (b, binList) -> aux binList (g acc b)
    aux lst acc
    
