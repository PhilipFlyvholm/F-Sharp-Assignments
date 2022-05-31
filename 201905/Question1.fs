module Question1

type Peano =
| O
| S of Peano

let rec toInt peano =
    match peano with
    | O -> 0u
    | S s ->  1u+(toInt s)

let rec fromInt x =
    match x with
    | 0u -> O
    | x -> S (fromInt (x-1u))
    
let rec add a b =
    match b with
    | O -> a
    | S s -> add (S a) s

let rec mult a b =
    match a with
    | O -> O
    | S s -> add b (mult s b)
    
let rec pow a b =
    match b with
    | O -> S O
    | S n -> mult a (pow a n)
    
let tailAdd a b =
    let rec aux acc b =
        match b with
        | O -> acc
        | S n -> aux (S acc) n
    aux a b

let tailMult a b =
    let rec aux acc a =
        match a with
        | O -> O
        | S O -> acc
        | S n -> aux (tailAdd b acc) n
    aux b a

let tailPow a b =
    match b with
    | O -> S O
    | S n -> 
        let rec aux acc b =
            match b with
            | O -> acc
            | S n -> tailMult a (aux a n)
        aux O n
        
let rec loop f acc s =
    match s with
    | O -> acc
    | S s -> loop f (f acc) s

let loopAdd a b =
    loop S a b
    
let loopMult a b =
    loop (fun x -> loopAdd x a) O b
    
let loopPow a b =
    if b = O then (S O) else loop (fun x -> loopMult x a) (S O) b