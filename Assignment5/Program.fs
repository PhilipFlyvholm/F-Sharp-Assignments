//5.1
let sum m n =
    let rec sumC acc =
        function
        | (_,n) when n = 0 -> acc+m
        | (m,n) -> sumC (acc + (m+n)) (m, n-1)
    sumC 0 (m,n)

//5.2
let length xs =
    let rec lengthC acc = 
        function
        | [] -> acc
        | x::xs -> lengthC (acc + 1) xs
    lengthC 0 xs

//5.3
let foldBack (folder:('a->'b->'c)) lst acc =
    let rec foldBackC c =
        function
        | [] -> c acc
        | x::lst -> foldBackC (fun r -> c (folder x r)) lst
    foldBackC id lst

//5.4

let factC x = 
    let rec aux c =
        function
        | 0 -> c 1
        | x -> aux (fun r -> c (x*r)) (x-1)
    aux id x

//5.5
let fibA x =
    let rec aux prev1 prev2 =
        function
        | i when i < x -> aux prev2 (prev1+prev2) (i+1)
        | _ -> prev1
    aux 0 1 0
(*
fibA 5
fibA 40
*)
let fibC x =
    let rec aux c =
        function
            | i when i = 0 -> 0
            | i when i = 1 -> 1
            | i when i <= x -> aux (fun r -> (r + (c (i-1) + c (i-2)))) (i-1)
            | _ -> 0
    aux id x
(*
fibC 5
*)