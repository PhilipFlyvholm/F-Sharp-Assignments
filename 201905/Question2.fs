module Question2

let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

let rec g xs =
    function
    | []    -> xs = []
    | y::ys -> 
        match f y xs with
        | Some xs' -> g xs' ys
        | None     -> false
        
        
//Question 2.1
// A
// f = 'a -> 'a list -> 'a list option
// g = 'a list -> 'a list -> bool
//
// B
// f returns list without x if the list contains x otherwise none
// g returns true if the lists are the same otherwise false
//