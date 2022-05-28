module CodeComprehension

    let f s =
        let l = String.length s
        let rec aux =
            function
            | i when i = l -> []
            | i -> s.[i] :: aux (i + 1)

        aux 0

    let g s = 
        s |> f |>
        List.filter System.Char.IsLetter |>
        List.map System.Char.ToLower |>
        fun lst -> lst = List.rev lst

    (* Question 2.1 *)

    (*
    What are the types of functions f and g?
        f = string -> char list
        g = string -> bool
    What do functions f and g do? Focus on what they do rather than how they do it.
        f converts a sting to a list of chars
        g takes a string and checks if it is palindrome
            It does this by converting the string to a list and removes all non-letter chars.
            The chars remaining will be set to lowercase and then the function checks if the list is the same in reverse order
    What would be appropriate names for functions f and g?
        f should be called stringToCharList
        g should be called isPalindrome
    *)

    (* Question 2.2 *)


    let f2 s = [for c in s -> c]

    (* Question 2.3 *)

    let g2 =
        f >>
        List.filter System.Char.IsLetter >>
        List.map System.Char.ToLower >>
        fun lst -> lst = List.rev lst

    
    (* Question 2.4 *)

    (* 
        It is not tail recursive since it needs to evaluate aux before adding s.[i] to the list
        
        f "abba"
        
        aux 0
        'a'::[aux 1]
        'a'::'b'::[aux 2]
        'a'::'b'::'b'::[aux 3]
        'a'::'b'::'b'::'a'::[aux 4]
        'a'::'b'::'b'::'a'::[]
        'a'::'b'::'b'::['a']
        'a'::'b'::['b';'a']
        'a'::['b';'b';'a']
        ['a';'b';'b';'a']
    *)

    let fTail s =
        let l = String.length s
        let rec aux f i =
            match i with
            | i when i = l -> f []
            | i -> aux (fun r -> (f (s.[i] :: r))) (i + 1)
            
        aux id 0

    (* Question 2.5 *)

    let gOpt (s:string) =
        let l = String.length s
        let rec aux sI eI =
            match sI, eI with
            | sI, eI when sI = eI -> true
            | sI, eI when sI > l || eI < 0 -> true
            | sI, _ when not (System.Char.IsLetter s.[sI]) -> aux (sI+1) eI
            | _, eI when not (System.Char.IsLetter s.[eI]) -> aux sI (eI-1)
            | sI, eI when System.Char.ToLower s.[sI] = System.Char.ToLower s.[eI] -> aux (sI+1) (eI-1)
            | _,_ -> false
        aux 0 (String.length s - 1)