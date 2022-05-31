module Exam2020
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

    let rec insert x lst =
        match lst with
        | [] -> [x]
        | y::lst when x<y -> x::y::lst
        | y::lst -> y::(insert x lst)
        
    let rec insertionSort lst =
        match lst with
        | [] -> []
        | x::lst -> insert x (insertionSort lst)
        
    
(* Question 1.2 *)

    let insertTail x lst =
        let rec aux acc lst =
            match lst with
            | [] -> x::acc
            | y::lst when x < y -> (List.rev <| x::y::lst) @ acc
            | y::lst -> aux (y::acc) lst
        List.rev <| (aux [] lst)
        
    let insertionSortTail lst =
        let rec aux lst acc =
            match lst with
            | [] -> acc
            | x::lst -> aux lst (insertTail x acc)
        aux lst []

(* Question 1.3 *)

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    A: <Your answer goes here>
    *)

    let insertionSort2 lst = List.fold (fun acc i -> insertTail i acc) [] lst

(* Question 1.4 *)
    
    let rec insertBy f x lst =
        match lst with
        | [] -> [x]
        | y::lst when (f x)<(f y) -> x::y::lst
        | y::lst -> y::(insertBy f x lst)
        
    let rec insertionSortBy f lst =
        match lst with
        | [] -> []
        | x::lst -> insertBy f x (insertionSortBy f lst)

(* 2: Code Comprehension *)
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss 

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: <Your answer goes here>


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: <Your answer goes here>


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: <Your answer goes here>

    *)

    let foo2 _ = failwith "not implemented"

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: <Your answer goes here>


    Q: What does it do? Focus on what it does rather than how it does it.

    A: <Your answer goes here>

    *)

(* Question 2.4 *)

    let bar2 _ = failwith "not implemented"

(* Question 2.5 *)

    let baz2 _ = failwith "not implemented"

(* Question 2.6 *)

    (*
    
    Q: The function foo is not tail recursive. Why?
    
    A: <Your answer goes here>

    *)

    let fooTail _ = failwith "not implemented"

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape = unit (* replace unit with the correct type declaration *)
    type result = unit (* replace unit with the correct type declaration *)

    let rps _ = failwith "not implemented"

(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot _ = failwith "not implemented"
    
    let beatingStrat _ = failwith "not implemented"

    let roundRobin _ = failwith "not implemented"

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: <Your answer goes here>
    
    *)

    let bestOutOf _ = failwith "not implemented"

(* Question 3.4 *)

    let playTournament _ = failwith "not implemented"

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = unit (* replace unit with the correct type declaration *)

    let emptyStack = () (* replace () with the correct value *)

(* Question 4.2 *)

    type SM<'a> = S of (stack -> ('a * stack) option)

    let ret x = S (fun s -> Some (x, s))
    let fail  = S (fun _ -> None)
    let bind f (S a) : SM<'b> = 
        S (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (S g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (S f) = f emptyStack 

    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"

(* Question 4.3 *)

    let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

    let read =
        let rec aux acc =
            match System.Console.Read() |> char with
            | '\n' when acc = [] -> None
            | c    when System.Char.IsWhiteSpace c -> 
                acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
            | c -> aux (c :: acc)

        S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?
    
    A: <Your answer goes here>
    
    *)

(* Question 4.4 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculateRPN _ = failwith "not implemented"