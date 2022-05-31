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

    A: 
        Foo: 'a -> 'a list -> 'a list
        Bar: 'a -> 'a list list -> 'a list list
        Baz: 'a list -> 'a list list


    Q: What do functions foo, bar, and baz do? 
       Focus on what they do rather than how they do it.

    A: 
        Foo: Removes the first instance of x
        Bar: Adds x to all sublists
        Baz: Lists all combinations of the list given


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
        Foo: RemoveFirst
        Bar: AppendToAll
        Baz: GetCombinations
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: This happens since it is missing a match on an empty list in the function pattern match on ys


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No it will never cause problems since

    *)

    let foo2 x = 
        function
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

(* Question 2.3 *) 

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a -> (list<'a> -> list<list<'a>>)


    Q: What does it do? Focus on what it does rather than how it does it.

    A: It sends the result of foo to baz and the result of baz to bar
    *)

(* Question 2.4 *)

    let bar2 x xss = List.map (fun xs -> x::xs) xss

(* Question 2.5 *)

    let baz2 =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            (*let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs*)
            List.collect (fun x -> (foo x >> baz >> bar x) xs) xs

(* Question 2.6 *)

    (*
     let foo2 x = 
        function
        | [] -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)
        
    Q: The function foo is not tail recursive. Why?
    
    A: 
    Foo is not tail recusive since it calls "y :: (foo x ys)" which means that (foo x ys) needs to finish running before it can concatenate it with y
    foo 5 [1;4;5;3]
    1 :: (foo 5 [4;5;3])
    1 :: (4 :: (foo 5 [5;3]))
    1 :: (4 :: [3])
    1 :: [4;3]
    [1;4;3]
    As seen above then it needs to go through the list before it can start concatinating the result
    *)

    let fooTail x l =
        let rec aux f =
            function
            | [] -> f []
            | y::ys when x = y -> f ys
            | y::ys -> aux (fun r -> f <| y::r) ys
        aux id l

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

    type shape =
        | ROCK
        | PAPER
        | SCISSOR
    type result =
        | P1Win
        | P2Win
        | Draw
        
    let rps (p1Play:shape) (p2Play:shape) =
        match p1Play, p2Play with
        | ROCK, ROCK | PAPER, PAPER | SCISSOR, SCISSOR -> Draw
        | PAPER, ROCK | ROCK, SCISSOR | SCISSOR, PAPER -> P1Win
        | ROCK, PAPER | SCISSOR, ROCK | PAPER, SCISSOR -> P2Win
(* Question 3.2 *)

    type strategy = (shape * shape) list -> shape

    let parrot (defaultPlay:shape) =
        function
        | [] -> defaultPlay
        | move::_ -> snd move
    
    let beatingStrat =
        function
        | [] -> ROCK
        | moves ->
            let stats = List.countBy snd moves
            let max = List.maxBy snd stats
            let maxList = List.filter (fun t -> t = max) stats
            match List.tryFind (fun (shape, _) -> shape = ROCK) stats with
            | Some _ -> PAPER
            | None -> match List.tryFind (fun (shape, _) -> shape = PAPER) stats with
                        | Some _ -> SCISSOR
                        | None -> match List.tryFind (fun (shape, _) -> shape = SCISSOR) stats with
                                    | Some _ -> ROCK
                                    | None -> List.head stats |> fst

    let roundRobin defaultShapes =
        let mutable shapes = defaultShapes
        let rec aux () =
            match shapes with
            | [] ->
                shapes <- defaultShapes
                aux ()
            | shape::shapes' ->
                shapes <- shapes'
                shape
        fun _ -> aux ()

(* Question 3.3 *)

    (* 
    
    Q: It may be tempting to generate a function that calculates your 
       point tuple after n rounds and then use Seq.initInfinite to 
       generate the sequence. This is not a good solution. Why?

    A: InitInfinite needs to calculate all previous entries to calculate the nth entry
    
    *)

    let bestOutOf (p1Strat:strategy) (p2Strat:strategy) =
        Seq.unfold (fun (p1Moves, p2Moves, p1Wins, p2Wins) ->
                    let p1Play = p1Strat p1Moves
                    let p2Play = p2Strat p2Moves
                    let (p1Wins', p2Wins') =
                        match rps p1Play p2Play with
                        | P1Win -> (p1Wins+1, p2Wins)
                        | P2Win -> (p1Wins, p2Wins+1)
                        | Draw -> (p1Wins, p2Wins)
                    let p1Moves' = (p1Play,p2Play)::p1Moves
                    let p2Moves' = (p2Play,p1Play)::p2Moves
                    Some ((p1Wins', p2Wins'), (p1Moves', p2Moves', p1Wins', p2Wins'))
                   ) ([], [], 0, 0)
        |> Seq.append (Seq.singleton (0,0))

(* Question 3.4 *)

    let playTournament numRounds (players:strategy list) =
        let rec play players =
            match players with
            | [] -> None
            | [(points, id)] -> Some id
            | players ->
                List.fold (fun acc p ->
                            match acc with
                            | (Some p1, None)::xs -> (Some p1, Some p)::xs
                            | (Some _, Some _)::xs -> (Some p, None)::acc
                            | _ -> [(Some p, None)]
                        ) [] players
                |> List.map (fun game ->
                        async {
                            return match game with
                                    | (Some p1, None) -> Some p1 
                                    | (Some (p1Strat, p1Id), Some (p2Strat, p2Id)) ->
                                        match bestOutOf p1Strat p2Strat |> Seq.item numRounds with
                                        | (p1Wins, p2Wins) when p1Wins = p2Wins -> None
                                        | (p1Wins, p2Wins) when p1Wins > p2Wins ->
                                            Some(p1Strat, p1Id)
                                        | _ ->
                                            Some(p2Strat, p2Id)
                                    | _ -> None
                        }
                    )
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.filter Option.isSome
                |> Array.map Option.get
                |> List.ofArray//Converts to list from array and also gets the values in the option
                |> play
        play <| List.mapi (fun i x -> (x, i)) players
            
        

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

    type stack = int list (* replace unit with the correct type declaration *)

    let emptyStack = [] (* replace () with the correct value *)

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

    let push x = S (fun s -> Some ((), x::s))
    let pop = S (fun s ->
                    match s with
                    | [] -> None
                    | x::xs -> Some (x, xs)
                )

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
    
    A: We want to decide when to print since printf has side effects of writing to the console
    
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
    let isInt (str : string) : bool = System.Int32.TryParse str |> fst
    let binop op = pop >>= (fun x1 -> pop >>= (fun x2 -> op x2 x1 |> push))
    let calculateRPN () =
        let rec aux () =
            read >>= (fun s ->
                    match s with
                    | Some "+" -> binop (+) >>>= aux ()
                    | Some "-" -> binop (-) >>>= aux ()
                    | Some "*" -> binop (*) >>>= aux ()
                    | Some str when isInt str -> push (int str) >>>= aux ()
                    | Some _ -> fail
                    | None -> pop >>= fun x -> string x |> write
                )
        aux ()