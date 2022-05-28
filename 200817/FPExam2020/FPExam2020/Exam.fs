module Exam2020_2
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
(* module Exam2020_2 = *)

(* 1: Binary search trees *)

    type 'a bintree = 
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

    let rec insert x =
        function
        | Leaf -> Node (Leaf, x, Leaf)
        | Node (tl, a, tr) when x <= a -> Node((insert x tl), a, tr)
        | Node (tl, a, tr) -> Node(tl, a, (insert x tr))
    
(* Question 1.2 *)
    let fromList lst =
        let rec aux acc =
            function
            | [] -> acc
            | x::xs -> aux (insert x acc) xs
        aux Leaf lst

(* Question 1.3 *)

    let fold (f: 'a->'b->'a) (acc:'a) tree : 'a=
        let rec aux tree acc =
            match tree with
            | Leaf -> acc
            | Node (tl, a, tr) ->  aux tl acc |> f <|a |> aux tr
        aux tree acc
        
    let foldBack (f: 'a->'b->'a) (acc:'a) tree : 'a=
        let rec aux tree acc =
            match tree with
            | Leaf -> acc
            | Node (tl, a, tr) ->  aux tr acc |> f <|a |> aux tl
        aux tree acc
    let inOrder t =
        foldBack (fun acc i -> i::acc) [] t

(* Question 1.4 *)

    (* 

    Q: Consider the following map function

    *)

    let rec badMap f =
      function
      | Leaf -> Leaf
      | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)

    (*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree` 
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: Since a binary search tree needs to be in a specific order with the lowest to the left and highest to the right 
    then we can using the map make it go out of order
    badMap (fun x -> -x) (Node (2, Node (1, Leaf, Leaf), Node(3, Leaf, Leaf))) which takes a valid search tree as argument,
       but which returns (Node (-2, Node (-1, Leaf, Leaf), Node(-3, Leaf, Leaf)))
       which has flipped the ordering of the tree.
    *)

    let rec map f =
        let rec aux acc =
            function
            | Leaf -> acc
            | Node (l,y,r) -> aux (aux (insert (f y) acc) l) r
        aux Leaf

(* 2: Code Comprehension *)
    let rec foo =
        function 
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
        foo = 'a list -> 'a list
        bar = 'a list -> bool
        baz = 'a list -> 'a list

    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A:
    Bar checks if the list is sorted
    Baz sorts the list and checks if it sorted it correctly


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
    foo = sort
    bar = isSorted
    baz = sortAndCheck
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: 
        foo and bar does not support an empty list

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: 
        This will not cause a issue for any possible execution of baz since baz pattern matches after an empty list

    *)

    let foo2 =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs
    let bar2 =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)

(* Question 2.3 *) 

    (* Consider this alternative definition of *)

    (*
    let rec foo =
        function 
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs
    *)
    let rec foo3 =
      function
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: 
        foo3 will never sort the function since x::xs is always true when x::y::xs when x > y is true meaning it will never
        come down to x::y::xs when x > y. This also means that it just returns the list it gets
        
        foo [2;1] = [1;2]
        foo3 [2;1] = [2;1]
    *)

(* Question 2.4 *)

    let bar3 l : bool =
         List.mapi (
            fun index item ->
            match index with
            | 0 -> true
            | index -> (List.item (index - 1) l) <= item) l
         |> List.exists (fun c -> c = false) |> not
    
   
(* Question 2.5 *)

    (*

    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: Foo is not tail recursive since it does x :: foo ... meaning it needs to wait for foo to finish executing before it can concatenate

    *)

    (* ONLY implement the one that is NOT already tail recursive *)

    let fooTail l =
        let rec aux f =
            function
            | [] -> f []
            | [x]                 -> f [x]
            | x::y::xs when x > y -> aux (fun r -> f (y :: r)) (x::xs)
            | x::xs               -> aux (fun r -> f (x :: r)) xs
        aux id l
    let bazTail _ = failwith "not implemented"

(* 3: Big Integers *)

(* Question 3.1 *)

    type bigInt = int list

    let fromString (s:string) : bigInt =
        Seq.map (fun c -> int c - int '0') s |> List.ofSeq
    let toString (bigInt:bigInt) = List.fold (fun acc i -> acc + (string i)) "" bigInt

(* Question 3.2 *)

    let add (bigInt1:bigInt) (bigInt2:bigInt) =
        let (longBoi, smallBoi) = if List.length bigInt1 >=  List.length bigInt2 then (bigInt1, bigInt2) else (bigInt2, bigInt1)
        let smallLength = List.length smallBoi
        let longLength = List.length longBoi
        
        let rec aux overflow (i:int) (acc:bigInt) =
            match List.tryItem (smallLength - i) smallBoi with
            | None -> acc
            | Some x ->
                match List.tryItem (longLength - i) longBoi with
                | None ->
                    let r = overflow+x
                    match r with
                    | r when r >= 10 -> aux (r/10) (i + 1) ((r%10)::acc)
                    | r-> aux 0 (i + 1) (r::acc)
                | Some y ->
                    let r = x+y+overflow
                    match r with
                    | r when r >= 10 -> aux (r/10) (i + 1) ((r%10)::acc)
                    | r-> aux 0 (i + 1) (r::acc)
        aux 0 1 []

(* Question 3.3 *)

    let multSingle _ = failwith "not implemented"

(* Question 3.4 *)

    let mult _ = failwith "not implemented"

(* Question 3.5 *)

    let fact _ = failwith "not implemented"

(* 4: Lazy lists *)

    type 'a llist =
    | Cons of (unit -> ('a * 'a llist))

    let rec llzero = Cons (fun () -> (0, llzero))

(* Question 4.1 *)

    let step _ = failwith "not implemented"
    let cons _ = failwith "not implemented"

(* Question 4.2 *)

    let init _ = failwith "not implemented"

(* Question 4.3 *)

    let llmap _ = failwith "not implemented"

(* Question 4.4 *)

    let filter _ = failwith "not implemented"

(* Question 4.5 *)

    let takeFirst _ = failwith "not implemented"

(* Question 4.6 *)

    let unfold _ = failwith "not implemented"

    (* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

    let fib x =
        let rec aux acc1 acc2 =
            function
            | 0 -> acc1
            | x -> aux acc2 (acc1 + acc2) (x - 1)

        aux 0 1 x

    (* Uncomment after you have implemented init and unfold *)

(*
    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
  *)  
    (* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: <Your answer goes here>
    
    *)
