﻿open _202108.Question1
printfn "Question 1.1"
length Nil |> printfn "0 = %A"

length (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))) |> printfn "5 = %A"

printfn "Question 1.2"
split (Nil : binList<int, bool>) |> printfn "([], []) = %A"

split (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil))))))  |> printfn "([3; 4], [true; false; true]) = %A"

printfn "Question 1.3"
map (fun x -> x % 2 = 0) 
    (function | true -> 0 | false -> 1)
    (Nil : binList<int, bool>) |> printfn "Nil = %A"

map (fun x -> x % 2 = 0) 
    (function | true -> 0 | false -> 1) 
    (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))) |> printfn "Cons1 (false, Cons2 (0, Cons1 (true, Cons2 (1, Cons2 (0, Nil))))) = %A"
    
printfn "Question 1.4"
filter (fun x -> x % 2 = 0) 
       id 
       (Nil : binList<int, bool>) |> printfn "Nil = %A"

filter (fun x -> x % 2 = 0) 
       id 
       (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))) |> printfn "Cons2 (true, Cons1 (4, Cons2 (true, Nil))) = %A"
       
printfn "Question 1.5"
fold (+) 
     (fun acc -> function | true -> acc | false -> -acc) 
     0 
     (Nil : binList<int, bool>) |> printfn "0 = %A"

fold (+) 
     (fun acc -> function | true -> acc | false -> -acc) 
     0 
     (Cons1 (3, Cons2 (true, Cons1 (4, Cons2 (false, Cons2(true, Nil)))))) |> printfn "-7 = %A"
     
open _202108.Question2
findClosestPerfectSquare 5 |> printfn "4 = %A"

approxSquare 5 0 |> printfn "2.0 = %A"

approxSquare 5 1 |> printfn "2.25 = %A"

approxSquare 5 2 |> printfn "2.236111111 = %A"

approxSquare 5 3 |> printfn "2.236067978 = %A"

approxSquare 5 4 |> printfn "2.236067977 = %A"

printfn "Question 3.2"

quadratic 5 (-4) (-1) 1 |> printfn "(1.0, -0.2) = %A"

quadratic 5 (-3) (-1) 1 |> printfn "(0.84, -0.24) = %A"

quadratic 5 (-3) (-1) 2 |> printfn "(0.8385185185, -0.2385185185) = %A"

quadratic 5 (-3) (-1) 3 |> printfn "(0.8385164807, -0.2385164807) = %A"

[1..10] |> List.map (fun x -> (x, -(x + 1), -(x + 2))) |> 
           fun eqs -> parQuadratic eqs 3 5 |> printfn "[(3.0, -1.0); (2.350781059, -0.8507810594); (2.119632981, -0.7862996478);
   (2.0, -0.75); (1.926649916, -0.7266499161); (1.877014558, -0.7103478914);
   (1.841170631, -0.6983134882); (1.814061525, -0.6890615247);
   (1.792836525, -0.681725414); (1.775765067, -0.6757650672)]
   =
   %A" 