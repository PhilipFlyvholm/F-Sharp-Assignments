open System
open Exam2020

let testQ1() =
    (* Testsfor Q1.1 *)

    printfn "Question 1"
    printfn "%A" (insertTail true [])
    printfn "%A" (insertTail 5 [1; 3; 8; 9])
    printfn "%A" (insertTail 'c' ['a'; 'd'; 'e'])

    printfn "%A" (insertionSortTail [5; 3; 1; 8; 9])
    printfn "%A" (insertionSortTail ['o'; 'l'; 'H'; 'e'; 'l'])

    printfn "%A" (insertionSort2 ['o'; 'l'; 'H'; 'e'; 'l'])
    
    printfn "%A" <| insertBy String.length "abc" ["q"; "bb"; "lbcd"]
    printfn "%A" <| insertionSortBy String.length ["bb"; "lbcd"; "q"; "abc"]

    ()

let testQ2() =
    // place debug prints for Q2 here
    ()

let testQ3 =
    // place debug prints for Q3 here
    ()

let testQ4 =
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    0 // return an integer exit code
