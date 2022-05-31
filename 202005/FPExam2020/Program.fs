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
    baz [] |> printfn "%A"
    ()

let testQ2() =
    // place debug prints for Q2 here
    ()

let testQ3 () =
    // place debug prints for Q3 here
    printfn "%A" <| rps ROCK PAPER  
    printfn "%A" <| rps PAPER ROCK  
    printfn "%A" <| rps SCISSOR SCISSOR  
    printfn "%A" <| parrot ROCK []
    printfn "%A" <| parrot PAPER [(ROCK, SCISSOR); (PAPER, ROCK)]
    printfn "%A" <| beatingStrat []
    printfn "%A" <| beatingStrat [(SCISSOR, PAPER);]
    printfn "%A" <| beatingStrat [(SCISSOR, PAPER); (ROCK, ROCK)]
    let strat = roundRobin [PAPER; ROCK; ROCK]
    printfn "%A" <| strat []
    printfn "%A" <| strat []
    printfn "%A" <| strat []
    printfn "%A" <| strat []
    bestOutOf strat strat |> Seq.item 0 |> printfn "(0,0) = %A"
    bestOutOf (fun _ -> ROCK) (fun _ -> ROCK) |> Seq.item 5 |> printfn "(0,0) = %A"
    bestOutOf (fun _ -> ROCK) (fun _ -> PAPER) |> Seq.item 5 |> printfn "(0,5) = %A"
    bestOutOf (fun _ -> ROCK) (fun _ -> SCISSOR) |> Seq.item 5 |> printfn "(5,0) = %A"
    bestOutOf (roundRobin [ROCK; PAPER; SCISSOR]) (parrot ROCK) |> Seq.item 50 |> printfn "(49,0) = %A"
    bestOutOf (roundRobin [ROCK; ROCK; SCISSOR]) beatingStrat |> Seq.item 50 |> printfn "(16,33) = %A"
    let rock : strategy = fun _ -> ROCK
    let paper : strategy = fun _ -> PAPER
    let scissors : strategy = fun _ -> SCISSOR
    playTournament 5 [rock] |> printfn "Some 0 = %A"
    playTournament 5 [rock; paper; scissors] |> printfn "Some 2 = %A"
    playTournament 5 [for i in 1..1000 do yield rock; yield paper; yield scissors]|> printfn "Some 2013 = %A"
    ()

let testQ4 () =
    // place debug prints for Q4 here
    push 5 >>>= push 6 >>>= pop |> evalSM |> printfn "Some (6, {5}) = %A"
    pop |> evalSM |> printfn "None = %A"
    ()

[<EntryPoint>]
let main argv =
    testQ4 ()
    0 // return an integer exit code
