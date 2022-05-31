open System
open Exam2020_2

let testQ1() =
    (* Testsfor Q1.1 *)
    let t1 = insert 5 Leaf
    let t2 = insert 3 t1
    let t3 = insert 4 t2
    let t4 = insert 10 t3
    
    printfn "Node (Leaf,5,Leaf) = %A" t1
    printfn "Node (Node (Leaf,3,Leaf),5,Leaf) = %A" t2
    printfn "Node (Node (Leaf,3,Node (Leaf,4,Leaf)),5,Leaf) = %A" t3
    printfn "Node (Node (Leaf,3,Node (Leaf,4,Leaf)),5,Node (Leaf,10,Leaf)) = %A" t4
    ()

let testQ2() =
    // place debug prints for Q2 here
     
    fromList [5;3;4;10] |> printfn "Node (Node (Leaf,3,Node (Leaf,4,Leaf)),5,Node (Leaf,10,Leaf)) = %A"
    ()

let testQ3() =
    // place debug prints for Q3 here
    printfn "old"
    fold (fun acc x -> (x - acc)) 0 (fromList [3;5;4;10]) |> printfn "6 = %A"
    foldBack (fun acc x -> x - acc) 0 (fromList [3;5;4;10]) |> printfn "-6 = %A"
    inOrder (fromList [5;3;4;10]) |> printfn "[3; 4; 5; 10] = %A"
    ()

let testQ4() =
    printfn "new"
    // place debug prints for Q4 here
    printfn "Node (Node (Leaf,-10,Leaf),-5,Node (Node (Leaf,-4,Leaf),-3,Leaf)) = "
    printfn "%A" (map (fun x -> -x) (fromList [5;3;4;10]))
    ()

[<EntryPoint>]
let main argv =
    testQ4()
    "0" |> fromString |> toString |> printfn "0 = %A"
    "120" |> fromString |> toString |> printfn "120 = %A"
    "12345689123456789" |> fromString |> toString |> printfn "12345689123456789 = %A"
    "12345689123456789" |> fromString |> add ("12345689123456789" |> fromString) |> printfn "%A"
    add (fromString "15") (fromString "39") |> toString |> printfn "54 = %A"
    add (fromString "9995") (fromString "8") |> toString |> printfn "10003 = %A"
    add (fromString "123456789123456789")
      (fromString "987654321987654321") |> toString = "1111111111111111110"  |> printfn "1111111111111111110 = %A"
    multSingle (fromString "15") 2 |> printfn "%A"
    multSingle (fromString "4") 8 |> toString |> printfn "32 = %A"
    multSingle (fromString "424") 0 |> toString |> printfn "0 = %A"
    multSingle (fromString "123456789123456789") 9 |> toString |> printfn "1111111102111111101 = %A"
    mult (fromString "12") (fromString "2") |> toString |> printfn "24 = %A"
    mult (fromString "45") (fromString "956") |> toString |> printfn "43020 = %A"
    mult (fromString "123456789123456789")
        (fromString "987654321987654321") |> toString = "121932631356500531347203169112635269" |> printfn "121932631356500531347203169112635269 = %A"
    fact 0 1 |> toString |> printfn "1 = %A"
    fact 10 1 |> toString |> printfn "3628800 = %A"
    fact 10 2 |> toString |> printfn "3628800 = %A"
    fact 10 5 |> toString |> printfn "3628800 = %A"
    fact 10 10 |> toString |> printfn "3628800 = %A"
    fact 20 10 |> toString |> printfn "2432902008176640000 = %A"
    printfn "Question 4.2"
    let (hd, tl) = step llzero
    printfn "0 = %A" hd
    let (hd1, tl1) = step tl
    printfn "0 = %A" hd1
    let (hd2, tl2) = step tl1
    printfn "0 = %A" hd2
    let (hd, tl) = step (cons 42 llzero)
    printfn "42 = %A" hd
    let (hd1, tl1) = step tl
    printfn "0 = %A" hd1
    let (hd, tl) = step (init (fun x -> x % 3))
    printfn "0 = %A" hd
    let (hd1, tl1) = step tl
    printfn "1 = %A" hd1
    let (hd2, tl2) = step tl1
    printfn "2 = %A" hd2
    let (hd3, tl3) = step tl2
    printfn "0 = %A" hd3
    printfn "Question 4.3"
    let (hd, tl) = init id |> llmap (fun x -> x % 2 = 0) |> step
    printfn "true = %A" hd
    let (hd1, tl1) = step tl
    printfn "false = %A" hd1
    let (hd2, tl2) = step tl1
    printfn "true = %A" hd2
    let (hd, tl) = init id |> filter (fun x -> x % 2 = 0) |> step
    printfn "0 = %A" hd
    let (hd1, tl1) = step tl
    printfn "2 = %A" hd1
    let (hd2, tl2) = step tl1
    printfn "4 = %A" hd2
    printfn "Question 4.5"
    let (hdlst, tl) = init id |> takeFirst 10
    printfn "[0; 1; 2; 3; 4; 5; 6; 7; 8; 9] = %A" hdlst
    let (hd1, tl1) = step tl
    printfn "10 = %A" hd1
    printfn "Question 4.6"
    let (hd, tl) = step (unfold (fun st -> (st, st + 5)) 0)
    printfn "0 = %A" hd
    let (hd1, tl1) = step tl
    printfn "5 = %A" hd1
    let (hd2, tl2) = step tl1
    printfn "10 = %A" hd2
    
    
    fact 200 10 |> toString |> printfn "%s"

    
    
    
    
    0 // return an integer exit code
