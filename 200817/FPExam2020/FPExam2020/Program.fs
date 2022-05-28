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
      (fromString "987654321987654321") |> toString |> printfn "1111111111111111110 = %A"
    0 // return an integer exit code
