// For more information see https://aka.ms/fsharp-console-apps
open Question1

printfn "Question 1.1"
sumMap List.length String.length (Left [1; 2; 3]) |> printfn "3 = %A"
sumMap List.length String.length (Right "Hello World!") |> printfn "12 = %A"

printfn "Question 1.2"
ofList [Left "Hello"; Right [1; 2; 3]; Left " world!!!"] |> printfn "CLeft (\"Hello\", CRight ([1; 2; 3], CLeft (\" world!!!\", Nil))) =  %A"

printfn "Question 1.3"
reverse (CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))) |> printfn " CLeft (\" world!!!\", CRight ([1; 2; 3], CLeft (\"Hello\", Nil))) = %A"

printfn "Question 1.4"
ofList2 [Left "Hello"; Right [1; 2; 3]; Left " world!!!"] |> printfn "CLeft (\"Hello\", CRight ([1; 2; 3], CLeft (\" world!!!\", Nil))) =  %A"

printfn "Question 1.5"
let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))
foldBackSumColl 
    (fun s acc   -> String.length s + acc) 
    (fun lst acc -> List.length lst - acc)
    coll
    0
    |> printfn "7 = %A"
    
open CodeComprehension
printfn "Question 2.4"
fTail "abba" |> printfn "%A"

printfn "Question 2.5"
gOpt "abba" |> printfn "True = %A"
gOpt "abbda" |> printfn "False = %A"
gOpt "AbBa" |> printfn "true = %A"
gOpt "" |> printfn "true = %A"
gOpt "Dromedaren Alpotto planerade mord!!!" |> printfn "true = %A"


open Question3
printfn "Question 3.1"
fib 10 |> printfn "%A"

calculateGoldenRatio 0 |> printfn "1 = %A"
calculateGoldenRatio 3 |> printfn "1.666666667 = %A"
calculateGoldenRatio 42 |> printfn "1.618033989 = %A"

printfn "Question 3.3"
goldenRectangleSeq 2.5 |> printfn "%A"
goldenTriangleSeq 2.5 |> printfn "%A"
goldenRectangleTriangle 2.5 |> printfn "%A"

open Question4
printfn "Question 4.2"
let bD = (BLUE, DIAMOND)
let rD = (RED, DIAMOND)
let pD = (PURPLE, DIAMOND)
validTiles [bD; rD] pD |> printfn "True = %A"
validTiles [bD; rD] bD |> printfn "False = %A"
let pS = (PURPLE, STAR)
let pC = (PURPLE, CLUB)
validTiles [pS; pD] pC |> printfn "True = %A"
validTiles [pS; pD] pS |> printfn "False = %A"

printfn "Second test"
validTiles2 [bD; rD] pD |> printfn "True = %A"
validTiles2 [bD; rD] bD |> printfn "False = %A"
validTiles2 [pS; pD] pC |> printfn "True = %A"
validTiles2 [pS; pD] pS |> printfn "False = %A"

let board2 =
    [(Coord (-1, -1), mkTile "blue" "diamond"); 
     (Coord ( 0, -1), mkTile "red" "diamond");
     (Coord ( 1, -1), mkTile "purple" "diamond");

     (Coord (-1, 0), mkTile "blue" "circle"); 
     (Coord ( 1, 0), mkTile "purple" "circle");
         
     (Coord (-1, 1), mkTile "blue" "square"); 
     (Coord ( 0, 1), mkTile "red" "square");
     (Coord ( 1, 1), mkTile "purple" "square")] |>
         
    Map.ofList |> 
    Board

let boardToPPFormat (Board m) =
    m |> Map.toList |> List.map (fun (c, t) -> (c, tileToString t))

printfn "Some = %A" 
    (placeTile (Coord (0, 0), mkTile "red" "circle") board2 |>
     Option.map boardToPPFormat)
printfn "None = %A" 
    (placeTile (Coord (0, 0), mkTile "blue" "circle") board2 |>
     Option.map boardToPPFormat)
printfn "None = %A" 
    (placeTile (Coord (0, 0), mkTile "red" "diamond") board2 |>
     Option.map boardToPPFormat)
printfn "None = %A" 
    (placeTile (Coord (1, 1), mkTile "red" "circle") board2 |>
     Option.map boardToPPFormat)
let board3 =
    [(Coord (-1, -1), mkTile "blue" "diamond"); 
     (Coord ( 0, -1), mkTile "red" "diamond");
     (Coord ( 1, -1), mkTile "purple" "diamond");
     
     (Coord (-1, 1), mkTile "blue" "square"); 
     (Coord ( 0, 1), mkTile "red" "square");
     (Coord ( 1, 1), mkTile "purple" "square")] |>
         
    Map.ofList |> 
    Board

printfn "Last boi"
let boardToPPFormat2 (Board m) = // Other name for CodeJudge technical reasons
    m |> Map.toList |> List.map (fun (c, t) -> (c, tileToString t))
    
printfn "Some = %A"
    (placeTiles [] board3 |>
     Option.map boardToPPFormat2)
printfn "Some = %A"
    (placeTiles [(Coord (0, 0), mkTile "red" "circle")] board3 |>
     Option.map boardToPPFormat2)
printfn "None = %A"
    (placeTiles [(Coord (0, 0), mkTile "blue" "circle")] board3 |>
     Option.map boardToPPFormat2)
printfn "None = %A"
    (placeTiles [(Coord (0, 0), mkTile "red" "diamond")] board3 |>
     Option.map boardToPPFormat2)
printfn "Some = %A"
    (placeTiles [(Coord (-1, 0), mkTile "blue" "circle"); 
                 (Coord (0, 0), mkTile "red" "circle");
                 (Coord (1, 0), mkTile "purple" "circle")] board3 |>
     Option.map boardToPPFormat2)
printfn "None = %A"
    (placeTiles [(Coord (-1, 0), mkTile "red" "circle"); 
                 (Coord (0, 0), mkTile "blue" "circle");
                 (Coord (1, 0), mkTile "purple" "circle")] board3 |>
     Option.map boardToPPFormat2)
printfn "None = %A"
    (placeTiles [(Coord (-1, 1), mkTile "blue" "circle"); 
                 (Coord (0, 1), mkTile "red" "circle");
                 (Coord (1, 1), mkTile "purple" "circle")] board3 |>
     Option.map boardToPPFormat2)