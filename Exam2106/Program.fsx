//1.1
type direction = North | East | South | West
type coord     = C of int * int

let move = fun dist dir (C(x,y)) ->
    match dir with
    | North -> C (x,y-dist)
    | East -> C (x+dist,y)
    | South -> C (x,y+dist)
    | West -> C (x-dist,y)
    
    
let turnRight =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
let turnLeft =
    function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

//1.2
type position = P of (coord * direction)
type move     = TurnLeft | TurnRight | Forward of int

let step (P (coord,dir)) =
    function
    | TurnLeft -> P(coord, (turnLeft dir))
    | TurnRight -> P(coord, (turnRight dir))
    | Forward dist -> P((move dist dir coord), dir)
    
    
//1.3
let rec walk pos =
    function
    | [] -> pos
    | move::moves -> walk (step pos move) moves

let walk2 = List.fold step

//1.4
let path (P (startCoord, startDir)) moves =
   let rec aux (P (coord, dir)) =
       function
       | [] -> []
       | move::moves ->
           match move with
           | TurnLeft | TurnRight -> (aux (step (P (coord,dir)) move) moves) 
           | Forward dist ->
               let (P (coord',dir')) = step (P (coord,dir)) (Forward dist)
               coord'::(aux (P (coord',dir')) moves)
   startCoord::(aux (P (startCoord, startDir)) moves)
//1.5
let path2 (P (startCoord, startDir)) moves =
   let rec aux (P (coord,dir)) acc =
       function
       | [] -> acc
       | move::moves' ->
           match move with
           | TurnLeft | TurnRight -> (aux (step (P (coord,dir)) move) acc moves') 
           | Forward dist ->
               let (P (coord',dir')) = step (P (coord,dir)) (Forward dist)
               aux (P (coord',dir')) (acc@[coord']) moves'
   aux (P (startCoord, startDir)) [startCoord] moves
//1.6
(*
If we have the following call as an example: path (P (C (0, 0), North)) [Forward 5; TurnRight; Forward 5];;
Then we can evaluate it on the path function:
[0,0]::aux (P (C (0, 0), North)) [Forward 5; TurnRight; Forward 5]
[0,0;0,5]::aux (P (C (0, 5), North)) [TurnRight; Forward 5]
[0,0;0,5]::aux (P (C (0, 5), East)) [Forward 5]
[0,0;0,5;0,10]::aux (P (C 0,10), East)) []
[0,0;0,5;0,10]

Then we can evaluate it on the path2 function
aux (P (C (0,0), North)) [0,0] [Forward 5; TurnRight; Forward 5]
[0,0]@(aux (P (C (0,5), North)) [0,0] [Forward 5; TurnRight; Forward 5])
[0,0]@[[0,5]@(aux (P (C (0,5), East)) [0,5] [Forward 5; TurnRight; Forward 5])]
*)
let path3 (P (startCoord, startDir)) moves =
   let rec aux f (P (coord,dir)) =
       function
       | [] -> f []
       | move::moves' ->
           match move with
           | TurnLeft | TurnRight -> (aux f (step (P (coord,dir)) move) moves') 
           | Forward dist ->
               let (P (coord',dir')) = step (P (coord,dir)) (Forward dist)
               aux (fun r -> f (coord'::r)) (P (coord',dir')) moves'
   aux (fun r -> (startCoord::r)) (P (startCoord, startDir)) moves



