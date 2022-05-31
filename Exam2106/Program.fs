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
[0,0;0,5]@(aux (P (C (0,5), East)) [0,5] [Forward 5; TurnRight; Forward 5])]
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


//2

let foo f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m ->
            y
        | None ->
            m <- Map.add x (f x) m;
            f x
    aux
let rec bar x = 
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
and baz = foo bar

//2.1
(*
2.1.1:
    Foo = Non-recursive function
    Bar & baz = Recursive functions
2.1.2:
    Foo: Foo takes a function and a key. If the key exists then it returns the value otherwise it applies the function on the key and sets it to be the value of the key
    Baz: Uses foo and bar to calculate fibonacci of n
2.1.3:
    The mutable variable is there to keep the same map for different executions of foo
    
    If you remove the mutable keyword then it would give an error since m would be assigned a new value later in execution but that can not be done on unmutable values 
2.1.4:
    Foo = SetupMapForFunction
    Bar = CalculateFibonacciAux
    Baz = CalculateFibonacci

2.2:
2.2.1:
    "and" is used creating mutual recursive methods
2.2.2:
    Then you could not recursively run bar in baz

2.3:

2.3.1:
     This happens on the Map.tryfind pattern match since Some y is only true when the map contains the key X
2.3.2:
    This will never effect the execution of baz since m always contains the key x if Map.tryfind x in m returns some y.
        Map.tryFind x m returns some y iff. Map.containsKey x m is true
2.3.3:  
*)
let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
            let y = f x
            m <- Map.add x y m
            y
    aux
(*
    We can remove map.containskey and store the result of f x as y and reference to y when f x is needed
*)

//2.4:
(*
let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

*)
(*
2.4.1:
    Barbaz is much slower then baz since barbaz calls barbaz recusively and needs to create a new foo each time while baz only creates foo once
    
2.5:
*)
let rec bar2 x = 
        match x with
        | 0 -> 0
        | 1 -> 1
        | y -> baz2 (y - 1) + baz2 (y - 2)
    and baz2 = foo2 bar2
let bazSeq =
    Seq.initInfinite baz2

//3:

//3.1:
type element = (int * int) list

let charToInt c = (int c - int '0')
let elToString (element:element) =
    let rec addToStringXTimes (s:string) (acc:string) =
        function
        | 0 -> ""
        | 1 -> acc+s
        | x -> addToStringXTimes s (acc+s) (x-1) 
    
    List.fold (fun acc ((elId, elAmount):int*int) ->
                addToStringXTimes (string elId) acc elAmount
              ) "" element
        
let elFromString (stringEl:string) =
    List.fold (
        fun acc (char:char) ->
            let intChar = (charToInt char)
            match acc with
            | [] -> [(intChar, 1)]
            | _ ->
                match acc.Head with
                | idHead, amountHead when intChar = idHead -> (intChar, amountHead+1)::acc.Tail 
                | _ -> (intChar, 1)::acc
        ) [] (List.ofSeq stringEl)
    |> List.rev


let nextElement (element:element) =
    List.fold
        (fun acc (id, amount) ->
                    acc@[(amount,1);(id,1)]
        ) [] (elFromString <| elToString element)


"1111111111" |> elFromString |> nextElement |> elToString |> elFromString;;
"1111111111" |> elFromString |> nextElement |> nextElement |> elToString;;

"1" |> elFromString |> nextElement |> elToString;;

"1" |> elFromString |> nextElement |> nextElement;;


"1" |> elFromString |> nextElement |> nextElement |> elToString;;


"1" |> elFromString |> nextElement |> nextElement |>
 nextElement |> nextElement |> elToString;;

"1" |> elFromString |> nextElement |> nextElement |>
 nextElement |> nextElement |> nextElement |> elToString;;
 
"11111111112222222" |> elFromString |> nextElement |> elToString;;
