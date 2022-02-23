//2.1
let rec downto1 x =
    if x > 0 then
        x :: downto1(x-1)
    else
        []
(*
downto1 5
*)
let rec downto2 =
    function
    | x when x > 0 -> x :: downto2 (x-1)
    | _ -> []
(*
downto2 5
*)

//2.2
let rec removeOddIdx = 
    function
    | [] -> []
    | x :: xs when xs.Length < 2 -> [x]
    | x :: xs -> x :: removeOddIdx xs.[1..xs.Length-1];;

(*removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
 "was"; "white"; "as"; "snow"];*)


//2.3
let rec combinePair =
    function
    | [] -> []
    | [_] -> []
    | x::y::xs -> (x, y) :: combinePair xs

(* 
combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
 "was"; "white"; "as"; "snow"]
 *)
//2.4
type complex = float * float

let mkComplex (x:float) (y:float) : complex = (x,y)

let complexToPair (x:complex) : (float*float) = (fst(x), snd(x))

let (|+|) (x:complex) (y:complex) : complex = (fst(x)+fst(y), snd(x) + snd(y))

let (|*|) (x:complex) (y:complex) : complex = 
    let a = fst x
    let b = snd x
    let c = fst y
    let d = snd y
    ((a*c)-(b*d),(b*c)+(a*d))


let (|-|) (x:complex) (y:complex) : complex = x|+|(mkComplex -(fst y) -(snd y))

let (|/|) (x:complex) (y:complex) : complex =
    let a:float = fst(y)
    let b:float = snd(y)
    if a <> 0. && b <> 0. then
        let eq1 = (a**2.0)+(b**2.0)
        x |*| mkComplex (a / eq1) (-b / eq1)
    else (0.,0.)


//2.5
let explode1 (s:string) = Seq.toList(s)

let rec explode2 = 
    function
    | s when s = "" -> []
    | s -> s.[0] :: explode2 s.[1 .. s.Length-1]

//2.6
let implode (list : char list) 
    = List.foldBack (fun c s -> string c + s ) list ""

let implodeRev (list : char list) 
    = List.foldBack (fun c s ->  s + string c) list ""

//2.7
let toUpper s = s |> explode1 |> List.map (fun c -> System.Char.ToUpper c) |> implode

//2.8
let rec ack =
    function
    | (m,n) when m = 0 ->  n+1
    | (m,n) when m > 0 && n = 0 -> ack (m - 1, 1)
    | (m,n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n-1))
    | _ -> 0
(*
ack (0,1)
ack (1,1)
*)

//2.9
let time f =
 let start = System.DateTime.Now
 let res = f ()
 let finish = System.DateTime.Now
 (res, finish - start)

(*
time (fun () -> ack (3, 11)) 
*)

let timeArg1 f a = time (fun () -> (f a))

(*
timeArg1 ack (3, 11)
*)
//2.10
let rec downto3 f n e =
    match n with
    | n when n > 0 -> downto3 f (n-1) (f n e)
    | _ -> e

let fac n = downto3 (fun n acc -> n*acc ) n 1
(*
fac 5
*)

let range g n = downto3 (fun n acc -> g n :: acc) n []
(*
range (fun x -> x*2) 5
*)

// 2.11
type word = (char * int) list

let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)]

type squareFun = word -> int -> int -> int
// 2.12
let singleLetterScore (w:word) pos acc = snd w.[pos] + acc
(*
singleLetterScore hello 4 0
singleLetterScore hello 4 42
*)
let doubleLetterScore (w:word) pos acc = (snd w.[pos] * 2) + acc
(*
doubleLetterScore hello 4 0
doubleLetterScore hello 4 42
*)
let tripleLetterScore (w:word) pos acc = (snd w.[pos] * 3) + acc
(*
tripleLetterScore hello 4 42;;
*)
//2.13
let doubleWordScore (w:word) _ acc = acc*2
let tripleWordScore (w:word) _ acc = acc*3
(*
doubleWordScore hello 4 0
tripleWordScore hello 4 0
doubleWordScore hello 12345 42
tripleWordScore hello 12345 42
*)

//2.14
open System
let isConsonant (c: char) = "BCDFGHJKLMNPQRSTVWXYZ".Contains(Char.ToUpper(c))

let oddConsonants (w:word) _ acc = 
    let amount = List.foldBack (fun (w:(char*int)) s -> if isConsonant (fst w) then (s+1) else s ) w 0
    if amount % 2 <> 0 then -acc else acc
(*
oddConsonants hello 0 50
oddConsonants hello 5 50;;
*)

type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)]
let DLS : square = [(0, doubleLetterScore)]
let TLS : square = [(0, tripleLetterScore)]
let DWS : square = SLS @ [(1, doubleWordScore)]
let TWS : square = SLS @ [(1, tripleWordScore)]

let calculatePoints squares word : int= 
    List.mapi (fun i square -> List.map (fun (p, sf) -> (p, sf word i)) square) squares |>
    List.fold (fun square acc -> square@acc) [] |> 
    List.sortBy (fun sqaure -> fst sqaure) |> 
    List.map (fun (_,x) -> x) |>
    List.fold (fun acc item -> item acc) 0

(*
calculatePoints [DLS; SLS; TLS; SLS; DWS] hello;;
calculatePoints [DLS; DWS; TLS; TWS; DWS] hello;;
*)