//Assignement 1

//1.1
let sqr x = x*x

//1.2
let pow x n = System.Math.Pow(x, n)

//1.3

let rec sum = 
    function
    | 0 -> 0
    | n -> n + sum (n-1)

//1.4

let rec fib =
    function
    | 1 -> 1
    | n when n > 0 -> fib(n-1) + fib(n-2)
    | _ -> 0

//1.5
let dup s : string = s+s

//1.6
let rec dupn s = 
    function
    | n when n > 0 -> s + dupn s (n-1)
    | _ -> "" 
    

//1.7
let rec bin = 
    function
    | (_,0) -> 1
    | (n,k) when n = k -> 1
    | (n,k) when n <> 0 && k <> 0 && n > k -> bin (n-1, k-1) + bin (n-1, k)
    | _ -> 0

//1.8
let timediff (hh1,mm1) (hh2,mm2) = (mm2-mm1) + (hh2-hh1)*60

//1.9
let minutes (hh,mm) = timediff (0,0) (hh,mm)

//1.10
let curry (f : 'a * 'b -> 'c) x y = f(x,y)

let uncurry (f : 'a -> 'b -> 'c) (x, y) = f x y

//1.11
let empty (letter:char, pointValue:int) = 
    fun (pos:int) -> (letter, pointValue)

//1.12
//this is kinda a linked list
let add (newPos:int) (letter:char, pointValue:int) (word:int->char*int) (pos:int) = 
    if pos = newPos 
    then (letter,pointValue) 
    else word pos

//1.13
let theLetterH : int -> char * int = empty ('H', 4)
let theLetterE = add 1 ('E', 1) theLetterH
let theLetterL1 = add 2 ('L', 1) theLetterE
let theLetterL2 = add 3 ('L', 1) theLetterL1
let hello = add 4 ('O', 1) theLetterL2


//1.14
let singleLetterScore (w : int -> char * int) pos = snd(w pos)
let doubleLetterScore (w : int -> char * int) pos = snd(w pos) * 2
let trippleLetterScore (w : int -> char * int) pos = snd(w pos) * 3