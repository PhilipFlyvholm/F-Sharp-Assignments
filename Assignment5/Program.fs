//5.1
let sum m n =
    let rec sumC acc =
        function
        | (_,n) when n = 0 -> acc+m
        | (m,n) -> sumC (acc + (m+n)) (m, n-1)
    sumC 0 (m,n)

//5.2
let length xs =
    let rec lengthC acc = 
        function
        | [] -> acc
        | x::xs -> lengthC (acc + 1) xs
    lengthC 0 xs

//5.3
let foldBack folder lst acc =
    let rec foldBackC c =
        function
        | [] -> c acc
        | x::lst -> foldBackC (fun r -> c (folder x r)) lst
    foldBackC id lst

//5.4

let factC x = 
    let rec aux c =
        function
        | 0 -> c 1
        | x -> aux (fun r -> c (x*r)) (x-1)
    aux id x

(* The accumulator function is faster since it can add the numbers together while processing the function while the continuation function needs to process all of the tail functions *)

//5.5
let fibA x =
    let rec aux prev1 prev2 =
        function
        | i when i < x -> aux prev2 (prev1+prev2) (i+1)
        | _ -> prev1
    aux 0 1 0
(*
fibA 5
fibA 40
*)

let fibC x =
    let rec aux c =
        function
            | 0 -> c 0
            | 1 -> c 1
            | i -> aux (fun r -> aux (fun r' -> c (r+r')) (i-1)) (i-2)
    aux id x
(*
fibC 6
*)

// 5.6
(*
    This is a recursive function since it in the given function on line 4 is waiting for the response of c res
*)

//5.7
type word = (char * int) list
type aExp =
 | N of int (* Integer literal *)
 | V of string (* Variable reference *)
 | WL (* Word length *)
 | PV of aExp (* Point value lookup at word index *)
 | Add of aExp * aExp (* Addition *)
 | Sub of aExp * aExp (* Subtraction *)
 | Mul of aExp * aExp (* Multiplication *)
 | CharToInt of cExp (* NEW: Cast to integer *)
and cExp =
 | C of char (* Character literal *)
 | CV of aExp (* Character lookup at word index *)
 | ToUpper of cExp (* Convert character to upper case *)
 | ToLower of cExp (* Convert character to lower case *)
 | IntToChar of aExp (* NEW: Cast to character *)

 
let rec arithEvalSimple (a:aExp) (w:word) (s:Map<string, int>) =
    match a with
    | N n  -> n
    | V v -> if s.ContainsKey(v) then (Map.find v s) else 0
    | WL -> List.length w
    | PV v -> snd w.[arithEvalSimple v w s]
    | Add (a,b) -> (arithEvalSimple a w s) + (arithEvalSimple b w s)
    | Sub (a,b) -> (arithEvalSimple a w s) - (arithEvalSimple b w s)
    | Mul (a,b) -> (arithEvalSimple a w s) * (arithEvalSimple b w s)
    | CharToInt cExp -> int (charEvalSimple cExp w s) - int '0'
and charEvalSimple (exp:cExp) (w:word) (s:Map<string, int>) = 
    match exp with
    | C c -> c
    | ToUpper v -> System.Char.ToUpper(charEvalSimple v w s)
    | ToLower v -> System.Char.ToLower(charEvalSimple v w s)
    | CV v -> fst w.[arithEvalSimple v w s]
    | IntToChar aExp -> (char (int '0' + arithEvalSimple aExp w s))
    
let rec arithEvalTail (a:aExp) (w:word) (s:Map<string, int>) (f: int->'a) =
    match a with
    | N n -> f n
    | V v -> s.TryFind v |> Option.defaultValue 0 |> f
    | WL -> w.Length |> f
    | PV v -> arithEvalTail v w s (fun r -> f (snd w.[r]))
    | Add (a,b) -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun r' -> f (r+r')))
    | Sub (a,b) -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun r' -> f (r-r')))
    | Mul (a,b) -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun r' -> f (r*r')))
    | CharToInt cExp -> charEvalTail cExp w s (fun r ->  f (int r - int '0'))
and charEvalTail (c:cExp) (w:word) (s:Map<string, int>) (f: char->'a) =
    match c with
    | C c -> f c
    | ToUpper v -> charEvalTail v w s (fun r -> f (System.Char.ToUpper r))
    | ToLower v -> charEvalTail v w s (fun r -> f (System.Char.ToLower r))
    | CV v -> arithEvalTail v w s (fun r -> f (fst w.[r]))
    | IntToChar aExp -> arithEvalTail aExp w s (fun r -> f (char (int '0' + r)))
    
    

let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id
