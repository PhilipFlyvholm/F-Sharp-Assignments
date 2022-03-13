type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

//3.1
let rec arithEvalSimple =
    function
    | N n -> n
    | Add (a,b) -> (arithEvalSimple a) + (arithEvalSimple b)
    | Sub (a,b) -> (arithEvalSimple a) - (arithEvalSimple b)
    | Mul (a,b) -> (arithEvalSimple a) * (arithEvalSimple b)

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;

(*
arithEvalSimple a1;;
arithEvalSimple a2;;
arithEvalSimple a3;; 
arithEvalSimple a4;;
arithEvalSimple a5;;

*)
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;


//3.2
let rec arithEvalState =
    function
    | N n -> fun _ -> n
    | V v -> fun (s:Map<string, int>) -> if s.ContainsKey(v) then (Map.find v s) else 0
    | Add (a,b) -> fun s -> (arithEvalState a s) + (arithEvalState b s)
    | Sub (a,b) -> fun s -> (arithEvalState a s) - (arithEvalState b s)
    | Mul (a,b) -> fun s -> (arithEvalState a s) * (arithEvalState b s)

(*
    arithEvalState a6 (Map.ofList [("x", 5)]);;
    arithEvalState a6 (Map.ofList [("y", 5)]);;
    arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)]);;
    arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)]);;
*)
type word = (char * int) list
type squareFun = word -> int -> int -> int

//So the pos is stored on the key "_pos_" and the acc is stored on the key "_acc_"

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let rec arithEval (a:aExp) (w:word) (s:Map<string, int>) =
    match a with
    | N n  -> n
    | V v -> if s.ContainsKey(v) then (Map.find v s) else 0
    | PV v -> snd w.[arithEval v w s]
    | WL -> List.length w
    | Add (a,b) -> (arithEval a w s) + (arithEval b w s)
    | Sub (a,b) -> (arithEval a w s) - (arithEval b w s)
    | Mul (a,b) -> (arithEval a w s) * (arithEval b w s)

let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)]

(*
arithEval WL [] Map.empty;;
arithEval WL hello Map.empty;;
arithEval (PV (N 0)) hello Map.empty;;
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);;
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);;
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);;
*)

//3.4
type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)

let rec charEval (exp:cExp) (w:word) (s:Map<string, int>) = 
    match exp with
    | C c -> c
    | ToUpper v -> System.Char.ToUpper(charEval v w s)
    | ToLower v -> System.Char.ToLower(charEval v w s)
    | CV v -> fst w.[arithEval v w s]

(*
    charEval (C 'H') [] Map.empty;;
    charEval (ToLower (CV (N 0))) hello Map.empty;;
    charEval (ToUpper (C 'h')) [] Map.empty;;
    charEval (ToLower (C '*')) [] Map.empty;;
    charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)]);;
*)

//3.5
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval (b:bExp) (w:word) (s:Map<string,int>) =
    match b with
    | TT -> true
    | FF -> false
    | AEq (a1, a2) -> (arithEval a1 w s) = (arithEval a2 w s)
    | ALt (a1, a2) -> (arithEval a1 w s) < (arithEval a2 w s)
    | Not exp ->  not (boolEval exp w s)
    | Conj (b1, b2) -> (boolEval b1 w s) && (boolEval b2 w s)
    | IsDigit c -> System.Char.IsDigit (charEval c w s)
    | IsLetter c -> System.Char.IsLetter (charEval c w s)
    | IsVowel c -> not ("BCDFGHJKLMNPQRSTVWXYZ".Contains(System.Char.ToUpper(charEval c w s)))

(*
boolEval TT [] Map.empty;;
boolEval FF [] Map.empty;;
boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]);;
boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]);;
boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)]);;
boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);;
boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)]);;
boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);;
*)

let isConsonant (c:cExp) = Not (IsVowel c)
(*
boolEval (isConsonant (C 'H')) [] Map.empty
boolEval (isConsonant (C 'h')) [] Map.empty
boolEval (isConsonant (C 'A')) [] Map.empty
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 0)]);;
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 1)]);;
*)

type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | IT of bExp * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec evalStmnt (stm:stmnt) (w:word) (s:Map<string,int>) =
    match stm with
    | Skip -> s
    | Ass (x, a) -> s.Add(x, arithEval a w s)
    | Seq (stm1, stm2) -> evalStmnt stm2 w (evalStmnt stm1 w s)
    | ITE (guard, stm1, stm2) -> if boolEval guard w s then evalStmnt stm1 w s else evalStmnt stm2 w s
    | IT (guard, stm) -> evalStmnt (ITE (guard, stm, Skip)) w s
    | While (guard, stm) -> if (boolEval guard w s) then evalStmnt (While (guard, stm)) w (evalStmnt stm w s) else s
(*
evalStmnt Skip [] Map.empty;;
evalStmnt (Ass ("x", N 5)) [] Map.empty;;
evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty;;
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;
evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello Map.empty;;
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)]);;
*)

//3.8
let stmntToSquareFun (stm:stmnt) = fun (w:word) pos acc -> Map.find "_result_" (evalStmnt stm w (evalStmnt (Seq (Ass ("_pos_", N pos), Ass ("_acc_", N acc))) w Map.empty))

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers = 
  stmntToSquareFun 
    (Seq (Ass ("_result_", V "_acc_"),
          While (V "i" .<. WL,
                 ITE (IsDigit (CV (V "i")),
                      Seq (
                           Ass ("_result_", V "_result_" .*. N -1),
                           Ass ("i", WL)),
                      Ass ("i", V "i" .+. N 1)))))
(*
    singleLetterScore hello 0 0;;

    doubleLetterScore hello 0 0;;

    tripleLetterScore hello 0 0;;

    singleLetterScore hello 0 42;;

    doubleLetterScore hello 0 42;;

    tripleLetterScore hello 0 42;;

    containsNumbers hello 5 50;;

    containsNumbers (('0', 100)::hello) 5 50;;

    containsNumbers (hello @ [('0', 100)]) 5 50;;
*)

//3.9
let oddConsonants = 
                    (Seq (
                        Ass ("isOdd", N 1),
                        (Seq (While (V "i" .<. WL, Seq ( 
                                        IT (IsVowel (CV (V "i")),
                                            ITE (AEq (V "isOdd", N 1), Ass ("isOdd", N 0), Ass ("isOdd", N 1))
                                        ),
                                        Ass ("i", V "i" .+. N 1)
                                    )
                                ), (
                                    ITE (AEq (V "isOdd", N 1), Ass ("_result_", (V "_acc_" .*. N -1)),  Ass ("_result_", V "_acc_"))
                                )
                        ))))


(*
let helll = [('H', 4);('E', 1);('L', 1);('L', 1);('L', 1)]
stmntToSquareFun oddConsonants hello 5 50;;
stmntToSquareFun oddConsonants hello 0 50;;
stmntToSquareFun oddConsonants helll 0 50;;
*)

// 3.10

type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints squares word : int =
    List.mapi (fun i square -> List.map (fun (p, sf) -> (p, sf word i)) square) squares |>
    List.fold (fun square acc -> square@acc) [] |> 
    List.sortBy (fun sqaure -> fst sqaure) |> 
    List.map (fun (_,x) -> x) |>
    List.fold (fun acc item -> item acc) 0

let calculatePoints2 squares word : int = calculatePoints (List.map (fun s -> List.map (fun (int, stmnt) -> (int, stmntToSquareFun stmnt)) s) squares) word

(*
calculatePoints2 [DLS; SLS; TLS; SLS; DWS] hello;;
calculatePoints2 [DLS; DWS; TLS; TWS; DWS] hello;;
*)
