module Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =  a >>= fun x -> b >>= fun y -> ret (x+y)
    let sub a b =  a >>= fun x -> b >>= fun y -> ret (x-y)
    let div a b = a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (x / y) else fail DivisionByZero    
    let mul a b =  a >>= fun x -> b >>= fun y -> ret (x*y)
    
    let modulo a b = a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (x % y) else fail DivisionByZero   

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV exp -> arithEval exp >>= pointValue
        | Add (a, b) -> add (arithEval a) (arithEval b)
        | Sub (a, b) -> sub (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mul (a, b) -> mul (arithEval a) (arithEval b)
        | CharToInt cExp -> (charEval cExp) >>= (fun s -> ret (int s))
        | Mod (a, b) -> modulo (arithEval a) (arithEval b)
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV aExp -> (arithEval aExp) >>= characterValue
        | ToUpper cExp -> (charEval cExp) >>= (fun c -> ret (System.Char.ToUpper c))
        | ToLower cExp -> (charEval cExp) >>= (fun c -> ret (System.Char.ToLower c))
        | IntToChar aExp -> (arithEval aExp) >>= (fun i -> ret (char (int '0' + i)))
    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (aExp1, aExp2) -> arithEval aExp1 >>= (fun a -> arithEval aExp2 >>= (fun b -> ret (a = b)))
        | ALt (aExp1, aExp2) -> arithEval aExp1 >>= (fun a -> arithEval aExp2 >>= (fun b -> ret (a < b)))
        | Not exp -> boolEval exp >>= (fun b -> ret (not b))
        | Conj (bExp1, bExp2) -> boolEval bExp1 >>= (fun a -> boolEval bExp2 >>= (fun b -> ret (a && b)))
        | IsDigit cExp -> charEval cExp >>= (fun c -> ret (System.Char.IsDigit c))
        | IsLetter cExp -> charEval cExp >>= (fun c -> ret (System.Char.IsLetter c))
        | IsVowel cExp -> charEval cExp >>= (fun c -> ret (not ("BCDFGHJKLMNPQRSTVWXYZ".Contains(System.Char.ToUpper(c)))))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    