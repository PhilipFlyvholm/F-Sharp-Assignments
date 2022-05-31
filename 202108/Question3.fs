module _202108.Question3

//Question 2.1

let findClosestPerfectSquare x : float =
    let rec aux index current= 
        match index with
        | index when ((x - index) * (x - index)) > ((x - current) * (x - current)) -> current
        | _ ->
            let square = sqrt index
            match square with
            | square when (square % 1.0 = 0.0) -> aux (index + 1.0) index
            | _ -> aux (index + 1.0) current
    aux 0.0 0.0

let approxSquare (x:int) (num:int) : float =
    let y = findClosestPerfectSquare (float x)
    let rec aux (r:float) =
        function
        | 0 -> r
        | n ->  aux (((float x / r)+r) / 2.0) (n-1)
    aux (sqrt y) num
    
    
let quadratic (a:int) (b:int) (c:int) (num:int) =
    let t = approxSquare ((b * b)-(4*a*c)) num
    let reverseB = float -b
    let bottom = 2.0 * float(a)
    (((reverseB + t)/bottom),((reverseB - t)/bottom))


//3.3
let parQuadratic (eqs : (int * int * int) list) (numProcesses:int) (num:int) : (float * float) list =
    List.splitInto numProcesses eqs
        |> List.map (
            fun eqs ->
                async {
                    return List.fold (fun acc (a,b,c) -> acc@[(quadratic a b c num)]) List.empty eqs
                }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold (fun acc r -> acc@r) []

//3.4
open JParsec.TextParser

let spaces = many <| pchar ' '
let operator = pchar '+' <|> pchar '-'
let solveQuadratic (str:string) (num:int)=
    let str' = str + "#"
    pint32
    .>> pstring "x^2"
    .>> spaces
    .>>. operator
    .>> spaces
    .>>. pint32
    .>> pstring "x"
    .>> spaces
    .>>. operator
    .>> spaces
    .>>. pint32
    .>> spaces
    .>> pchar '='
    .>> spaces
    .>> pchar '0'
    .>> pchar '#'
    |> run
    <| str'
    |> getSuccess |>
    (fun ((((a,op1),b),op2), c) ->
        let b' = if op1 = '-' then -b else b
        let c' = if op2 = '-' then -c else c
        quadratic a b' c' num 
    )
    
    