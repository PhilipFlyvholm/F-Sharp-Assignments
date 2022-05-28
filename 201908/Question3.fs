module Question3

(* Question 3.1 *)

let rec fibStore: 'a -> float =
    let mutable store = Map.empty

    let aux (n: int) : float =
        match Map.tryFind n store with
        | Some v -> v
        | None ->
            let v = (fib n)
            store <- Map.add n v store
            v
    aux

and fib (a: int) : float =
    match a with
    | 0 -> float 1
    | 1 -> float 1
    | a -> fibStore (a - 1) + fibStore (a - 2)


let calculateGoldenRatio (n: int) : float = fibStore (n + 1) / (fibStore n)

(* Question 3.2 *)

let grSeq = Seq.unfold (fun state -> Some(calculateGoldenRatio state, state + 1)) 0

(* Question 3.3 *)

let goldenRectangleSeq x =
    Seq.unfold (fun state -> Some(x * (x * Seq.item state grSeq), state + 1)) 0

let goldenTriangleSeq (b: float) =
    Seq.unfold
        (fun state ->
            Some(
                b * (b * (sqrt ((pown (Seq.item state grSeq) 2) - (1.0 / 4.0)))) / 2.0,
                state + 1
            ))
        0

(* Question 3.4 *)

let goldenRectangleTriangle a =
    Seq.unfold
        (fun state ->
            let rect = a * (a * Seq.item state grSeq)
            let triangle = a * (a * (sqrt ((pown (Seq.item state grSeq) 2) - (1.0 / 4.0)))) / 2.0
            Some(
                (rect, triangle),
                state + 1
            ))
        0
