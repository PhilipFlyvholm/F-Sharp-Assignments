module Question1

    type Sum<'A, 'B> =
    | Left of 'A
    | Right of 'B

    (* Question 1.1 *)

    let sum1 = Left [1;2;3]
    let sum2 = Right (Some true)

    let sumMap f g =
        function
        | Left x ->  f x
        | Right x -> g x

    (* Question 1.2 *)

    type SumColl<'A, 'B> =
    | Nil
    | CLeft of 'A * SumColl<'A, 'B>
    | CRight of 'B * SumColl<'A, 'B>

    let sumColl = CLeft ([true], CRight (1, Nil))

    let rec ofList =
        function
        | [] -> Nil
        | x::xs ->
            match x with
            | Left x -> CLeft (x, ofList xs)
            | Right x -> CRight (x, ofList xs)

    (* Question 1.3 *)
    let reverse sumColl =
        let rec aux acc sumColl'=
            match sumColl' with
            | Nil -> acc
            | CLeft(a, sumColl) -> aux (CLeft(a, acc)) sumColl
            | CRight(b, sumColl) -> aux (CRight(b, acc)) sumColl
        aux Nil sumColl

    (* Question 1.4 *)

    let ofList2 l =
        List.foldBack (fun i acc ->
            match i with
            | Left x -> CLeft (x, acc)
            | Right x -> CRight (x, acc)
        ) l Nil 

    (* Question 1.5 *)

    let foldBackSumColl f g startColl startAcc =
        let rec aux coll acc =
            match coll with
            | Nil -> acc
            | CLeft(a, sumColl) -> f a (aux sumColl acc)
            | CRight(b, sumColl) -> g b (aux sumColl acc)
        
        aux startColl startAcc