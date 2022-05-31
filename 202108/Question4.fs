module _202108.Question4

open Microsoft.FSharp.Core

//Question 4

type rat = int*int


let rec mkRat n d =
    match (n, d) with
    | _, d when d = 0 -> None
    | n, d when n < 0 && d < 0 -> mkRat -n -d
    | n, d when d < 0 -> mkRat -n -d
    | n, d ->
            let largest = if n >= d then n else d
            let rec findGreatestCommonDivider =
                function
                | g when n%g = 0 && d%g = 0 -> g
                | g -> findGreatestCommonDivider (g-1)
            let g = findGreatestCommonDivider largest
            Some (n/g, d/g)

let ratToString (n, d) = sprintf "%A / %A" n d

let plus ((a,b):rat) ((c,d):rat) = mkRat ((a*d)+(b*c)) (b*d)
let minus ((a,b):rat) ((c,d):rat) = mkRat ((a*d)-(b*c)) (b*d)
let mult ((a,b):rat) ((c,d):rat) = mkRat (a*c) (b*d)
let div ((a,b):rat) ((c,d):rat) = mkRat (a*d) (b*c)

type SM<'a> = SM of (rat -> ('a * rat) option)
let ret x = SM (fun st -> Some (x, st))
let bind (SM m) f =
    SM (fun st ->
        match m st with
        | None -> None
        | Some (x, st') ->
            let (SM g) = f x
            g st')
let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s

let smPlus (rat:rat) : SM<unit> =
    SM (fun state ->
            match plus state rat with
            | None -> None
            | Some rat -> Some((), rat)
        )
let smMinus (rat:rat) : SM<unit> =
    SM (fun state ->
            match minus state rat with
            | None -> None
            | Some rat -> Some((), rat)
        )
let smMult (rat:rat) : SM<unit> =
    SM (fun state ->
            match mult state rat with
            | None -> None
            | Some rat -> Some((), rat)
        )
let smDiv (rat:rat) : SM<unit> =
    SM (fun state ->
            match div state rat with
            | None -> None
            | Some rat -> Some((), rat)
        )

type StateBuilder() =

  member this.Bind(x, f)    = bind x f
  member this.Zero ()       = ret ()
  member this.Return(x)     = ret x
  member this.ReturnFrom(x) = x
  member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()
let rec calculate (opList:(rat * (rat -> SM<unit>)) list) : SM<unit>=
    state {
         match opList with
         | [] -> return ()
         | (rat, f)::opList ->
             do! f rat
             return! calculate opList
    }