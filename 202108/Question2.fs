module _202108.Question2

let rec foo xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs, y :: ys when x < y ->
    x :: (foo xs (y :: ys))
  | x :: xs, y :: ys ->
    y :: (foo (x :: xs) ys)

let rec bar =
  function
  | [] -> []
  | [x] -> [x]
  | xs ->
    let (a, b) = List.splitAt (List.length xs / 2) xs
    foo (bar a) (bar b)

//question 2.1
//2.1.1 LOOK AT RIDER
//2.1.2 It sorts the list by merging it
//2.1.3 Bar = SplitAndSort Foo = Sort
//2.1.4 a = leftSort b = rightSort

//question 2.2
// and makes the functions mutually recursive meaning you can fx. run function a and b recursively
// It still works since foo does not need to run bar and foo is already defined when running bar

let foo2 xs ys =
  List.unfold (
    fun state ->
      match state with
      | [], y::ys -> Some(y, ([], ys))
      | x::xs, [] -> Some(x, (xs, []))
      | x::xs, y::ys when x < y -> Some(x, (xs, y::ys))
      | x::xs, y::ys -> Some(y, (x::xs, ys))
      | _ -> None
    ) (xs,ys)


let fooTail xs ys =
  let rec aux f xs ys =
    match xs, ys with
    | [], ys -> f ys
    | xs, [] -> f xs
    | x :: xs, y :: ys when x < y -> aux (fun r -> f <| x::r) xs (y::ys)
    | x :: xs, y :: ys -> aux (fun r -> f <| y::r) (x::xs) ys
  aux id xs ys