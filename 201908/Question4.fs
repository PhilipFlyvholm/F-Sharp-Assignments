module Question4

    (* Question 4.1 *)

    type color =
        | RED
        | GREEN
        | BLUE
        | PURPLE
        | ORANGE
        | YELLOW
        
    type shape =
        | SQUARE
        | CIRCLE
        | STAR
        | DIAMOND
        | CLUB
        | CROSS
    
    type tile = color * shape
    
    let matchColor =
        function
        | "red" -> RED
        | "green" -> GREEN
        | "blue" -> BLUE
        | "yellow" -> YELLOW
        | "purple" -> PURPLE
        | "orange" -> ORANGE
        | _ -> failwith "Not a valid color"
        
    let colorToString =
        function
        | RED -> "red"
        | BLUE -> "blue"
        | ORANGE -> "orange"
        | YELLOW -> "yellow"
        | PURPLE -> "purple"
        | GREEN -> "green"
    let matchShape =
        function
        | "diamond" -> DIAMOND
        | "square" -> SQUARE
        | "circle" -> CIRCLE
        | "club" -> CLUB
        | "cross" -> CROSS
        | "star" -> STAR
        | _ -> failwith "Not a valid shape"
    
    let shapeToString =
        function
        | DIAMOND -> "diamond"
        | CLUB -> "club"
        | STAR -> "star"
        | CIRCLE -> "circle"
        | SQUARE -> "square"
        | CROSS -> "cross"
    
    let mkTile color shape : tile = (matchColor color), (matchShape shape)
    let tileToString (color, shape) =
        (colorToString color + " " + shapeToString shape)

    (* Question 4.2 *)

    let validTiles tiles (newColor, newShape) =
        let tilesLength = List.length tiles
        let (colors, shapes) = List.fold (
                                fun (colorAcc, shapeAcc) (color:color, shape:shape) ->
                                    let newColorList = if List.contains color colorAcc then colorAcc else color::colorAcc
                                    let newShapeList = if List.contains shape shapeAcc then shapeAcc else shape::shapeAcc
                                    (newColorList, newShapeList)
                                ) ([newColor], [newShape]) tiles
        match List.length colors with
        | c when c = tilesLength+1 || c = 1 || c = 0 ->
            //Colors correct
            match List.length shapes with
            | s when s = tilesLength+1 || s = 1 || s = 0 ->
                //shapes correct
                true
            | _ -> false
        | _ -> false
    let validTiles2 (tiles: tile list) (tile:tile) =
        let tilesLength = List.length tiles
        match (List.distinctBy fst (tile::tiles) |> List.length) with
        | c when c = tilesLength+1 || c = 1 || c = 0 ->
            match (List.distinctBy snd (tile::tiles) |> List.length) with
            | s when s = tilesLength+1 || s = 1 || s = 0 -> true
            | _ -> false
        | _ -> false;
        

    (* Question 4.3 optional *)

    type coord = Coord of int * int
    type board = Board of Map<coord, tile>
    type direction = Left | Right | Up | Down

    let moveCoord (Coord (x,y)) = function | Left -> (Coord (x-1,y)) | Right -> Coord(x+1,y) | Up -> Coord(x,y-1) | Down -> Coord(x,y+1)

    let collectTiles (Board b) (c:coord) d =
        let rec aux c acc =
            match Map.tryFind c b with
            | None -> acc
            | Some tile -> aux (moveCoord c d) (tile::acc)
        aux c []

    (* Question 4.4 *)

    let placeTile (Coord (x,y), tile:tile) (Board b) : board option =
        match Map.tryFind (Coord (x,y)) b with
        | Some _ -> None
        | None ->
            match validTiles2 (collectTiles (Board b) (Coord (x-1,y)) Left @ collectTiles (Board b) (Coord (x+1,y)) Right) tile with
            | false -> None
            | true ->
                match validTiles2 (collectTiles (Board b) (Coord (x,y-1)) Up @ collectTiles (Board b) (Coord (x,y+1)) Down) tile with
                | false -> None
                | true -> Some (Board (Map.add (Coord (x,y)) tile b))
            
        

    (* Question 4.5 *)

    (* You may use *either* railroad-oriented programming or computation expressions.
       You do not have to use both *)

    (* Railroad-oriented programming *)
    let ret = Some
    let bind f =
        function
        | None   -> None
        | Some x -> f x
    let (>>=) x f = bind f x

    (* Computation expressions *)
    type opt<'a> = 'a option
    type OptBuilderClass() =
        member t.Return (x : 'a) : opt<'a> = ret x
        member t.ReturnFrom (x : opt<'a>) = x
        member t.Bind (o : opt<'a>, f : 'a -> opt<'b>) : opt<'b> = bind f o
    let opt = new OptBuilderClass()

    let placeTiles l (b:board) =
        List.fold (
            fun (acc:board option) i ->
                acc >>= placeTile i
        ) (ret b) l