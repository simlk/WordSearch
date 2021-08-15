module CreateArray exposing (..)

--------------------------- imports ---------------------------------------

--import Random exposing (Generator)

--------------------------------------------------------------------------
-------------  Creating the Framework for a 2d array ---------------------
---------------------------------------------------------------------------

-- type Wordsrch = List (List (Int, Int, Char))

{- Helper for emptyW -}
fillCol : Int -> Int -> Int -> (List (Int, Int, Char))
fillCol x y r2 = 
  if y < r2 then 
    (x, y, '_') :: (fillCol x (y + 1) r2)
  else
    []

{- helper for createW -}
emptyW : Int -> Int -> Int -> Int -> (List (List (Int, Int, Char)))
emptyW x y r1 r2 =
  if x < r1 then
    (fillCol x y r2) :: (emptyW (x + 1) y r1 r2)
  else
    []

{- creates wordsearch of size x, y -}
createW : Int -> Int -> (List (List (Int, Int, Char)))
createW x y =
  emptyW 0 0 x y

{- determines the size of a column -}
sizeY : (List (Int, Int, Char)) -> Int
sizeY ls =
  case ls of
    (x, y, sym) :: [] -> (y + 1)
    x :: rest         -> sizeY rest
    []                -> 0

{- determines the size of a row -}
sizeX : (List (List (Int, Int, Char))) -> Int
sizeX ws =
  case ws of
    ((x, y, sym) :: rest) :: [] -> (x + 1)
    x :: rest                   -> sizeX rest
    []                          -> 0

{- determines with size of wordsearch with help of sizex and sizey -}
sizeW : (List (List (Int, Int, Char))) -> (Int, Int)
sizeW ws =
  case ws of
    col :: rest -> (sizeX (col :: rest), sizeY col)
    []          -> (0, 0)


{- iterates through column and takes a char if the y matches -}
checkColumn : List (Int, Int, Char) -> Int -> Char
checkColumn ws y =
  case ws of
    (w, z, sym) :: rest ->
      if z == y then sym
      else checkColumn rest y
    [] -> Debug.todo "checkColumn"

{- same as checkcolumn but returns something with matching sym -}
checkColumnOpp : List (Int, Int, Char) -> Char -> (Int, Int)
checkColumnOpp ws sym =
  case ws of
    (w, z, sym1) :: rest ->
      if sym == sym1 then (w, z)
        else checkColumnOpp rest sym
    [] -> (-1, -1)

{- iterates through every row and column and returns the char match -}
checkSpot
  : (List (List (Int, Int, Char))) -> Int -> Int -> Char
checkSpot ws x y =
  case ws of
    ((w, z, sym) :: rest) :: ws_rest ->
      if w == x then checkColumn ((w, z, sym) :: rest) y
      else checkSpot ws_rest x y
    _ -> '?'

checkSpotOpp : (List (List (Int, Int, Char))) -> Char -> (Int, Int)
checkSpotOpp ws sym = 
  case ws of
    (w :: rest) ->
      let
        (x, y) = checkColumnOpp w sym
      in
        if ((x == -1) && (y == -1)) then
          checkSpotOpp rest sym
        else (x, y)
    [] -> (-1 , -1)

{- gives the next empty space -}
nextEmpty : (List (List (Int, Int, Char))) -> (Int, Int)
nextEmpty ws = 
  checkSpotOpp ws '_'

{- helper for changeSpot -}
changeColumn 
  : (List (Int, Int, Char)) -> Int -> Char -> (List (Int, Int, Char))
changeColumn ws y sym =
  case ws of
    (w, z, sym1) :: rest ->
      if z == y then ((w, z, sym) :: rest)
      else (w, z, sym1) :: (changeColumn rest y sym)
    _ -> Debug.todo "changeColumn"

{- changes the character in a spot and returns full wordsearch -}
changeSpot
  : (List (List (Int, Int, Char))) -> Int -> Int -> Char 
  -> (List (List (Int, Int, Char)))
changeSpot ws x y sym =
  case ws of
    ((w, z, sym1) :: rest) :: ws_rest ->
      if w == x 
        then (changeColumn ((w, z, sym1) :: rest) y sym) :: ws_rest
      else
        ((w, z, sym1) :: rest) :: (changeSpot ws_rest x y sym)
    _ -> Debug.todo "changeSpot"

---------------------------------------------------------------------------
--------------------All Word-based Functions ------------------------------
---------------------------------------------------------------------------

{- defining a type for choosing the direction a word goes -}
type Direction = NN | NE | EE | SE | SS | SW | WW | NW

----------------------------------------------------------------------------
-------------------- Directional Scan Functions ----------------------------
----------------------------------------------------------------------------

{- scan the direction to see if it is viable -}
scanDirection 
  : (List (List (Int, Int, Char))) -> String -> (Int, Int) 
  -> (Int, Int) -> Direction -> Bool
scanDirection ws wrd point sze dir =
  case dir of
    NN -> scanNorth ws wrd point sze
    NE -> scanNorthEast ws wrd point sze
    EE -> scanEast ws wrd point sze
    SE -> scanSouthEast ws wrd point sze
    SS -> scanSouth ws wrd point sze
    SW -> scanSouthWest ws wrd point sze
    WW -> scanWest ws wrd point sze
    NW -> scanNorthWest ws wrd point sze

{- scan in the north direction. y - 1 -}
scanNorth 
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanNorth ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) -> 
      if y < 0 then False
      else 
        if (checkSpot ws x y) == '_' then
          scanNorth ws rest (x, (y - 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanNorth ws rest (x, (y - 1)) (sx, sy)
        else {- checkspot != '_' or xym -}
          False
    Nothing -> True

{- scan in the North East direction. x + 1, y - 1 -}
scanNorthEast 
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanNorthEast ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if ((y < 0) || (x >= sx)) then False
      else
        if (checkSpot ws x y) == '_' then
          scanNorthEast ws rest ((x + 1), (y - 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanNorthEast ws rest ((x + 1), (y - 1)) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- scan in the East direction. x + 1 -}
scanEast
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanEast ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if (x >= sx) then False
      else
        if (checkSpot ws x y) == '_' then
          scanEast ws rest ((x + 1), y) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanEast ws rest ((x + 1), y) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- scan in the South East direction. x + 1, y + 1 -}
scanSouthEast
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanSouthEast ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if ((x >= sx) || (y >= sy)) then False
      else
        if (checkSpot ws x y) == '_' then
          scanSouthEast ws rest ((x + 1), (y + 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanSouthEast ws rest ((x + 1), (y + 1)) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- scan in the South direction. y + 1 -}
scanSouth
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanSouth ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if (y >= sy) then False
      else
        if (checkSpot ws x y) == '_' then
          scanSouth ws rest (x, (y + 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanSouth ws rest (x, (y + 1)) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- scan in the SouthWest direction. x - 1, y + 1 -}
scanSouthWest
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanSouthWest ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if ((x < 0) || (y >= sy)) then False
      else
        if (checkSpot ws x y) == '_' then
          scanSouthWest ws rest ((x - 1), (y + 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanSouthWest ws rest ((x - 1), (y + 1)) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- scan in the West direction. x - 1 -}
scanWest
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanWest ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if (x < 0) then False
      else
        if (checkSpot ws x y) == '_' then
          scanWest ws rest ((x - 1), y) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanWest ws rest ((x - 1), y) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

{- Scan in the North West direction. x - 1, y - 1 -}
scanNorthWest 
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> (Int, Int) -> Bool
scanNorthWest ws wrd (x, y) (sx, sy) = 
  case (String.uncons wrd) of
    Just (sym, rest) ->
      if ((x < 0) || (y < 0)) then False
      else
        if (checkSpot ws x y) == '_' then
          scanNorthWest ws rest ((x - 1), (y - 1)) (sx, sy)
        else if (checkSpot ws x y) == sym then
          scanNorthWest ws rest ((x - 1), (y - 1)) (sx, sy)
        else {- checkspot != '_' or sym -}
          False
    Nothing -> True

allDirectionsHelper
  : (List (List (Int, Int, Char))) -> List Direction -> (Int, Int)
  -> String -> List Direction
allDirectionsHelper ws alldir strt wrd =
  case alldir of
    x :: rest -> 
      if (scanDirection ws wrd strt (sizeW ws) x) then
        x :: (allDirectionsHelper ws rest strt wrd)
      else
         allDirectionsHelper ws rest strt wrd
    [] -> []

{- Checks every direction to see which ones are viable -}
allDirections
  : (List (List (Int, Int, Char))) -> (Int, Int) -> String -> List Direction
allDirections ws strt wrd =
  let
    alldir = [NN, NE, EE, SE, SS, SW, WW, NW]
  in
    allDirectionsHelper ws alldir strt wrd

----------------------------------------------------------------------------
--------------------- Word Placement Functions -----------------------------
----------------------------------------------------------------------------

{- builds word north. y - 1 -}
placeWordNN
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordNN ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordNN ws rest (x, (y - 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word northeast. x + 1, y - 1 -}
placeWordNE
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordNE ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordNE ws rest ((x + 1), (y - 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word east. x + 1 -}
placeWordEE
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordEE ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordEE ws rest ((x + 1), y)
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word southeast. x + 1, y + 1 -}
placeWordSE
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordSE ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordSE ws rest ((x + 1), (y + 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word south. y + 1 -}
placeWordSS
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordSS ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordSS ws rest (x, (y + 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word southwest. x - 1, y + 1 -}
placeWordSW
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordSW ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordSW ws rest ((x - 1), (y + 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word west. x - 1 -}
placeWordWW
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordWW ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordWW ws rest ((x - 1), y)
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- builds word northwest. x - 1, y - 1 -}
placeWordNW
  : (List (List (Int, Int, Char))) -> String -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (List (Int, Int)))
placeWordNW ws wrd (x, y) =
  case (String.uncons wrd) of
    Just (sym, rest) ->
      let 
        place_rest = placeWordNW ws rest ((x - 1), (y - 1))
        rest_ws = Tuple.first place_rest
        rest_ls = Tuple.second place_rest
      in
        ((changeSpot rest_ws x y sym), (x, y) :: rest_ls)
    Nothing -> (ws, [])

{- helper for the placeWord function -}
placeWordHelper 
  : (List (List (Int, Int, Char))) -> String -> Direction -> (Int, Int) 
  -> ((List (List (Int, Int, Char))), (String, (List (Int, Int))))
placeWordHelper ws wrd dir strt =
  case dir of
    NN -> 
      let
        placed = placeWordNN ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    NE -> 
      let
        placed = placeWordNE ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    EE -> 
      let
        placed = placeWordEE ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    SE -> 
      let
        placed = placeWordSE ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    SS -> 
      let
        placed = placeWordSS ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    SW -> 
      let
        placed = placeWordSW ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    WW -> 
      let
        placed = placeWordWW ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))
    NW -> 
      let
        placed = placeWordNW ws wrd strt
        placed_ws = Tuple.first placed
        placed_ls = Tuple.second placed
      in
        (placed_ws, (wrd, placed_ls))


{- places a word based off of list direction and starting point -}
{- returns the wordsearch paired with a pair of the word and    -}
{- the list of points that match to it                          -}
placeWord 
  : (List (List (Int, Int, Char))) -> String -> Direction -> (Int, Int)
  -> ((List (List (Int, Int, Char))), (String, (List (Int, Int))))
placeWord ws wrd dir strt =
  placeWordHelper ws wrd dir strt
