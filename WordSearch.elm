module WordSearch exposing (main)

----------------------- imports -------------------------------------------

import Browser
import Browser.Events
import Cmd.Extra as CmdE
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)
import Random.Extra exposing (..)
import Random.List exposing (..)
import Random.Char exposing (..)
import Time
import Debug
import Svg exposing (..)
import Svg.Attributes exposing (..)
import CreateArray as CA

----------------------------------------------------------------------------
---------------------------- Word Search Time -----------------------------
---------------------------------------------------------------------------

type alias Direction = CA.Direction

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

pointGenerator : Generator (Int, Int)
pointGenerator = 
  Random.map2
    (\x y -> (x, y))
    (Random.int 0 19)
    (Random.int 0 19)

dirGen : List Direction -> Generator (Maybe Direction, List Direction)
dirGen ls =
  Random.List.choose ls

letterGen : Generator Char
letterGen =
  Random.Char.char 65 90

checkPointList : List (Int, Int) -> List (Int, Int) -> Bool
checkPointList ls ws =
  case ls of
    [] -> 
      case ws of
        [] -> True
        _  -> False
    (x, y) :: rest ->
      case ws of
        [] -> False
        (x1, y1) :: rest1 ->
          if ((x1 == x) && (y == y1)) then
            checkPointList rest rest1
          else
            False

checkStringPointList 
  : (String, List (Int, Int)) -> List (String, List (Int, Int))
  -> Maybe ((String, List (Int, Int)), (List (String, List (Int, Int))))
checkStringPointList (str, ints) ls =
  case ls of
    [] -> Nothing
    (key, points) :: rest -> 
      if (key == str) then
        if (checkPointList ints points) then
          Just ((key, points), rest)
        else
          case (checkStringPointList (str, ints) rest) of
            Nothing -> Nothing
            Just (entry, remainingls) ->
              Just (entry, (key, points) :: remainingls)
      else
        case (checkStringPointList (str, ints) rest) of
          Nothing -> Nothing
          Just (entry, remainingls) ->
            Just (entry, (key, points) :: remainingls)


type alias Model =
  { text : String
  , ws : List (List (Int, Int, Char))
  , words : List String
  , currword : (String, (Int, Int))
  , wordpos : List (String, List (Int, Int))
  , currsel : (List Char, List (Int, Int))
  , foundword : (List (String, List (Int, Int)))
  , done : Bool
  , complete : Bool
  }

type Msg =
    Change String
  | Start (List String)
  | NextWord 
  | WordPoint (Int, Int)
  | WordStart (Maybe Direction, List Direction)
  | NoWords
  | Letter Char
  | Select (Char, (Int, Int))
  | Enter

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)

initModel : Model
initModel = 
  { text = ""
  , ws = []
  , words = []
  , currword = ("blank", (-1, -1))
  , wordpos = []
  , currsel = ([], [])
  , foundword = []
  , done = False
  , complete = False
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Change s ->
      ( { text = s
        , ws = model.ws
        , words = model.words
        , currword = model.currword
        , wordpos = model.wordpos
        , currsel = model.currsel
        , foundword = model.foundword
        , done = model.done
        , complete = False
        }
      , Cmd.none)
    Start ls ->
      ( { text = model.text
        , ws = (CA.createW 20 20)
        , words = ls
        , currword = ("blank", (-1, -1))
        , wordpos = []
        , currsel = ([], [])
        , foundword = []
        , done = False
        , complete = False
        }
      , CmdE.perform NextWord)
    NextWord -> 
      case (model.words) of
        [] -> 
          (model, CmdE.perform NoWords)
        x :: rest -> 
          ( { text = model.text
            , ws = model.ws
            , words = rest
            , currword = (x, (-1, -1))
            , wordpos = model.wordpos
            , currsel = model.currsel
            , foundword = model.foundword
            , done = False
            , complete = model.complete
            }
          , Random.generate WordPoint pointGenerator)
    WordPoint (x, y) ->
      let 
        strt = Tuple.first model.currword
      in
        ( { text = model.text
          , ws = model.ws
          , words = model.words
          , currword = (strt, (x,y))
          , wordpos = model.wordpos
          , currsel = model.currsel
          , foundword = model.foundword
          , done = model.done
          , complete = model.complete
          }
        , Random.generate 
            WordStart (dirGen (CA.allDirections model.ws (x,y) strt)))
    WordStart (dir, ls) ->
      case dir of
        Nothing -> 
          ( { text = model.text
            , ws = model.ws
            , words = model.words
            , currword = model.currword
            , wordpos = model.wordpos
            , currsel = model.currsel
            , foundword = model.foundword
            , done = model.done
            , complete = model.complete
            }
          , Random.generate WordPoint pointGenerator)
        Just x ->
          let 
            wordp = 
              CA.placeWord model.ws (Tuple.first model.currword) x
                (Tuple.second model.currword)
          in
            ( { text = model.text
              , ws = (Tuple.first wordp)
              , words = model.words
              , currword = ("blank", (-1, -1))
              , wordpos = (Tuple.second wordp) :: (model.wordpos)
              , currsel = model.currsel
              , foundword = model.foundword
              , done = model.done
              , complete = model.complete
              }
            , CmdE.perform NextWord)
    NoWords -> (model, Random.generate Letter letterGen)
    Letter l -> 
      let 
        (x, y) = CA.nextEmpty model.ws
      in
        if ((x == -1) && (y == -1)) then
          ( { text = model.text
            , ws = model.ws
            , words = model.words
            , currword = model.currword
            , wordpos = model.wordpos
            , currsel = model.currsel
            , foundword = model.foundword
            , done = True
            , complete = model.complete
            }
          , Cmd.none)
        else 
          ( { text = model.text
            , ws = (CA.changeSpot model.ws x y l)
            , words = model.words
            , currword = model.currword
            , wordpos = model.wordpos
            , currsel = model.currsel
            , foundword = model.foundword
            , done = model.done
            , complete = model.complete
            }
          , (Random.generate Letter letterGen))
    Select (sym, (x, y)) ->
      ( { text = model.text
        , ws = model.ws
        , words = model.words
        , currword = model.currword
        , wordpos = model.wordpos
        , currsel = ( (Tuple.first model.currsel) ++ [sym]
                    , (Tuple.second model.currsel) ++ [(x,y)])
        , foundword = model.foundword
        , done = model.done
        , complete = model.complete
        }
      , Cmd.none)
    Enter -> 
      let
        res = checkStringPointList 
                ( (String.fromList (Tuple.first model.currsel))
                , (Tuple.second model.currsel))
                model.wordpos
      in
        case res of
          Nothing ->
            ( { text = model.text
              , ws = model.ws
              , words = model.words
              , currword = model.currword
              , wordpos = model.wordpos
              , currsel = ([], [])
              , foundword = model.foundword
              , done = model.done
              , complete = model.complete
              }
            , Cmd.none)
          Just (x, rest) ->
            case rest of
              [] ->
                ( { text = model.text
                  , ws = model.ws
                  , words = model.words
                  , currword = model.currword
                  , wordpos = rest
                  , currsel = ([], [])
                  , foundword = x :: model.foundword
                  , done = model.done
                  , complete = True
                  }
                , Cmd.none) 
              _  ->
                ( { text = model.text
                  , ws = model.ws
                  , words = model.words
                  , currword = model.currword
                  , wordpos = rest
                  , currsel = ([], [])
                  , foundword = x :: model.foundword
                  , done = model.done
                  , complete = model.complete
                  }
                , Cmd.none)

makeString : List (String, List (Int, Int)) -> String
makeString ls =
  case ls of
    (s, xs) :: [] -> s
    (s, xs) :: rest -> s ++ ", " ++ (makeString rest)
    [] -> ""

containedIn : List (Int, Int) -> Int -> Int -> Bool
containedIn ls x y =
  case ls of
    (x1, y1) :: rest -> 
      if ((x == x1) && (y == y1)) then True
      else (containedIn rest x y)
    [] -> False

containedInFound : List (String, List (Int, Int)) -> Int -> Int -> Bool
containedInFound ls x y =
  case ls of
    [] -> False
    (str, l) :: rest -> 
      if (containedIn l x y) then True
      else (containedInFound rest x y)

chooseColor : Model -> Int -> Int -> String
chooseColor model x y =
  if (containedIn (Tuple.second model.currsel) x y) then "lightgreen"
  else if (containedInFound model.foundword x y) then "pink"
  else "white"

viewLetter : Model -> Int -> Int -> Char -> Html Msg
viewLetter mdl x y c =
  Html.button [ onClick (Select (c, (x, y)))
              , Attr.style "padding" "5px"
              , Attr.style "height" "30px"
              , Attr.style "width" "30px"
              , Attr.style "background-color" (chooseColor mdl x y)
              ]
              [Html.text (String.fromChar c)]

viewRow : Model -> List (List (Int, Int, Char)) -> Int -> Html Msg
viewRow mdl ws y =
  Html.div []
    (List.map 
       (\x -> viewLetter mdl x y (CA.checkSpot ws x y))
       (List.range 0 19)
    )

viewFull : Model -> List (List (Int, Int, Char)) -> Html Msg
viewFull mdl ws =
  Html.div []
    (List.map
       (\y -> viewRow mdl ws y)
       (List.range 0 19)
    ) 
  

view : Model -> Html Msg
view model =
  let
    text =
      Html.input [ Attr.placeholder "Words for wordsearch"
                   , Attr.value model.text
                   , onInput Change ] []
    start = 
      Html.button [onClick (Start (String.split ", " model.text))]
                  [Html.text "Generate"]
    enter =
      Html.button [onClick Enter] [Html.text "Enter"]
 
  in
    Html.main_ [Attr.class "m-15"]
      [ Html.h1 [Attr.class "text-4xl my-16"]
                [Html.text "Custom WordSearch"]
      , Html.div [Attr.class "p-4 my-4"]
                 [text, start]
      , Html.div 
          [] 
          [Html.text
            (if model.complete then "COMPLETE!"
            else "")
          ]   
      , Html.div [] (if (model.done && (not (model.complete))) then
                       [Html.text ("Current Selection: " ++ 
                               (String.fromList (Tuple.first model.currsel))
                               ), enter]
                     else [])
      , Html.div [] (if (model.done && (not (model.complete))) then 
                      [Html.text ( "Words to find: " ++
                                 (makeString model.wordpos))]
                     else [])
      , Html.div [] (if (model.done && (not (model.complete))) then
                       [Html.text ( "Words found: " ++
                                 (makeString model.foundword))]
                     else [])
      , (if model.done then (viewFull model model.ws)
         else Html.div [] [])
      ]
