import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (beginnerProgram)
import Element exposing (..)
import Array
import Collage exposing (..)
import Text exposing (..)
import Color exposing (..)
import List exposing (..)
import String exposing (toInt)

type alias StickState = List (List Int)
type alias Model = { disksCount : Int, trace : Array.Array StickState, step : Int }
type alias DiskMove = (Int, Int)
type Msg = SetDisks Int | ShowStep Int | NullMsg

intMsg msg str =
  case (toInt str) of
    Ok i -> msg i
    _ -> NullMsg

main =
  beginnerProgram { model = initial, view = view, update = update }

initial : Model
initial = initialState 3

initialState : Int -> Model
initialState n =
  { disksCount = n
  , trace = Array.fromList <| performMoves (setupDisks n) (hanoi n 1 3 2)
  , step = 0 }

setupDisks : Int -> StickState
setupDisks n = [[1..n],[],[]]

performMoves : StickState -> List DiskMove -> List StickState
performMoves state moves =
  case moves of
    [] -> [state]
    m::rest -> state::(performMoves (towerMove m state) rest)

hanoi : Int -> Int -> Int -> Int -> List DiskMove
hanoi n a b c =
  case n of
    0 -> []
    _ -> hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

towerMove : DiskMove -> StickState -> StickState
towerMove m l =
  case (m,l) of
    ((1,2), [e::l1,l2,l3]) -> [l1,e::l2,l3]
    ((1,3), [e::l1,l2,l3]) -> [l1,l2,e::l3]
    ((2,3), [l1,e::l2,l3]) -> [l1,l2,e::l3]
    ((2,1), [l1,e::l2,l3]) -> [e::l1,l2,l3]
    ((3,1), [l1,l2,e::l3]) -> [e::l1,l2,l3]
    ((3,2), [l1,l2,e::l3]) -> [l1,e::l2,l3]
    _ -> l

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetDisks i -> initialState i
    ShowStep i -> { model | step = i }
    NullMsg -> model

width = 600
height = 300
stick_height = height - table_height - 20
stick_width = 10
table_y_from_center = (-stick_height / 2)
table_height = 20
disk_width = 0.8 * width / 3
stick_gap = (width - 3 * disk_width) / 6
stick_delta = disk_width + 2 * stick_gap
stick_x = map (\x -> width/2 + x*stick_delta) [-1,0,1]
disk_heigth = 15

llrect c w h = move (w/2,h/2) <| filled c <| rect w h
ll00 = move (-width/2,-height/2)
t = Html.text

view : Model -> Html Msg
view model =
  let
    state = Maybe.withDefault (setupDisks 1) (Array.get model.step model.trace)
    maxStep = Array.length model.trace - 1
    prevStep = Basics.max 0 (model.step - 1)
    nextStep = Basics.min maxStep (model.step + 1) in
  div [] [
    h1 [] [t "Towers of Hanoi"],
    span [] [
      t "Number of disks",
      input [
        type' "number",
        value (toString model.disksCount),
        onInput (intMsg SetDisks)
      ] [],
      t (" Step " ++ (toString model.step) ++ "/" ++ (toString maxStep)),
      button [onClick <| ShowStep prevStep] [t "<"],
      button [onClick <| ShowStep nextStep] [t ">"],
      input [
        type' "range",
        value (toString model.step),
        Html.Attributes.min (toString 0),
        Html.Attributes.max (toString maxStep),
        onInput (intMsg ShowStep)
      ] []
    ],
    (toHtml <| collage width 300 <| map ll00 <| [
        llrect blue width table_height
        ]
        ++ (map (\x -> move (x - stick_width/2, table_height) <| llrect black stick_width stick_height) stick_x)
        ++ (map (\ (pieces,x) -> move (x, table_height) <| disks (disk_sizer model.disksCount) pieces) (zipModel state stick_x)))
  ]

zipModel : List a -> List b -> List (a,b)
zipModel a b =
  case a of
    [l1,l2,l3] ->
      case b of
        [x1,x2,x3] -> [(l1,x1),(l2,x2),(l3,x3)]
        _ -> []
    _ -> []

disks disk_sizer pieces  =
  group <| indexedMap (\i p -> disk disk_sizer ((length pieces) - i) p) pieces
disk disk_sizer p size =
  let self_width = disk_sizer size in
    move (-self_width/2, ((toFloat p)-0.9) * (disk_heigth * 1.1)) <| llrect red self_width disk_heigth

disk_sizer : Int -> Int -> Float
disk_sizer max_disk size =
  let min_width = stick_width * 1.2 in
  (disk_width - min_width) / (toFloat max_disk) * (toFloat size) + min_width
