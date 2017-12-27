import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Task
import String
import Json.Decode as Json
import Time exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> every second Tick)
    }



-- MODEL

type alias Position =
  { x : Int
  , y : Int
  }

type alias Dot =
  { val : Int
  , hover : Bool
  , p : Position
  , s : Int
  }

type alias Model =
  { dots : List Dot
  , current : Int
  }

targetSize : Int
targetSize = 25

defautDots : List Dot
defautDots =
  [ Dot 0 False (Position 0 0) targetSize
  ]

generateDots : Position -> Int -> List Dot
generateDots p x = []

init : ( Model, Cmd Msg )
init =
  ( Model defautDots 0, Cmd.none )


-- UPDATE


type Msg
  = MouseEnterDot Dot
  | MouseLeaveDot Dot
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseEnterDot dot -> ({ model | dots = [ {dot | hover = True} ]}, Cmd.none)
    MouseLeaveDot dot -> ({ model | dots = [ {dot | hover = False} ]}, Cmd.none)
    Tick t -> ({ model | current = (formatT t), dots = (updateDotV model.dots (formatT t)) }, Cmd.none)

updateDotV : List Dot -> Int -> List Dot
updateDotV dots t = List.map (\dot -> { dot | val = t}) dots

formatT : Time -> Int
formatT t = ((truncate (t / 1000)) % 10)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    (List.append
      [ h2 [] [ text "Hello Elm" ]
      ]
      (List.map viewDot model.dots)
    )

viewDot : Dot -> Html Msg
viewDot dot =
  let styleStr =
          [ ("position", "absolute")
          , ("font", "normal 15px sans-serif")
          , ("text-align", "center")
          , ("cursor", "pointer")
          , ("width", (toString dot.s) ++ "px")
          , ("height", (toString dot.s) ++ "px")
          , ("left", (toString dot.p.x) ++ "px")
          , ("top", (toString dot.p.y) ++ "px")
          , ("border-radius", (toString (toFloat(dot.s) / 2)) ++ "px")
          , ("line-height", (toString dot.s) ++ "px")
          , ("background", if dot.hover then "#ff0" else "#61dafb")
          ]
  in
  div
    [ style styleStr, onMouseEnter (MouseEnterDot dot), onMouseLeave (MouseLeaveDot dot) ]
    [ text (toString dot.val) ]
