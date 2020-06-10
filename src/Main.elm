module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Debug
import Dict exposing (Dict)
import Element exposing (Element, Orientation(..), alignRight, centerX, centerY, el, fill, fillPortion, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Svg
import Svg.Attributes exposing (cx, cy, height, r, width)
import Task


type alias Model =
    { tick : Int
    , sheeps : Flock
    , dog : Dog
    , target : Target
    , gameSettings : GameSettings
    }


type alias GameSettings =
    { size : ( Int, Int )
    }


type Component
    = KeyboardComponent
    | SpriteComponent
    | AreaComponent Int Int Int Int
    | ScoreComponent
    | LocationComponent Int Int


type alias Entity =
    List Component


type alias Dog =
    Entity


type alias Sheep =
    Entity


type alias Target =
    Entity


type alias Flock =
    List Sheep



-- Entity : { position: 2DPoint, velocity: Vector, components:List component }
-- update =
-- for every entity
--   new_velocity = f(velocity)
--   postion = position + new_velocity
--   f : Vector -> Vector
--   avoidDog : Dog -> Vector -> Vector
--   align : List Sheep -> Vector -> Vector
--   seperate : List Sheep -> Vector -> Vector
--   { sheep | velocity = avoidDog model.dog <| align model.sheeps <| seperate model.Sheep}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tick = 0
      , sheeps = [ [ LocationComponent 100 200 ], [ LocationComponent 300 400 ] ]
      , dog = [ LocationComponent 50 50 ]
      , target = [ AreaComponent 50 50 100 100 ]
      , gameSettings = { size = ( 600, 600 ) }
      }
    , Cmd.none
    )


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = AMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AMessage ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sheepgame"
    , body =
        [ Element.layout
            [ Font.family
                [ Font.sansSerif
                ]
            , Element.width fill
            ]
          <|
            Element.el
                [ centerX
                ]
            <|
                Element.html <|
                    gameView model
        ]
    }


gameView : Model -> Html Msg
gameView model =
    Svg.svg
        [ Svg.Attributes.height <| String.fromInt <| Tuple.first model.gameSettings.size
        , Svg.Attributes.width <| String.fromInt <| Tuple.second model.gameSettings.size
        ]
        [ Svg.circle [ cx "60", cy "60", r "50" ] [] ]
