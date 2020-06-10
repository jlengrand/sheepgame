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
import Task


type alias Model =
    { val : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { val = "Hello, world!"
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
    {title="Sheepgame"
    , body =
    [Element.layout
        [ Font.family
            [ Font.sansSerif
            ]
        , width fill
        ]
    <|
        Element.el
            [ centerX
            ]
            <| Element.text model.val
    ]}
