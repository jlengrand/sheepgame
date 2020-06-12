module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Element exposing (Element, Orientation(..), alignRight, centerX, centerY, el, fill, fillPortion, height, padding, rgb255, row, spacing, text, width)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes exposing (color, cx, cy, height, r, rx, ry, width, x, y)


type Direction
    = Left
    | Up
    | Right
    | Down
    | Other


type alias Model =
    { tick : Int
    , entities : List Entity
    , gameSettings : GameSettings
    }


type alias GameSettings =
    { size : ( Int, Int )
    }


type Component
    = KeyboardComponent
    | SpriteComponent
    | AreaComponent Int Int Int Int AreaStyling
    | ScoreComponent
    | LocationComponent Int Int LocationStyling
    | RenderComponent (List Component -> Svg.Svg Msg)


type alias AreaStyling =
    { color : String
    }


type alias LocationStyling =
    { radius : Int
    , color : String
    }


sheepStyling =
    LocationStyling 5 "#9bf6ff"


dogStyling =
    LocationStyling 10 "#ffc6ff"


areaStyling =
    AreaStyling "#fdffb6"


type EntityType
    = Sheep
    | Dog
    | Target


type alias Entity =
    { entityType : EntityType
    , components : List Component
    }


type alias Dog =
    Entity


type alias Sheep =
    Entity


type alias Target =
    Entity


type alias Flock =
    List Sheep


startingSheeps =
    [ { entityType = Sheep, components = [ LocationComponent 100 200 sheepStyling ] }, { entityType = Sheep, components = [ LocationComponent 300 400 sheepStyling ] } ]


startingDog =
    [ { entityType = Dog
      , components = [ LocationComponent 50 50 dogStyling ]
      }
    ]


target =
    [ { entityType = Dog
      , components = [ AreaComponent 50 50 100 100 areaStyling ]
      }
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tick = 0
      , entities = startingSheeps ++ startingDog ++ target
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


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


keyDecoder : Json.Decode.Decoder Direction
keyDecoder =
    Json.Decode.map toDirection (Json.Decode.field "key" Json.Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Json.Decode.map KeyPressed keyDecoder)


type Msg
    = KeyPressed Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed direction ->
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
    let
        renderComponents =
            model.entities
                |> List.concatMap
                    (\entity ->
                        entity.components
                            |> List.filter
                                (\c -> isLocationOrAreaComponent c)
                    )
    in
    Svg.svg
        [ Svg.Attributes.height <| String.fromInt <| Tuple.first model.gameSettings.size
        , Svg.Attributes.width <| String.fromInt <| Tuple.second model.gameSettings.size
        ]
    <|
        List.filterMap identity <|
            List.map render renderComponents


render : Component -> Maybe (Svg Msg)
render zeComponent =
    case zeComponent of
        LocationComponent zeX zeY styling ->
            Just <|
                Svg.circle
                    [ cx <| String.fromInt zeX
                    , cy <| String.fromInt zeY
                    , r <| String.fromInt styling.radius
                    , Svg.Attributes.fill styling.color
                    ]
                    []

        AreaComponent zeX zeY zeWidth zeHeight styling ->
            Just <|
                Svg.rect
                    [ x <| String.fromInt zeX
                    , y <| String.fromInt zeY
                    , Svg.Attributes.width <| String.fromInt zeWidth
                    , Svg.Attributes.height <| String.fromInt zeHeight
                    , Svg.Attributes.fill styling.color
                    , rx "0"
                    , ry "0"
                    ]
                    []

        _ ->
            Nothing


isLocationOrAreaComponent : Component -> Bool
isLocationOrAreaComponent component =
    case component of
        LocationComponent _ _ _ ->
            True

        AreaComponent _ _ _ _ _ ->
            True

        _ ->
            False
