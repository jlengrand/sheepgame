module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onKeyDown)
import Element exposing (Element, Orientation(..), centerX, fill, height, width)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, r, rx, ry, x, y)


type Direction
    = Left
    | Up
    | Right
    | Down
    | Other


type alias Model =
    { tick : Int
    , entities : Entities
    , gameSettings : GameSettings
    }


type alias GameSettings =
    { size : ( Int, Int )
    , color : String
    }


type Component
    = KeyboardComponent
    | AreaComponent Int Int Int Int AreaStyling
    | ScoreComponent
    | LocationComponent Location LocationStyling
    | RenderComponent (List Component -> Svg.Svg Msg)


type alias Location =
    { x : Int
    , y : Int
    , speed : Float
    }


type alias Entities =
    List Entity


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
    [ { entityType = Sheep, components = [ LocationComponent (Location 100 200 0) sheepStyling ] }, { entityType = Sheep, components = [ LocationComponent (Location 300 400 0) sheepStyling ] } ]


startingDog =
    [ { entityType = Dog
      , components = [ LocationComponent (Location 50 50 0) dogStyling, KeyboardComponent ]
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
      , gameSettings = { size = ( 600, 600 ), color = "#bdb2ff" }
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
            ( { model | entities = handleKeyPress direction model.entities }, Cmd.none )


handleKeyPress : Direction -> Entities -> Entities
handleKeyPress direction entities =
    List.map
        (\e ->
            if hasKeyboardComponent e then
                { e | components = updateLocationComponent e.components direction }

            else
                e
        )
        entities


hasKeyboardComponent : Entity -> Bool
hasKeyboardComponent entity =
    List.any
        (\c ->
            case c of
                KeyboardComponent ->
                    True

                _ ->
                    False
        )
        entity.components


updateLocationComponent : List Component -> Direction -> List Component
updateLocationComponent components direction =
    List.map
        (\c ->
            case c of
                LocationComponent location styling ->
                    LocationComponent (findNewLocation direction location) styling

                _ ->
                    c
        )
        components


findNewLocation : Direction -> Location -> Location
findNewLocation direction location =
    -- TODO: Loads, for now we don't use speed at all
    case direction of
        Up ->
            { location | x = location.x + 10 }

        Down ->
            { location | x = location.x - 10 }

        Left ->
            { location | x = location.y - 10 }

        Right ->
            { location | x = location.y + 10 }

        Other ->
            location



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
        backgroundRectangle =
            Svg.rect
                [ Svg.Attributes.height <| String.fromInt <| Tuple.first model.gameSettings.size
                , Svg.Attributes.width <| String.fromInt <| Tuple.second model.gameSettings.size
                , Svg.Attributes.fill "#bdb2ff"
                ]
                []

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
        ([ backgroundRectangle ]
            ++ (List.filterMap identity <|
                    List.map render renderComponents
               )
        )


render : Component -> Maybe (Svg Msg)
render zeComponent =
    case zeComponent of
        LocationComponent location styling ->
            Just <|
                Svg.circle
                    [ cx <| String.fromInt location.x
                    , cy <| String.fromInt location.y
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
        LocationComponent _ _ ->
            True

        AreaComponent _ _ _ _ _ ->
            True

        _ ->
            False
