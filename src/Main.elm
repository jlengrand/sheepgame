module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame, onKeyDown)
import Direction2d
import Element exposing (Element, Orientation(..), centerX, fill)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, r, rx, ry, x, y)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type TopLeftCoordinates
    = TopLeftCoordinates


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
    , lastTick : Posix
    , currentDirection : Maybe Direction
    }


type alias GameSettings =
    { size : ( Int, Int )
    , color : String
    }


type Component
    = KeyboardComponent
    | AreaComponent Int Int Int Int AreaStyling
    | ScoreComponent
    | LocationComponent KinematicState LocationStyling
    | RenderComponent (List Component -> Svg.Svg Msg)


type alias KinematicState =
    { position : Point2d Pixels TopLeftCoordinates
    , velocity : Vector2d Pixels TopLeftCoordinates
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
    [ { entityType = Sheep, components = [ LocationComponent (KinematicState (Point2d.pixels 200 100) (Vector2d.pixels 0 0)) sheepStyling ] }
    , { entityType = Sheep, components = [ LocationComponent (KinematicState (Point2d.pixels 300 400) (Vector2d.pixels 0 0)) sheepStyling ] }
    ]


startingDog =
    [ { entityType = Dog
      , components = [ LocationComponent (KinematicState (Point2d.pixels 50 50) (Vector2d.pixels 0 0)) dogStyling, KeyboardComponent ]
      }
    ]


target =
    [ { entityType = Target
      , components = [ AreaComponent 50 50 100 100 areaStyling ]
      }
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tick = 0
      , entities = startingSheeps ++ startingDog ++ target
      , gameSettings = { size = ( 600, 600 ), color = "#bdb2ff" }
      , lastTick = Time.millisToPosix 0
      , currentDirection = Maybe.Nothing
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
    Sub.batch
        [ onKeyDown (Json.Decode.map KeyPressed keyDecoder)
        , onAnimationFrame NewFrame
        ]


type Msg
    = KeyPressed Direction
    | NewFrame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed direction ->
            ( { model | currentDirection = Just direction }, Cmd.none )

        NewFrame tick ->
            ( { model
                | lastTick = tick
                , currentDirection = Maybe.Nothing
                , entities = updateVelocities model.currentDirection model.entities |> updatePositions
              }
            , Cmd.none
            )


updatePositions : Entities -> Entities
updatePositions entities =
    -- TODO : Here we just update the positions based on speed (and reduce speed most likely)
    List.map
        (\e ->
            { e | components = updatePositionOfLocationComponent e.components }
        )
        entities


updateVelocities : Maybe Direction -> Entities -> Entities
updateVelocities maybeDirection entities =
    -- TODO : here we handle the change of velocity of sheeps and dogs
    case maybeDirection of
        Maybe.Nothing ->
            entities

        Just direction ->
            List.map
                (\e ->
                    if hasKeyboardComponent e then
                        { e | components = updateVelocityOfDogs e.components direction }

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


updateVelocityOfDogs : List Component -> Direction -> List Component
updateVelocityOfDogs components direction =
    List.map
        (\c ->
            case c of
                LocationComponent kinematicState styling ->
                    LocationComponent (findNewVelocityOfDog direction kinematicState) styling

                _ ->
                    c
        )
        components


findNewVelocityOfDog : Direction -> KinematicState -> KinematicState
findNewVelocityOfDog direction kstate =
    case direction of
        Up ->
            { kstate | velocity = Vector2d.pixels 0 0 }

        Down ->
            { kstate | velocity = Vector2d.pixels 0 0 }

        Left ->
            { kstate | velocity = Vector2d.pixels 0 0 }

        Right ->
            { kstate | velocity = Vector2d.pixels 0 0 }

        Other ->
            kstate


updatePositionOfLocationComponent : List Component -> List Component
updatePositionOfLocationComponent components =
    List.map
        (\c ->
            case c of
                LocationComponent location styling ->
                    LocationComponent (findNewPosition location) styling

                _ ->
                    c
        )
        components


findNewPosition : KinematicState -> KinematicState
findNewPosition location =
    -- TODO: Loads, for now we don't use velocity at all. Should update position based on velocity and position. This is the same for everyone. No specific behaviour
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
                    [ cx <| String.fromFloat (Point2d.toPixels location.position).x
                    , cy <| String.fromFloat (Point2d.toPixels location.position).y
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
