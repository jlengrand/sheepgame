module Main exposing (..)

import Angle
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
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, transform, width, x, xlinkHref, y)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


frictionRate =
    0.96


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
    | LocationComponent KinematicState CircleStyling
    | AvoidComponent AvoiderSettings
    | AvoideeComponent
    | BlockComponent Int


type alias KinematicState =
    { position : Point2d Pixels TopLeftCoordinates
    , velocity : Vector2d Pixels TopLeftCoordinates
    }


type alias KinematicStateAndBlockRadius =
    { kinematicState : KinematicState
    , blockRadius : Int
    }


type alias AvoiderSettings =
    { strength : Float
    , avoid_radius : Float
    }


type alias Entities =
    List Entity


type alias AreaStyling =
    { color : String
    }


type alias CircleStyling =
    { radius : Int
    , color : String
    , imagePath : Maybe String
    }


treeStyling =
    CircleStyling 10 "#caffbf" <| Maybe.Nothing


sheepStyling =
    CircleStyling 25 "#9bf6ff" <| Just "/static/animals/elephant.png"


dogStyling =
    CircleStyling 25 "#ffc6ff" <| Just "/static/animals/snake.png"


areaStyling =
    AreaStyling "#fdffb6"


type EntityType
    = Sheep
    | Dog
    | Target
    | Tree


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


defaultAvoiderSettings =
    { strength = 100
    , avoid_radius = 200
    }


startingSheeps =
    [ { entityType = Tree
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 10 10) (Vector2d.pixels 0 0)) treeStyling
            , AvoideeComponent
            , BlockComponent 10
            ]
      }
    , { entityType = Sheep
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 200 100) (Vector2d.pixels 0 0)) sheepStyling
            , AvoidComponent defaultAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 200 150) (Vector2d.pixels 0 0)) sheepStyling
            , AvoidComponent defaultAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 250 60) (Vector2d.pixels 0 0)) sheepStyling
            , AvoidComponent defaultAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 250 30) (Vector2d.pixels 0 0)) sheepStyling
            , AvoidComponent defaultAvoiderSettings
            ]
      }
    ]


startingDog =
    [ { entityType = Dog
      , components =
            [ LocationComponent (KinematicState (Point2d.pixels 50 50) (Vector2d.pixels 0 0)) dogStyling
            , KeyboardComponent
            , AvoideeComponent
            ]
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
    List.map
        (\e ->
            { e | components = updatePositionOfLocationComponent e.components }
        )
        entities


updateVelocities : Maybe Direction -> Entities -> Entities
updateVelocities maybeDirection entities =
    List.map
        (\e ->
            if hasKeyboardComponent e then
                { e | components = updateVelocityOfDogs e.components maybeDirection }

            else if hasAvoidComponent e then
                let
                    avoideeLocations =
                        getAvoideesLocation entities
                in
                { e | components = avoid e.components avoideeLocations }

            else
                e
        )
        entities


avoid : List Component -> List KinematicState -> List Component
avoid avoider avoidees =
    let
        avoiderPostion =
            List.filterMap getKinemeticState avoider

        avoiderAvoiderSettings =
            List.filterMap getAvoiderSettings avoider
    in
    -- We only use the first one!
    case List.head avoiderPostion of
        Nothing ->
            avoider

        Just avoiderKs ->
            case List.head avoiderAvoiderSettings of
                Just avoiderAs ->
                    let
                        desired =
                            avoidees
                                |> List.map
                                    (\avoidee ->
                                        let
                                            distance =
                                                Vector2d.from avoiderKs.position avoidee.position

                                            distancevalue =
                                                Pixels.inPixels (Vector2d.length distance)

                                            scaled =
                                                if distancevalue > avoiderAs.avoid_radius then
                                                    0

                                                else
                                                    avoiderAs.strength / distancevalue
                                        in
                                        distance
                                            |> Vector2d.direction
                                            |> Maybe.map (Vector2d.withLength (Pixels.pixels scaled))
                                            |> Maybe.withDefault (Vector2d.pixels 0 0)
                                            |> Vector2d.reverse
                                    )
                                |> Vector2d.sum
                    in
                    applyForce avoider desired

                Nothing ->
                    avoider


applyForce : List Component -> Vector2d Pixels TopLeftCoordinates -> List Component
applyForce components force =
    components
        |> List.map
            (\c ->
                case c of
                    LocationComponent ks styling ->
                        LocationComponent
                            { position = ks.position
                            , velocity = Vector2d.scaleBy 0.35 (Vector2d.plus ks.velocity force)
                            }
                            styling

                    _ ->
                        c
            )


desiredAvoid : Point2d Pixels TopLeftCoordinates -> KinematicState -> Vector2d Pixels TopLeftCoordinates
desiredAvoid myPosition avoideePostion =
    Vector2d.from myPosition avoideePostion.position



--getBlocksKinematicStateAndRadius : Entities -> List KinematicStateAndBlockRadius
--getBlocksKinematicStateAndRadius entities =
--    let
--        blocks =
--            List.filter hasBlockComponent entities
--    in
--    blocks
--        |> List.concatMap
--            (\entity ->
--                entity.components
--                    |> List.filterMap
--                        getKinemeticState
--            )


getAvoideesLocation : Entities -> List KinematicState
getAvoideesLocation entities =
    let
        avoidees =
            List.filter hasAvoideeComponent entities
    in
    avoidees
        |> List.concatMap
            (\entity ->
                entity.components
                    |> List.filterMap
                        getKinemeticState
            )


getKinemeticState : Component -> Maybe KinematicState
getKinemeticState c =
    case c of
        LocationComponent k _ ->
            Just k

        _ ->
            Nothing


getAvoiderSettings : Component -> Maybe AvoiderSettings
getAvoiderSettings c =
    case c of
        AvoidComponent a ->
            Just a

        _ ->
            Nothing


hasBlockComponent : Entity -> Bool
hasBlockComponent entity =
    List.any
        (\c ->
            case c of
                BlockComponent _ ->
                    True

                _ ->
                    False
        )
        entity.components


hasAvoidComponent : Entity -> Bool
hasAvoidComponent entity =
    List.any
        (\c ->
            case c of
                AvoidComponent _ ->
                    True

                _ ->
                    False
        )
        entity.components


hasAvoideeComponent : Entity -> Bool
hasAvoideeComponent entity =
    List.any
        (\c ->
            case c of
                AvoideeComponent ->
                    True

                _ ->
                    False
        )
        entity.components


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


updateVelocityOfDogs : List Component -> Maybe Direction -> List Component
updateVelocityOfDogs components mdirection =
    case mdirection of
        Just direction ->
            List.map
                (\c ->
                    case c of
                        LocationComponent kinematicState styling ->
                            LocationComponent (findNewVelocityOfDog direction kinematicState) styling

                        _ ->
                            c
                )
                components

        Nothing ->
            components


findNewVelocityOfDog : Direction -> KinematicState -> KinematicState
findNewVelocityOfDog direction kstate =
    case direction of
        Up ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Vector2d.pixels 0 -2 }

        Down ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Vector2d.pixels 0 2 }

        Left ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Vector2d.pixels -2 0 }

        Right ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Vector2d.pixels 2 0 }

        Other ->
            kstate


updatePositionOfLocationComponent : List Component -> List Component
updatePositionOfLocationComponent components =
    --let
    --    blockComponents =
    --        components.
    --in
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
findNewPosition kinematicState =
    { kinematicState
        | position = Point2d.translateBy kinematicState.velocity kinematicState.position
        , velocity = Vector2d.scaleBy frictionRate kinematicState.velocity
    }



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
        (backgroundRectangle
            :: (List.filterMap identity <|
                    List.map render <|
                        List.sortBy zOrder renderComponents
               )
        )


zOrder : Component -> Int
zOrder component =
    -- I guess we have to add all 'renderables' here
    case component of
        LocationComponent _ _ ->
            0

        AreaComponent _ _ _ _ _ ->
            -1

        _ ->
            999


createRotationString : KinematicState -> CircleStyling -> String
createRotationString kstate styling =
    let
        x =
            (Point2d.toPixels kstate.position).x + (toFloat styling.radius / 2)

        y =
            (Point2d.toPixels kstate.position).y + (toFloat styling.radius / 2)

        direction =
            Vector2d.direction kstate.velocity

        angle =
            -90.0
                + (case direction of
                    Just d ->
                        d
                            |> Direction2d.toAngle
                            |> Angle.inDegrees

                    _ ->
                        0
                  )
    in
    "rotate(" ++ String.fromFloat angle ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"


render : Component -> Maybe (Svg Msg)
render zeComponent =
    case zeComponent of
        LocationComponent location styling ->
            case styling.imagePath of
                Just path ->
                    Just <|
                        Svg.image
                            [ x <| String.fromFloat (Point2d.toPixels location.position).x
                            , y <| String.fromFloat (Point2d.toPixels location.position).y
                            , width <| String.fromInt styling.radius
                            , height <| String.fromInt styling.radius
                            , xlinkHref path
                            , transform <| createRotationString location styling
                            ]
                            []

                _ ->
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
