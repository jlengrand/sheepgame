module Main exposing (..)

import Angle
import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame, onKeyDown)
import Circle2d exposing (Circle2d)
import Direction2d
import Element exposing (Element, Orientation(..), centerX, fill)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, transform, width, x, xlinkHref, y)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


frictionRate =
    0.96


windowSize =
    { width = 600, height = 600 }


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
    | BodyComponent KinematicState CircleStyling
    | AvoidComponent AvoiderSettings
    | AvoideeComponent String


type alias KinematicState =
    { position : Point2d Pixels TopLeftCoordinates
    , velocity : Vector2d Pixels TopLeftCoordinates
    , circleRadius : Float
    , collides : Bool
    , max_v : Quantity.Quantity Float Pixels
    }


type alias KinematicStateAndBlockRadius =
    { kinematicState : KinematicState
    , blockRadius : Int
    }


type alias AvoiderSettings =
    { strength : Float
    , avoid_radius : Float
    , avoidee_id : String
    }


type alias Entities =
    List Entity


type alias AreaStyling =
    { color : String
    , patternName : Maybe String
    }


type alias CircleStyling =
    { radius : Float
    , color : String
    , imagePath : Maybe String
    }


treeStyling =
    CircleStyling 37.5 "#caffbf" <| Just "/static/objects/tree.png"


sheepStyling =
    CircleStyling 17.5 "#9bf6ff" <| Just "/static/animals/goat.png"


dogStyling =
    CircleStyling 17.5 "#ffc6ff" <| Just "/static/animals/dog.png"


areaStyling =
    AreaStyling "#fdffb6" <| Just "dirt"


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


dogAvoiderSettings =
    { strength = 100
    , avoid_radius = 150
    , avoidee_id = "dog"
    }


treeAvoiderSettings =
    { strength = 50
    , avoid_radius = 40
    , avoidee_id = "tree"
    }


rangeStep : Float -> Float -> Float -> List Float
rangeStep from to step =
    List.map (\i -> toFloat i * step) <|
        List.range 0 <|
            floor (to / step)


startingTrees =
    List.map
        (\( x, y ) ->
            { entityType = Tree
            , components =
                [ BodyComponent (KinematicState (Point2d.pixels x y) (Vector2d.pixels 0 0) 15 True (Pixels.pixels 0)) <| CircleStyling 15 "#caffbf" <| Just "/static/objects/tree.png"
                ]
            }
        )
    <|
        List.map
            (\x -> Tuple.pair x 0)
            (rangeStep 0 windowSize.width 30)
            ++ List.map
                (\x -> Tuple.pair x windowSize.height)
                (rangeStep 0 windowSize.width 30)
            ++ List.map
                (\y -> Tuple.pair windowSize.width y)
                (rangeStep 0 windowSize.height 30)
            ++ List.map
                (\y -> Tuple.pair 0 y)
                (rangeStep 0 windowSize.height 30)


playfieldTrees =
    [ { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 400 130) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0)) treeStyling
            , AvoideeComponent "tree"
            ]
      }
    , { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 380 480) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0)) treeStyling
            , AvoideeComponent "tree"
            ]
      }
    , { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 200 320) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0)) treeStyling
            , AvoideeComponent "tree"
            ]
      }
    ]


startingSheeps =
    [ { entityType = Sheep
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 300 300) (Vector2d.pixels 0 0.1) 17.5 True (Pixels.pixels 1)) sheepStyling
            , AvoidComponent dogAvoiderSettings

            -- , AvoidComponent treeAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 260 350) (Vector2d.pixels 0.1 0) 17.5 True (Pixels.pixels 1)) sheepStyling
            , AvoidComponent dogAvoiderSettings

            -- , AvoidComponent treeAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 250 300) (Vector2d.pixels 0 0.1) 17.5 True (Pixels.pixels 1)) sheepStyling
            , AvoidComponent dogAvoiderSettings

            -- , AvoidComponent treeAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 250 375) (Vector2d.pixels 0.1 0) 17.5 True (Pixels.pixels 1)) sheepStyling
            , AvoidComponent dogAvoiderSettings

            -- , AvoidComponent treeAvoiderSettings
            ]
      }
    ]


startingDog =
    [ { entityType = Dog
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 50 50) (Vector2d.pixels 0 0) 17.5 False (Pixels.pixels 75)) dogStyling
            , KeyboardComponent
            , AvoideeComponent "dog"
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
      , entities = startingSheeps ++ startingDog ++ target ++ startingTrees ++ playfieldTrees
      , gameSettings = { size = ( windowSize.width, windowSize.height ), color = "#bdb2ff" }
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
    let
        colliders =
            findColliders entities
    in
    List.map
        (\e ->
            { e | components = updatePositionOfBodyComponent colliders e.components }
        )
        entities



-- findStaticEntities


findColliders : Entities -> List KinematicState
findColliders entities =
    List.filter isAColider entities
        |> List.concatMap
            (\entity ->
                entity.components
                    |> List.filterMap
                        (\c ->
                            case c of
                                BodyComponent ks _ ->
                                    Just ks

                                _ ->
                                    Nothing
                        )
            )


updatePositionOfBodyComponent : List KinematicState -> List Component -> List Component
updatePositionOfBodyComponent colliders components =
    List.map
        (\c ->
            case c of
                BodyComponent location styling ->
                    BodyComponent (findNewPositionMaybeBlocked colliders location) styling

                _ ->
                    c
        )
        components


findNewPositionMaybeBlocked : List KinematicState -> KinematicState -> KinematicState
findNewPositionMaybeBlocked colliders kinematicState =
    let
        newPosition =
            findNewPosition kinematicState

        blockers =
            List.filter (\b -> circlesCollide kinematicState b) colliders
    in
    case List.head blockers of
        Just blocker ->
            findNewPosition <|
                findNewPosition
                    { kinematicState
                        | velocity =
                            Vector2d.withLength (Pixels.pixels 1.3) <| Maybe.withDefault Direction2d.x <| Vector2d.direction <| Vector2d.from blocker.position kinematicState.position
                    }

        _ ->
            newPosition


circlesCollide : KinematicState -> KinematicState -> Bool
circlesCollide p1 p2 =
    if p1.position == p2.position then
        False

    else
        let
            threshold =
                (p1.circleRadius + p2.circleRadius) ^ 2

            p1c =
                Point2d.toPixels p1.position

            p2c =
                Point2d.toPixels p2.position

            distance =
                abs <| ((p1c.x - p2c.x) ^ 2) + ((p1c.y - p2c.y) ^ 2)
        in
        distance <= threshold


findNewPosition : KinematicState -> KinematicState
findNewPosition kinematicState =
    let
        scaled_velocity =
            if Quantity.greaterThan kinematicState.max_v (Vector2d.length kinematicState.velocity) then
                Maybe.withDefault kinematicState.velocity <|
                    Maybe.map (Vector2d.withLength kinematicState.max_v) <|
                        Vector2d.direction kinematicState.velocity

            else
                kinematicState.velocity
    in
    { kinematicState
        | position = Point2d.translateBy scaled_velocity kinematicState.position
        , velocity = Vector2d.scaleBy frictionRate scaled_velocity
    }


updateVelocities : Maybe Direction -> Entities -> Entities
updateVelocities maybeDirection entities =
    List.map
        (\e ->
            if hasKeyboardComponent e then
                { e | components = updateVelocityOfDogs e.components maybeDirection }

            else if hasAvoidComponent e then
                let
                    avoiders =
                        getAvoiders e

                    avoideeLocations =
                        List.map2 Tuple.pair avoiders (List.map (getAvoideesLocation entities) avoiders)
                in
                { e | components = List.foldl avoid e.components avoideeLocations }

            else
                e
        )
        entities


avoid : ( AvoiderSettings, List KinematicState ) -> List Component -> List Component
avoid ( settings, avoidees ) avoider =
    let
        avoiderPostion =
            List.filterMap getKinematicState avoider
    in
    case List.head avoiderPostion of
        Nothing ->
            avoider

        Just avoiderKs ->
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
                                        if distancevalue > settings.avoid_radius then
                                            0

                                        else
                                            -- distancevalue / settings.strength
                                            0
                                in
                                distance
                                    |> Vector2d.direction
                                    |> Maybe.map (Vector2d.withLength (Pixels.pixels scaled))
                                    |> Maybe.withDefault (Vector2d.pixels 0 0)
                                    |> Vector2d.reverse
                            )
                        |> Vector2d.sum

                steering =
                    desired |> Vector2d.plus avoiderKs.velocity
            in
            applyForce avoider desired


applyForce : List Component -> Vector2d Pixels TopLeftCoordinates -> List Component
applyForce components force =
    components
        |> List.map
            (\c ->
                case c of
                    BodyComponent ks styling ->
                        BodyComponent
                            { ks | velocity = Vector2d.plus ks.velocity force }
                            -- { ks | velocity = force }
                            styling

                    _ ->
                        c
            )


getAvoideesLocation : Entities -> AvoiderSettings -> List KinematicState
getAvoideesLocation entities avoider =
    let
        avoidees =
            List.filter (hasAvoideeString avoider.avoidee_id) <| List.filter hasAvoideeComponent entities
    in
    avoidees
        |> List.concatMap
            (\entity ->
                entity.components
                    |> List.filterMap
                        getKinematicState
            )


getAvoiders : Entity -> List AvoiderSettings
getAvoiders e =
    e.components
        |> List.filterMap
            (\c ->
                case c of
                    AvoidComponent avoider ->
                        Just avoider

                    _ ->
                        Nothing
            )


getKinematicState : Component -> Maybe KinematicState
getKinematicState c =
    case c of
        BodyComponent k _ ->
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


isAColider : Entity -> Bool
isAColider entity =
    List.any
        (\c ->
            case c of
                BodyComponent ks _ ->
                    ks.collides

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
                AvoideeComponent _ ->
                    True

                _ ->
                    False
        )
        entity.components


hasAvoideeString : String -> Entity -> Bool
hasAvoideeString id entity =
    List.any
        (\c ->
            case c of
                AvoideeComponent id_ ->
                    id == id_

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
                        BodyComponent kinematicState styling ->
                            BodyComponent (findNewVelocityOfDog direction kinematicState) styling

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
                , Svg.Attributes.fill "url(#grass)"
                ]
                []

        patternDefs =
            Svg.defs [] <|
                List.map
                    (\id ->
                        Svg.pattern
                            [ Svg.Attributes.id id
                            , Svg.Attributes.x "0"
                            , Svg.Attributes.y "0"
                            , Svg.Attributes.width "64"
                            , Svg.Attributes.height "64"
                            , Svg.Attributes.patternUnits "userSpaceOnUse"
                            , Svg.Attributes.patternContentUnits "Default"
                            ]
                            [ Svg.image
                                [ width "64"
                                , height "64"
                                , xlinkHref <| "static/tiles/" ++ id ++ ".png"
                                ]
                                []
                            ]
                    )
                    [ "grass", "dirt" ]

        renderComponents =
            model.entities
                |> List.concatMap
                    (\entity ->
                        entity.components
                            |> List.filter
                                (\c -> isRenderable c)
                    )
    in
    Svg.svg
        [ Svg.Attributes.height <| String.fromInt <| Tuple.first model.gameSettings.size
        , Svg.Attributes.width <| String.fromInt <| Tuple.second model.gameSettings.size
        ]
        ([ patternDefs, backgroundRectangle ]
            ++ (List.filterMap identity <|
                    List.map render <|
                        List.sortBy zOrder renderComponents
               )
        )


zOrder : Component -> Int
zOrder component =
    -- I guess we have to add all 'renderables' here
    case component of
        BodyComponent _ _ ->
            0

        AreaComponent _ _ _ _ _ ->
            -1

        _ ->
            999


createRotationString : KinematicState -> CircleStyling -> String
createRotationString kstate styling =
    let
        x =
            (Point2d.toPixels kstate.position).x

        y =
            (Point2d.toPixels kstate.position).y

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
        BodyComponent location styling ->
            case styling.imagePath of
                Just path ->
                    Just <|
                        -- Svg.g [] [
                        Svg.image
                            [ x <| String.fromFloat <| (Point2d.toPixels location.position).x - styling.radius
                            , y <| String.fromFloat <| (Point2d.toPixels location.position).y - styling.radius
                            , width <| String.fromFloat (styling.radius * 2)
                            , height <| String.fromFloat (styling.radius * 2)
                            , xlinkHref path
                            , transform <| createRotationString location styling
                            ]
                            []

                --         Svg.circle
                --             [ cx <| String.fromFloat (Point2d.toPixels location.position).x
                --             , cy <| String.fromFloat (Point2d.toPixels location.position).y
                --             , r <| String.fromFloat location.circleRadius
                --             , Svg.Attributes.stroke styling.color
                --             , Svg.Attributes.strokeWidth "2"
                --             , Svg.Attributes.fill "none"
                --             ]
                --             []
                -- -- ]
                _ ->
                    Just <|
                        Svg.circle
                            [ cx <| String.fromFloat (Point2d.toPixels location.position).x
                            , cy <| String.fromFloat (Point2d.toPixels location.position).y
                            , r <| String.fromFloat styling.radius
                            , Svg.Attributes.fill styling.color
                            ]
                            []

        AreaComponent zeX zeY zeWidth zeHeight styling ->
            let
                fillStyling =
                    case styling.patternName of
                        Just id ->
                            Svg.Attributes.fill <| "Url(#" ++ id ++ ")"

                        Nothing ->
                            Svg.Attributes.fill styling.color
            in
            Just <|
                Svg.rect
                    [ x <| String.fromInt zeX
                    , y <| String.fromInt zeY
                    , Svg.Attributes.width <| String.fromInt zeWidth
                    , Svg.Attributes.height <| String.fromInt zeHeight
                    , fillStyling
                    , rx "0"
                    , ry "0"
                    ]
                    []

        _ ->
            Nothing


isRenderable : Component -> Bool
isRenderable component =
    case component of
        BodyComponent _ _ ->
            True

        AreaComponent _ _ _ _ _ ->
            True

        _ ->
            False
