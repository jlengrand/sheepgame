module Main exposing (..)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame, onKeyDown)
import Direction2d
import Element exposing (Element, Orientation(..), centerX, fill)
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import String exposing (toInt)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, transform, width, x, xlinkHref, y)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


frictionRate =
    0.98


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
    | AreaComponent BoundingBox AreaStyling
    | BodyComponent KinematicState CircleStyling
    | AvoidComponent AvoiderSettings
    | AvoideeComponent String
    | ScoreComponent Int


type alias BoundingBox =
    BoundingBox2d Pixels TopLeftCoordinates


type alias KinematicState =
    { position : Point2d Pixels TopLeftCoordinates
    , velocity : Vector2d Pixels TopLeftCoordinates
    , acceleration : Vector2d Pixels TopLeftCoordinates
    , circleRadius : Float
    , collides : Bool
    , max_v : Quantity.Quantity Float Pixels
    , max_a : Quantity.Quantity Float Pixels
    }


type alias KinematicStateAndBlockRadius =
    { kinematicState : KinematicState
    , blockRadius : Int
    }


type alias AvoiderSettings =
    { strength : Float
    , avoid_radius : Quantity.Quantity Float Pixels
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
    | Misc


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
    { strength = 1
    , avoid_radius = Pixels.pixels 170
    , avoidee_id = "dog"
    }


rangeStep : Float -> Float -> Float -> List Float
rangeStep from to step =
    List.map (\i -> from + (toFloat i * step)) <|
        List.range 0 <|
            floor ((to - from) / step)


createTreeRectangle : Float -> Float -> Float -> Float -> Entities
createTreeRectangle xMin xMax yMin yMax =
    List.map
        (\( x, y ) ->
            { entityType = Tree
            , components =
                [ BodyComponent (KinematicState (Point2d.pixels x y) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 15 True (Pixels.pixels 0) (Pixels.pixels 0)) <| CircleStyling 15 "#caffbf" <| Just "/static/objects/tree.png"
                ]
            }
        )
    <|
        List.map
            (\x -> Tuple.pair x yMin)
            (rangeStep xMin xMax 30)
            ++ List.map
                (\y -> Tuple.pair xMax y)
                (rangeStep yMin yMax 30)
            ++ List.map
                (\x -> Tuple.pair x yMax)
                (rangeStep xMin xMax 30)
            ++ List.map
                (\y -> Tuple.pair xMin y)
                (rangeStep yMin yMax 30)


targetTrees : BoundingBox -> Entities
targetTrees boundingBox =
    -- We'll accept target is a single rectangle
    -- We could partition in a smarter way but I don't want to fuck with randoms now
    let
        extrema =
            BoundingBox2d.extrema boundingBox

        trees =
            createTreeRectangle (Pixels.inPixels extrema.minX) (Pixels.inPixels extrema.maxX) (Pixels.inPixels extrema.minY) (Pixels.inPixels extrema.maxY)
    in
    List.take (floor (toFloat (List.length trees) * 0.9)) trees


startingTrees =
    createTreeRectangle 0 windowSize.width 0 windowSize.height


playfieldTrees =
    [ { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 400 130) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0) (Pixels.pixels 0)) treeStyling
            ]
      }
    , { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 380 480) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0) (Pixels.pixels 0)) treeStyling
            ]
      }
    , { entityType = Tree
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 200 320) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 37.5 True (Pixels.pixels 0) (Pixels.pixels 0)) treeStyling
            ]
      }
    ]


sheepBody x y =
    BodyComponent (KinematicState (Point2d.pixels x y) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 17.5 True (Pixels.pixels 4) (Pixels.pixels 0.1)) sheepStyling


startingSheeps =
    [ { entityType = Sheep
      , components =
            [ sheepBody 300 300
            , AvoidComponent dogAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ sheepBody 260 350
            , AvoidComponent dogAvoiderSettings
            ]
      }
    , { entityType = Sheep
      , components =
            [ sheepBody 250 300
            , AvoidComponent dogAvoiderSettings
            ]
      }
    ]


startingDog =
    [ { entityType = Dog
      , components =
            [ BodyComponent (KinematicState (Point2d.pixels 50 50) (Vector2d.pixels 0 0) (Vector2d.pixels 0 0) 17.5 False (Pixels.pixels 6) (Pixels.pixels 2)) dogStyling
            , KeyboardComponent
            , AvoideeComponent "dog"
            ]
      }
    ]


startingScore =
    [ { entityType = Misc
      , components =
            [ ScoreComponent 0
            ]
      }
    ]


startingTargetBBox =
    BoundingBox2d.from (Point2d.pixels 150 150) (Point2d.pixels 350 270)


target =
    [ { entityType = Target
      , components = [ AreaComponent startingTargetBBox areaStyling ]
      }
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tick = 0
      , entities =
            target
                ++ targetTrees startingTargetBBox
                ++ startingSheeps
                ++ startingDog
                ++ startingTrees
                ++ playfieldTrees
                ++ startingScore
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
                , entities = updateVelocities model.currentDirection model.entities |> updatePositions |> updateScore
              }
            , Cmd.none
            )


isDog : Entity -> Bool
isDog entity =
    case entity.entityType of
        Dog ->
            True

        _ ->
            False


isSheep : Entity -> Bool
isSheep entity =
    case entity.entityType of
        Sheep ->
            True

        _ ->
            False


updateScore : Entities -> Entities
updateScore entities =
    -- TODO : Do much better
    let
        dogsStates =
            entities
                |> List.filter
                    isDog
                |> List.concatMap
                    (\entity ->
                        entity.components
                            |> List.filterMap
                                getKinematicState
                    )

        sheepsStates =
            entities
                |> List.filter
                    isSheep
                |> List.concatMap
                    (\entity ->
                        entity.components
                            |> List.filterMap
                                getKinematicState
                    )

        theTarget =
            List.filter
                (\e ->
                    case e.entityType of
                        Target ->
                            True

                        _ ->
                            False
                )
                entities
                |> List.head

        theTargetBoundingBox =
            case theTarget of
                Just t ->
                    getAreaComponentOfEntity t

                Maybe.Nothing ->
                    Maybe.Nothing

        score =
            case theTargetBoundingBox of
                Just bb ->
                    Basics.max (getScore bb sheepsStates - getScore bb dogsStates) 0

                Maybe.Nothing ->
                    0
    in
    List.map
        (\e -> { e | components = updateScoreOfComponents score e.components })
        entities


getScore : BoundingBox -> List KinematicState -> Int
getScore bbox kstates =
    kstates
        |> List.map
            (\ks -> BoundingBox2d.contains ks.position bbox)
        |> List.filter
            identity
        |> List.length


getAreaComponentOfEntity : Entity -> Maybe BoundingBox
getAreaComponentOfEntity entity =
    List.filterMap
        (\c ->
            case c of
                AreaComponent bb _ ->
                    Just bb

                _ ->
                    Maybe.Nothing
        )
        entity.components
        |> List.head


updateScoreOfComponents : Int -> List Component -> List Component
updateScoreOfComponents score components =
    List.map
        (\c ->
            case c of
                ScoreComponent _ ->
                    ScoreComponent score

                _ ->
                    c
        )
        components


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


findColliders : Entities -> List KinematicState
findColliders entities =
    List.filter isACollider entities
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
                            Vector2d.withLength (Pixels.pixels 1.3) <|
                                Maybe.withDefault Direction2d.x <|
                                    Vector2d.direction <|
                                        Vector2d.from blocker.position kinematicState.position
                        , acceleration =
                            Vector2d.withLength kinematicState.max_a <|
                                Maybe.withDefault Direction2d.x <|
                                    Vector2d.direction <|
                                        Vector2d.from blocker.position kinematicState.position
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


limit : Quantity.Quantity Float Pixels -> Vector2d Pixels TopLeftCoordinates -> Vector2d Pixels TopLeftCoordinates
limit m v =
    -- Limit a vector to a given quantity
    if Vector2d.length v |> Quantity.lessThan m then
        v

    else
        case Vector2d.direction v of
            Just dir ->
                Vector2d.withLength m dir

            Nothing ->
                Vector2d.zero


findNewPosition : KinematicState -> KinematicState
findNewPosition kinematicState =
    if Quantity.equalWithin (Pixels.pixels 0) kinematicState.max_v (Pixels.pixels 0) then
        { kinematicState | velocity = Vector2d.zero }

    else
        { kinematicState
            | position = Point2d.translateBy (limit kinematicState.max_v kinematicState.velocity) kinematicState.position
            , velocity = limit kinematicState.max_v <| Vector2d.plus (Vector2d.scaleBy frictionRate kinematicState.velocity) (limit kinematicState.max_a kinematicState.acceleration)
            , acceleration = Vector2d.zero
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
                                        Vector2d.from avoidee.position avoiderKs.position

                                    distancevalue =
                                        Pixels.inPixels (Vector2d.length distance)

                                    scaled =
                                        if Quantity.greaterThan settings.avoid_radius (Vector2d.length distance) then
                                            Vector2d.zero

                                        else
                                            distance |> Vector2d.scaleBy 0.1 |> Vector2d.scaleBy settings.strength
                                in
                                scaled
                             -- |> Vector2d.direction
                             -- |> Maybe.map (Vector2d.withLength (Pixels.pixels scaled))
                             -- |> Maybe.withDefault (Vector2d.pixels 0 0)
                             -- |> Vector2d.reverse
                            )
                        |> Vector2d.sum

                -- steering =
                --     desired |> Vector2d.plus avoiderKs.velocity
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
                            { ks | acceleration = Vector2d.plus ks.acceleration force }
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


isACollider : Entity -> Bool
isACollider entity =
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
            { kstate | velocity = Vector2d.plus kstate.velocity <| Maybe.withDefault Vector2d.zero <| Maybe.map (Vector2d.withLength kstate.max_a) <| Vector2d.direction <| Vector2d.pixels 0 -1 }

        Down ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Maybe.withDefault Vector2d.zero <| Maybe.map (Vector2d.withLength kstate.max_a) <| Vector2d.direction <| Vector2d.pixels 0 1 }

        Left ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Maybe.withDefault Vector2d.zero <| Maybe.map (Vector2d.withLength kstate.max_a) <| Vector2d.direction <| Vector2d.pixels -1 0 }

        Right ->
            { kstate | velocity = Vector2d.plus kstate.velocity <| Maybe.withDefault Vector2d.zero <| Maybe.map (Vector2d.withLength kstate.max_a) <| Vector2d.direction <| Vector2d.pixels 1 0 }

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
        instructions =
            Svg.text_
                [ x <| "60"
                , y <| "35"
                , Svg.Attributes.fill "black"
                ]
                [ Svg.text <| "Use the arrow keys and lead all the goats to their pen" ]

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
                    List.map (\c -> render model.gameSettings c) <|
                        List.sortBy zOrder renderComponents
               )
            ++ [ instructions ]
        )


zOrder : Component -> Int
zOrder component =
    -- I guess we have to add all 'renderables' here
    case component of
        BodyComponent _ _ ->
            0

        AreaComponent _ _ ->
            -1

        ScoreComponent _ ->
            1

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


render : GameSettings -> Component -> Maybe (Svg Msg)
render gameSettings zeComponent =
    case zeComponent of
        BodyComponent location styling ->
            case styling.imagePath of
                Just path ->
                    Just <|
                        -- Svg.g []
                        --     [
                        -- SHOW THE THING
                        Svg.image
                            [ x <| String.fromFloat <| (Point2d.toPixels location.position).x - styling.radius
                            , y <| String.fromFloat <| (Point2d.toPixels location.position).y - styling.radius
                            , width <| String.fromFloat (styling.radius * 2)
                            , height <| String.fromFloat (styling.radius * 2)
                            , xlinkHref path
                            , transform <| createRotationString location styling
                            ]
                            []

                -- -- SHOW BOUNDING CIRCLES
                -- , Svg.circle
                --     [ cx <| String.fromFloat (Point2d.toPixels location.position).x
                --     , cy <| String.fromFloat (Point2d.toPixels location.position).y
                --     , r <| String.fromFloat location.circleRadius
                --     , Svg.Attributes.stroke styling.color
                --     , Svg.Attributes.strokeWidth "2"
                --     , Svg.Attributes.fill "none"
                --     ]
                --     []
                -- -- SHOW VELOCITIES
                -- , Svg.line
                --     [ Svg.Attributes.x1 <| String.fromFloat <| (Point2d.toPixels location.position).x
                --     , Svg.Attributes.y1 <| String.fromFloat <| (Point2d.toPixels location.position).y
                --     , Svg.Attributes.x2 <| String.fromFloat <| (Point2d.toPixels (Point2d.translateBy (Vector2d.scaleBy 10 location.velocity) location.position)).x
                --     , Svg.Attributes.y2 <| String.fromFloat <| (Point2d.toPixels (Point2d.translateBy (Vector2d.scaleBy 10 location.velocity) location.position)).y
                --     , Svg.Attributes.style "stroke:rgb(0,0,0);stroke-width:4"
                --     ]
                --     []
                -- ]
                _ ->
                    Just <|
                        Svg.circle
                            [ cx <| String.fromFloat (Point2d.toPixels location.position).x
                            , cy <| String.fromFloat (Point2d.toPixels location.position).y
                            , r <| String.fromFloat styling.radius
                            , Svg.Attributes.fill styling.color
                            ]
                            []

        AreaComponent boundingBox styling ->
            let
                extrema =
                    BoundingBox2d.extrema boundingBox

                ( width, height ) =
                    BoundingBox2d.dimensions boundingBox

                fillStyling =
                    case styling.patternName of
                        Just id ->
                            Svg.Attributes.fill <| "Url(#" ++ id ++ ")"

                        Nothing ->
                            Svg.Attributes.fill styling.color
            in
            Just <|
                Svg.rect
                    [ x <| String.fromFloat (Pixels.inPixels extrema.minX)
                    , y <| String.fromFloat (Pixels.inPixels extrema.minY)
                    , Svg.Attributes.width <| String.fromFloat (Pixels.inPixels width)
                    , Svg.Attributes.height <| String.fromFloat (Pixels.inPixels height)
                    , fillStyling
                    , rx "0"
                    , ry "0"
                    ]
                    []

        ScoreComponent score ->
            Just <|
                Svg.text_
                    [ x <| "20"
                    , y <| String.fromInt <| Tuple.first gameSettings.size - 25
                    , Svg.Attributes.fill "black"
                    ]
                    [ Svg.text <| "Score: " ++ String.fromInt score ]

        _ ->
            Nothing


isRenderable : Component -> Bool
isRenderable component =
    case component of
        BodyComponent _ _ ->
            True

        AreaComponent _ _ ->
            True

        ScoreComponent _ ->
            True

        _ ->
            False
