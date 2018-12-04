module Main exposing (Model, Msg(..), init, main, update, view)

import Animation exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg, svgBox)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (Html)


type alias Model =
    { angleAnimation : Animation
    , clock : Clock
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model (static 0) 0, Cmd.none )


type Msg
    = Tick Float
    | Click
    | NoOp


starAnimation : Float -> Animation
starAnimation start =
    animation start
        |> from 0
        |> to (2 * pi)
        |> duration 3000
        |> delay 200


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | clock = model.clock + dt }, Cmd.none )

        Click ->
            ( { model | angleAnimation = starAnimation model.clock }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


tree : Collage msg
tree =
    let
        branch w =
            triangle (toFloat w)
                |> filled (uniform green)
                |> scaleY 0.5

        base =
            400

        anchor i =
            ( 0.0, toFloat i * base / 4 )
    in
    List.foldl
        (\i t ->
            at (\_ -> anchor i) (branch (base - 30 * i)) t
        )
        empty
        [ 0, 1, 2, 3, 4 ]


star : Model -> Collage Msg
star { angleAnimation, clock } =
    let
        n =
            5

        outerRadius =
            1.0

        innerRadius =
            0.38

        theta =
            animate clock angleAnimation

        point i =
            let
                angle =
                    turns (toFloat i / (2 * n))

                r =
                    if modBy 2 i == 0 then
                        outerRadius

                    else
                        innerRadius
            in
            ( r * sin angle, r * cos angle )

        points =
            List.map point <| List.range 0 (2 * n - 1)
    in
    (polygon points
        |> filled (uniform yellow)
        |> scale 60
        |> rotate (radians theta)
    )
        |> onClick Click


drawing : Model -> Html Msg
drawing model =
    let
        background =
            rectangle 500 800 |> outlined (solid thin (uniform red))
    in
    at top (star model) (tree |> center)
        |> (\x -> impose x background)
        |> svg


view : Model -> Browser.Document Msg
view model =
    { title = "Christmas Tree"
    , body = [ drawing model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Tick


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
