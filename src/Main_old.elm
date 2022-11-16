module Main_old exposing (..)module Main exposing (..)

import Browser
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , fieldset
        , form
        , h2
        , h3
        , h4
        , img
        , input
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( alt
        , attribute
        , autofocus
        , checked
        , class
        , cols
        , disabled
        , href
        , id
        , name
        , placeholder
        , required
        , rows
        , selected
        , size
        , src
        , style
        , tabindex
        , target
        , title
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Markdown



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    acts "/files/macbeth_act_one_raw.txt"


acts : String -> ( Model, Cmd Msg )
acts file =
    ( Loading
    , Http.get
        { url = file
        , expect = Http.expectString GotText
        }
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | SwitchAct String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchAct file ->
            acts file

        GotText result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


navigationView : Model -> Html Msg
navigationView model =
    div
        [ class "navigation" ]
        [ button
            [ onClick <| SwitchAct "/files/macbeth_act_one_raw.txt"
            ]
            [ text "Első Felvonás" ]
        , button
            [ onClick <| SwitchAct "/files/macbeth_act_two_raw.txt"
            ]
            [ text "Második Felvonás" ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Hiba! Nem tudjuk betölteni a színdarabot..elnézést."

        Loading ->
            text "Töltés..."

        Success fullText ->
            div
                [ class "page"
                ]
                [ div
                    [ class "header"
                    ]
                    [ navigationView model
                    ]
                , div
                    [ class "play"
                    ]
                    [ Markdown.toHtml [] fullText
                    ]
                ]
