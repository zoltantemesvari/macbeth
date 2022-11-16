module Main exposing (..)

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
import List exposing (filter)
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


type alias Model =
    { filestate : FileState
    , light : Light
    , homepage : String
    , act1 : String
    , act2 : String
    , currentpage : String
    }


type Light
    = Day
    | Night


type FileState
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { filestate = Loading
      , currentpage = "/files/macbeth_act_one_raw.txt"
      , homepage = "/"
      , act1 = "/files/macbeth_act_one_raw.txt"
      , act2 = "/files/macbeth_act_two_raw.txt"
      , light = Day
      }
    , Http.get
        { url = "/files/macbeth_act_one_raw.txt"
        , expect = Http.expectString GotText
        }
    )


acts : Model -> String -> ( Model, Cmd Msg )
acts model page =
    ( { model
        | filestate = Loading
        , currentpage = page
      }
    , Http.get
        { url = page
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
            acts model file

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | filestate = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | filestate = Failure }, Cmd.none )



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
            [ onClick <| SwitchAct model.act1
            ]
            [ text "Első Felvonás" ]
        , button
            [ onClick <| SwitchAct model.act2
            ]
            [ text "Második Felvonás" ]
        ]


viewPlay : Model -> Html Msg
viewPlay model =
    case model.filestate of
        Failure ->
            text "Hiba! Nem tudjuk betölteni a színdarabot..elnézést."

        Loading ->
            text "Töltés..."

        Success fullText ->
        
            div
                [ class "play"
                ]
                [ Markdown.toHtml [] fullText
                ,   div
                [ class "footer"
                ]
                [ navigationView model
                ]
                ]
            
        
                


view : Model -> Html Msg
view model =
    
            div
                [ class "page"
                ]
                [ div
                    [ class "header"
                    ]
                    [ navigationView model
                    ]
                , viewPlay model
                ]
