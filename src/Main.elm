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
        , h1
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
    , mobilemenu : MobileMenu
    , mobilenav : String
    }


type Light
    = Day
    | Night

type MobileMenu
    = Open
    | Closed


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
      , mobilemenu = Closed
      , mobilenav = "closed"
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
        , mobilenav = "closed"
      }
    , Http.get
        { url = page
        , expect = Http.expectString GotText
        }
    )


dayornight : Light -> String
dayornight light =
    case light of
        Day ->
            "day"

        Night ->
            "night"


dayornightinverse : Light -> String
dayornightinverse light =
    case light of
        Day ->
            "Éjjel"

        Night ->
            "Nappal"



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | SwitchAct String
    | DayOrNight
    | MobileNav


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

        DayOrNight ->
            case model.light of
                Day ->
                    ( { model | light = Night , mobilenav = "closed" }, Cmd.none )

                Night ->
                    ( { model | light = Day , mobilenav = "closed" }, Cmd.none )

        MobileNav ->
            case model.mobilemenu of
                Closed ->
                    ( { model | mobilenav = "open", mobilemenu = Open }, Cmd.none )
                
                Open ->
                    ( { model | mobilenav = "closed", mobilemenu = Closed  }, Cmd.none )
                


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


navigationView : Model -> Html Msg
navigationView model =
    div
        [ class ("navigation " ++ model.mobilenav) ]
        [ button
            [ class "button"
            , onClick <| SwitchAct model.act1
            ]
            [ text "Első Felvonás" ]
        , button
            [ class "button"
            , onClick <| SwitchAct model.act2
            ]
            [ text "Második Felvonás" ]
        , button
            [ class "button"
            , onClick <| DayOrNight
            ]
            [ text (dayornightinverse model.light) ]
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
                [ class "play "
                ]
                [ Markdown.toHtml [] fullText
                , div
                    [ class "footer"
                    ]
                    [ navigationView model
                    ]
                ]


view : Model -> Html Msg
view model =
    div
        [ class ("page " ++ dayornight model.light)
        ]
        [ div
            [ class "header"
            ]
            [ navigationView model
            , div
                [ class "title"
                ]
                [ h1
                    []
                    [ text "William Shakespeare" ]
                , h1
                    []
                    [ text "MACBETH" ]
                , h4
                    []
                    [ text "Fordította: Temesvári Zoltán" ]
                , p
                    [ class "printmessage" ]
                    [ text "E-könyv igényelhető a hello@macbeth.hu emailcímen" ]
                ]
            , div [ class "mobilemenu" ]
                [ button
                    [ class "button"
                    , onClick <| MobileNav
                    ]
                    [ text "Menü" ]
                ]
            ]
        , viewPlay model
        ]
