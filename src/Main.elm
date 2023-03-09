module Main exposing (..)

import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , text
        )
import Html.Attributes
    exposing
        ( checked
        , class
        , type_
        )
import Html.Events exposing (onCheck, onClick)
import Http
import List exposing (filter)
import Markdown
import Array



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
    , pages: List(Acts)
    , currentpage : Acts
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

type Acts
    = Act1 
    | Act2
    | Act3
    | Act4
    | Act5
    | Impressum
    | Characters

to_load : Acts -> String
to_load act =
    case act of
        Act1 ->
            "/files/macbeth_act_one_raw.txt"

        Act2 ->
            "/files/macbeth_act_two_raw.txt"

        Act3 ->
            "/files/macbeth_act_three_raw.txt"

        Act4 ->
            "/files/macbeth_act_four_raw.txt"

        Act5 ->
            "/files/macbeth_act_five_raw.txt"

        Impressum ->
            "/files/impressum.txt"

        Characters ->
            "/files/characters.txt"

navtext : Acts -> String
navtext act =
    case act of
        Act1 ->
            "Első Felvonás"

        Act2 ->
            "Második Felvonás"

        Act3 ->
            "Harmadik Felvonás"

        Act4 ->
            "Negyedik Felvonás"

        Act5 ->
            "Ötödik Felvonás"

        Impressum ->
            "Impresszum"

        Characters ->
            "Szereplők"

init : () -> ( Model, Cmd Msg )
init _ =
    ( { filestate = Loading
      , currentpage = Act1
      , pages = [ Act1, Act2, Act3 ]
      , light = Day
      , mobilemenu = Closed
      , mobilenav = "closed"
      }
    , Http.get
        { url = to_load Act1
        , expect = Http.expectString GotText
        }
    )


acts : Model -> Acts -> ( Model, Cmd Msg )
acts model page =
    ( { model
        | filestate = Loading
        , currentpage = page
        , mobilenav = "closed"
      }
    , Http.get
        { url = to_load page
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


daynightswitch : Bool -> Msg
daynightswitch check =
    if check then
        LetNight

    else
        LetDay

next_act : Model -> Acts
next_act model =
    let pages_array = Array.fromList model.pages
        indexed_pages = Array.toIndexedList pages_array
        current_index = List.filter (\( index, page ) -> page == model.currentpage) indexed_pages |> List.head |> Maybe.withDefault ( 0, Act1 ) |> Tuple.first
        next_index = current_index + 1

    in
        
    Array.get next_index pages_array |> Maybe.withDefault Act1
    
    

-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | SwitchAct Acts
    | LetDay
    | LetNight
    | MobileNav


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchAct page ->
            acts model page

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | filestate = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | filestate = Failure }, Cmd.none )

        MobileNav ->
            case model.mobilemenu of
                Closed ->
                    ( { model | mobilenav = "open", mobilemenu = Open }, Cmd.none )

                Open ->
                    ( { model | mobilenav = "closed", mobilemenu = Closed }, Cmd.none )

        LetDay ->
            ( { model | light = Day, mobilenav = "closed" }, Cmd.none )

        LetNight ->
            ( { model | light = Night, mobilenav = "closed" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


navigationView : Model -> Html Msg
navigationView model =
    div
        [ class ("navigation " ++ model.mobilenav) ]
        
        ( navButtonView model )
        

navButtonView : Model -> List(Html Msg)
navButtonView model =
    List.map
        (\page ->
            button
                [ class "button"
                , onClick <| SwitchAct page
                ]
                [ text <| navtext page ]
        )
        model.pages

navigationNextView : Model -> Html Msg
navigationNextView model =
    div
        [ class ("navigation " ++ model.mobilenav) ]
        [ button
            [ class "button"
            , onClick
                ( model
                    |> next_act
                    |> SwitchAct
                )
            ]
            [ text "Következő Felvonás" ]
        ]

viewPlay : Model -> Html Msg
viewPlay model =
    case model.filestate of
        Failure ->
            div
                [ class "play"
                ]
                [ h1
                    []
                    [ text "Hiba! Nem tudjuk betölteni a színdarabot..valószínűleg hálózati probléma. Kérem, ellenőrizze az internetkapcsolatot."
                    ]
                ]

        Loading ->
            text "Töltés..."

        Success fullText ->
            div []
                [ div
                    [ class "play"
                    ]
                    [ Markdown.toHtml [] fullText
                    ]
                , div
                    [ class "footer"
                    ]
                    [ navigationNextView model
                    ]
                ]


view : Model -> Html Msg
view model =
    div
        [ class "page"
        ]
        [ div
            [ class ("overlay " ++ dayornight model.light)
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
                        [ text "E-könyv igényelhető a hello@macbeth.hu emailcímen!" ]
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
            , label
                [ class "dayNight"
                ]
                [ input
                    [ type_ "checkbox"
                    , onCheck <| daynightswitch
                    ]
                    []
                , div []
                    []
                ]
            ]
        ]
