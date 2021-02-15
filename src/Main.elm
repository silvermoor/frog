module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (Html, button, div, li, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as JD
import Url
import Url.Builder



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { questions : List Question
    , currentQuestionId : Int
    }


type alias Variant =
    { sentence : String
    , options : List String
    , answer : String
    }


type alias Question =
    { id : Int
    , variants : List Variant
    , answer : Maybe String
    , recommendation : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions = []
      , currentQuestionId = 1
      }
    , Http.get
        { url = Url.Builder.absolute [ "data.json" ] []
        , expect = Http.expectJson GotData dataDecoder
        }
    )



-- UPDATE


type Msg
    = ChoseQuestion Int
    | ChoseAnswer String
    | GotData (Result Http.Error (List Question))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChoseAnswer answer ->
            let
                updateQuestion q =
                    if q.id == model.currentQuestionId then
                        { q | answer = Just answer }

                    else
                        q
            in
            ( { model | questions = List.map updateQuestion model.questions }, Cmd.none )

        ChoseQuestion id ->
            ( { model | currentQuestionId = id }, Cmd.none )

        GotData result ->
            case result of
                Ok questions ->
                    ( { questions = questions
                      , currentQuestionId = 1
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- API


dataDecoder =
    JD.field "questions"
        (JD.list
            (JD.map4 Question
                (JD.field "id" JD.int)
                (JD.field "variants"
                    (JD.list
                        (JD.map3 Variant
                            (JD.field "sentence" JD.string)
                            (JD.field "options" (JD.list JD.string))
                            (JD.field "answer" JD.string)
                        )
                    )
                )
                (JD.succeed Nothing)
                (JD.field "recommendation" JD.string)
            )
        )



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view model =
    { title = "Frog"
    , body = [ (viewBody >> toUnstyled) model ]
    }


viewBody model =
    case model.questions |> List.filter (\q -> q.id == model.currentQuestionId) |> List.head of
        Just q ->
            let
                variant =
                    case q.variants |> List.head of
                        Just v ->
                            v

                        Nothing ->
                            { sentence = ""
                            , options = []
                            , answer = "ok"
                            }
            in
            div
                [ css
                    [ width (px 460)
                    , margin2 (px 20) auto
                    , fontFamily sansSerif
                    , fontSize (pt 18)
                    , color (rgb 80 80 80)
                    , padding (px 12)
                    , backgroundImage <| linearGradient (stop <| hex "AAF") (stop <| hex "AFA") []
                    ]
                ]
                [ text variant.sentence
                , ul [] <| List.map (viewOption q.answer) (variant.answer :: variant.options)
                , viewQuestions model.questions
                ]

        Nothing ->
            div [] [ text "Question not found" ]


viewQuestions questions =
    let
        qButton q =
            span
                [ onClick (ChoseQuestion q.id)
                , css
                    [ padding (px 12)
                    , margin (px 12)
                    , borderRadius (px 32)
                    , display inlineBlock
                    , width (px 32)
                    , height (px 32)
                    , textAlign center
                    , cursor pointer
                    , backgroundColor (hex "aaaaff")
                    , color (hex "fff")
                    ]
                ]
                [ text "?" ]
    in
    div [] <| List.map qButton questions


viewOption answer option =
    li
        [ onClick (ChoseAnswer option)
        , css
            [ listStyle none
            , lineHeight (px 32)
            , padding (px 8)
            , borderRadius (px 8)
            , if answer == Just option then
                backgroundColor (hex "9f9")

              else
                backgroundColor (hex "fff")
            ]
        ]
        [ text " "
        , text option
        ]
