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


type alias Question =
    { id : Int
    , sentence : String
    , answerOptions : List String
    , rightAnswer : String
    , answer : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions =
            [ { id = 1
              , sentence = "I ... not think about it."
              , answerOptions = [ "can", "have to", "will", "expect" ]
              , rightAnswer = "can"
              , answer = Nothing
              }
            , { id = 2
              , sentence = "You ... imagine what I have whent through."
              , answerOptions = [ "may be", "can easily", "can hardly", "should try" ]
              , rightAnswer = "can hardly"
              , answer = Nothing
              }
            ]
      , currentQuestionId = 1
      }
    , Http.get
        { url = Url.Builder.absolute [ "data.json" ] []
        , expect = Http.expectJson GotData (JD.field "questions" JD.string)
        }
    )



-- UPDATE


type Msg
    = ChoseQuestion Int
    | ChoseAnswer String
    | GotData (Result Http.Error String)


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

        GotData _ ->
            ( model, Cmd.none )



-- API
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
    let
        question =
            case model.questions |> List.filter (\q -> q.id == model.currentQuestionId) |> List.head of
                Just q ->
                    q

                Nothing ->
                    Question 0 "oops..." [ "ok" ] "" Nothing
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
        [ text question.sentence
        , ul [] <| List.map (viewOption question.answer) question.answerOptions
        , viewQuestions model.questions
        ]


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
