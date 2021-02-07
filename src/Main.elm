module Main exposing (..)

import Browser
import Html exposing (Html, button, div, li, span, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



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
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChoseQuestion Int
    | ChoseAnswer String


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



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Frog"
    , body = [ viewBody model ]
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
    div [ style "margin" "20px" ]
        [ text question.sentence
        , ul [] <| List.map (viewOption question.answer) question.answerOptions
        , viewQuestions model.questions
        ]


viewQuestions questions =
    let
        qButton q =
            span
                [ onClick (ChoseQuestion q.id)
                , style "padding" "6px"
                ]
                [ text "q" ]
    in
    div [] <| List.map qButton questions


viewOption answer option =
    li [ onClick (ChoseAnswer option) ]
        [ text " "
        , if answer == Just option then
            text "—"

          else
            text "-"
        , text option
        ]
