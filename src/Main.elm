module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (Html, br, button, div, li, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as JD
import List.Extra exposing (getAt, removeAt)
import Random
import Task
import Time
import Url
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view >> toUnstyledDocument
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { questions : List Question
    , currentQuestionId : Int
    , seed : Int
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


type alias QuestionState =
    Maybe Bool


questionState : Question -> Variant -> QuestionState
questionState q v =
    case q.answer of
        Nothing ->
            Nothing

        Just answer ->
            Just (answer == v.answer)


nextQuestion : Model -> Maybe Question
nextQuestion model =
    let
        parts =
            List.partition (\q -> q.id <= model.currentQuestionId) model.questions

        questions =
            Tuple.second parts ++ Tuple.first parts
    in
    List.filter (\q -> q.answer == Nothing) questions |> List.head


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions = []
      , currentQuestionId = 1
      , seed = 0
      }
    , Cmd.batch
        [ Http.get
            { url = Url.Builder.absolute [ "data.json" ] []
            , expect = Http.expectJson GotData dataDecoder
            }
        , Task.perform SetSeed Time.now
        ]
    )



-- UPDATE


type Msg
    = ChoseQuestion Int
    | ChoseAnswer String
    | GotData (Result Http.Error (List Question))
    | SetSeed Time.Posix


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
                    ( { model
                        | questions = questions
                        , currentQuestionId = 1
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        SetSeed time ->
            ( { model | seed = Time.posixToMillis time }
            , Cmd.none
            )



-- API


dataDecoder : JD.Decoder (List Question)
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


toUnstyledDocument : Document msg -> Browser.Document msg
toUnstyledDocument doc =
    { title = doc.title
    , body = List.map toUnstyled doc.body
    }


view : Model -> Document Msg
view model =
    { title = "Frog"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.questions |> List.filter (\q -> q.id == model.currentQuestionId) |> List.head of
        Just q ->
            let
                variant =
                    case shuffleList (Random.initialSeed <| model.seed + q.id) q.variants |> List.head of
                        Just v ->
                            v

                        Nothing ->
                            { sentence = ""
                            , options = []
                            , answer = "ok"
                            }

                options =
                    shuffleList (Random.initialSeed <| model.seed + q.id) (variant.answer :: variant.options)

                answerText =
                    case q.answer of
                        Just answer ->
                            answer

                        Nothing ->
                            "…"

                nq =
                    nextQuestion model
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
                [ String.split "{{answer}}" variant.sentence |> String.join answerText |> text
                , ul [] <| List.map (viewOption q.answer) options
                , viewActionButton nq
                , viewQuestions model
                , div [] [ text (String.fromInt model.seed) ]
                ]

        Nothing ->
            div [] [ text "Question not found" ]


viewActionButton : Maybe Question -> Html Msg
viewActionButton nq =
    case nq of
        Nothing ->
            button [ css [ fontSize (px 25) ] ] [ text "See result" ]

        Just q ->
            button
                [ onClick (ChoseQuestion q.id)
                , css [ fontSize (px 25) ]
                ]
                [ text <| "Go to " ++ String.fromInt q.id ]


viewQuestions : Model -> Html Msg
viewQuestions model =
    let
        qButton q =
            span
                [ onClick (ChoseQuestion q.id)
                , css
                    [ padding (px 4)
                    , margin (px 5)
                    , borderRadius (px 18)
                    , display inlineBlock
                    , width (px 27)
                    , height (px 18)
                    , textAlign center
                    , cursor pointer
                    , backgroundColor
                        (hex <|
                            if q.id == model.currentQuestionId then
                                "77cc77"

                            else
                                case q.answer of
                                    Just _ ->
                                        "aaaaff"

                                    Nothing ->
                                        "7777ff"
                        )
                    , fontSize (px 15)
                    , color (hex "fff")
                    ]
                ]
                [ text <| String.fromInt q.id ]
    in
    div [] <| List.map qButton model.questions


viewOption : Maybe String -> String -> Html Msg
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


shuffleList : Random.Seed -> List a -> List a
shuffleList seed list =
    shuffleListHelper seed list []


shuffleListHelper : Random.Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                Random.int 0 (List.length source - 1)

            ( index, nextSeed ) =
                Random.step indexGenerator seed

            valAtIndex =
                getAt index source

            sourceWithoutIndex =
                removeAt index source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                result
