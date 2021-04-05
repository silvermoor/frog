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
    , finished : Bool
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


nextQuestion : Model -> Maybe Question
nextQuestion model =
    let
        ( past, future ) =
            List.partition (\q -> q.id <= model.currentQuestionId) model.questions

        questions =
            future ++ past
    in
    List.filter (\q -> q.answer == Nothing) questions |> List.head

nextMistake : Model -> Maybe Question
nextMistake model =
    List.filter (checkQuestion model) model.questions |> List.head

numberOfPoints : Model -> Int
numberOfPoints model =
    let
        countQuestion q acc =
            1
    in
    List.foldl countQuestion 0 model.questions


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions = []
      , currentQuestionId = 1
      , seed = 0
      , finished = False
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
    | Finish
    | SetRightAnswers



-- for debug purposes


setRightAnswers : Model -> Model
setRightAnswers model =
    let
        setAnswer q =
            let
                variant =
                    getVariant model q
            in
            { q | answer = Just variant.answer }

        newQuestions =
            List.map setAnswer model.questions
    in
    { model | questions = newQuestions }


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

        SetRightAnswers ->
            ( setRightAnswers model, Cmd.none )

        Finish ->
            ( { model | finished = True }, Cmd.none )



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


getVariant : Model -> Question -> Variant
getVariant model q =
    case shuffleList (Random.initialSeed <| model.seed + q.id) q.variants |> List.head of
        Just v ->
            v

        Nothing ->
            { sentence = ""
            , options = []
            , answer = "ok"
            }


getQuestion : Model -> Int -> Maybe Question
getQuestion model id =
    model.questions |> List.filter (\q -> q.id == id) |> List.head


checkQuestion : Model -> Question -> Bool
checkQuestion model q =
    let
        variant =
            getVariant model q
    in
    case q.answer of
        Nothing ->
            False

        Just answer ->
            answer == variant.answer


pointsCount : Model -> Int
pointsCount model =
    let
        results =
            List.map (checkQuestion model) model.questions

        count result i =
            case result of
                True ->
                    i + 1

                False ->
                    i
    in
    List.foldl count 0 results


viewBody : Model -> Html Msg
viewBody model =
    case getQuestion model model.currentQuestionId of
        Just q ->
            let
                variant =
                    getVariant model q

                options =
                    shuffleList (Random.initialSeed <| model.seed + q.id) (variant.answer :: variant.options)

                answerText =
                    case q.answer of
                        Just answer ->
                            answer

                        Nothing ->
                            "…"
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
                , viewActionButton model
                , viewQuestions model
                , div [] [ text (String.fromInt model.seed) ]
                , div [] [ text (String.fromInt <| pointsCount model) ]
                , button [ onClick SetRightAnswers, css [ fontSize (px 18) ] ] [ text "Answer Random" ]
                ]

        Nothing ->
            div [] [ text "Question not found" ]


viewActionButton : Model -> Html Msg
viewActionButton model =
    if model.finished then
        case nextMistake model of
            Nothing ->
                text "YOU ARE AWESOME!"

            Just q ->
                button
                    [ onClick (ChoseQuestion q.id)
                    , css [ fontSize (px 25) ]
                    ]
                    [ text <| "Next rule" ]

    else
        case nextQuestion model of
            Nothing ->
                button
                    [ onClick Finish
                    , css [ fontSize (px 25) ]
                    ]
                    [ text "Finish" ]

            Just q ->
                button
                    [ onClick (ChoseQuestion q.id)
                    , css [ fontSize (px 25) ]
                    ]
                    [ text <| "Next question" ]


viewQuestionColor : Model -> Question -> String
viewQuestionColor model q =
    let
        result =
            checkQuestion model q
    in
    if model.finished then
        if result then
            "44AA44"

        else
            "AA4444"

    else
        "444444"


viewQuestions : Model -> Html Msg
viewQuestions model =
    let
        qCursor q =
            if model.currentQuestionId == q.id then
                borderBottom3 (px 2) solid (hex "fff")
            else
                borderBottom3 (px 2) solid transparent

        qButton q =
            span
                [ onClick (ChoseQuestion q.id)
                , css
                    [ padding (px 4)
                    , margin (px 5)
                    , display inlineBlock
                    , width (px 27)
                    , height (px 18)
                    , textAlign center
                    , cursor pointer
                    , color (hex <| viewQuestionColor model q)
                    , fontSize (px 15)
                    , qCursor q
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
