module Questions exposing
    ( Question, Msg(..), Mark(..), UserState(..), MsgTaggers
    , createStateful, createStateless
    , mapView
    , getQuestionView, getResponseFieldView
    )

{-|


# Types

@docs Question, Msg, Mark, UserState, MsgTaggers


# Creation

@docs createStateful, createStateless


# View

@docs mapView


# Utilities

@docs getQuestionView, getResponseFieldView

-}

import Html as H exposing (Html)
import Html.Attributes as HA


{-| An opaque type that represent quiz question component. For now, the inner representation is:

    type Question
        = Question
            { viewQuestion : Html Never
            , viewResponseField : Html Msg
            }

where `Msg` is defined below.

-}
type Question
    = Question
        { viewQuestion : Html Never
        , viewResponseField : Html Msg
        }


{-| `Msg`s sent from the quiz component to the parent application. You need to convert them into your application's `Msg` type using the first argument of the `mapView` function.
-}
type Msg
    = Updated Question
    | Marked Mark


defaultMsg : Msg
defaultMsg =
    Updated
        (Question
            { viewQuestion = H.text ""
            , viewResponseField = H.text ""
            }
        )


{-| A custom type for quiz results. You can attach detailed explanation in `Html Never` for educational purpose.
-}
type Mark
    = Correct (Html Never)
    | Wrong (Html Never)


{-| A type alias of record which is used in `createStateful` function.
-}
type alias MsgTaggers model ans =
    { modelUpdated : model -> Msg
    , modelUpdatedWith : (model -> model) -> Msg
    , userHasAnswered : ans -> Msg
    }


{-| Creates a quiz component with its own `Model`, separate from your main application's model.
Think of the argument as a simplified version of `Browser.sandbox`, as it lacks an `update` function and has a limited `Msg`.

You can send a `Msg` using the internally provided `modelUpdate`, `modelUpdateWith` and `userHasAnswered` functions.
The `modelUpdated newModel` and `modelUpdatedWith f` functions emit `Updated <Question with the model updated>`, and the `userHasAnswered ans` function emits `Marked ((Correct|Wrong) (Html Never))` based on whether `isCorrect ans` returns `True` or `False`.

    import Html as H exposing (Html)
    import Html.Attributes as HA
    import Html.Events as HE

    type alias QuizModel =
        { userInput : String
        , isShowingHint : Bool
        }

    type alias Answer =
        String

    quiz : Question
    quiz =
        let
            qInit : QuizModel
            qInit = ""

            viewQuestion : Html Never
            viewQuestion =
                span []
                    [ H.text <| "Write the name of the atom that is represented in atomic symbol "
                    , H.span [HA.style "font-weight" "bold"]
                        [H.text "P"]
                    , H.text "."
                    ]

            viewResponseField :
                MsgTagger QuizModel Answer
                    -> QuizModel
                    -> Html Msg
            viewResponseField {modelUpdated, userHasAnswered} model =
                \model ->
                    H.div []
                        [ H.input
                            [ HA.value model
                            , HA.placeholder "input your answer."
                            , HE.onInput
                                (\input ->
                                    modelUpdated
                                        { model | userInput = ans }
                                )
                            ]
                            []
                        , H.button
                            [ HE.onClick
                                (userHasAnswered model.userInput)
                            ]
                            [ H.text "submit"]
                        , H.br [] []
                        , H.button
                            [ HE.onClick <|
                                modelUpdated
                                    { model
                                        | isShowingHint =
                                            not model.isShowingHint
                                    }
                            ]
                            [ if model.isShowingHint then
                                H.text "show hint"
                            , else
                                H.text "hide hint"
                            ]
                        , if model.isShowingHint then
                            H.div [ HA.style "color" "pink" ]
                                [ H.text "Phosphoric acid contains this element as its center atom."
                                ]
                          else
                              H.text ""
                        ]

            isCorrect : Answer -> Bool
            isCorrect userAnswer =
                List.member userAnswer
                    [ "Phosphorus"
                    , "phosphorus"
                    , "PHOSPHORUS"
                    ]

            viewExplanation : Answer -> Html Never
            viewExplanation userAnswer =
                if isCorrect userAnswer then
                    H.text "Correct!"

                else
                    H.span
                        []
                        [ H.span [ HA.style "font-weight" "bold" ]
                            [H.text "P"]
                        , H.text " stands for "
                        , H.strong []
                            [ H.text "phosphorus"
                            ]
                        , H.text
                            <| ", while you have input "
                            ++ userAnswer
                            ++ "."
                        ]
        in
        createStateful
            { init = qInit
            , viewQuestion = viewQuestion
            , viewRespanseField = viewResponseField
            , isCorrect = isCorrect
            , viewExplanation = viewExplanation
            }

-}
createStateful :
    { init : model
    , viewQuestion : Html Never
    , viewResponseField :
        { modelUpdated : model -> Msg
        , modelUpdatedWith : (model -> model) -> Msg
        , userHasAnswered : answer -> Msg
        }
        -> (model -> Html Msg)
    , isCorrect : answer -> Bool
    , viewExplanation : answer -> Html Never
    }
    -> Question
createStateful ({ init, viewQuestion, viewResponseField, isCorrect, viewExplanation } as record) =
    { viewQuestion = viewQuestion
    , viewResponseField =
        viewResponseField
            { modelUpdated =
                \newModel ->
                    Updated (createStateful { record | init = newModel })
            , modelUpdatedWith =
                \f ->
                    Updated (createStateful { record | init = f init })
            , userHasAnswered =
                \ans ->
                    if isCorrect ans then
                        Marked (Correct (viewExplanation ans))

                    else
                        Marked (Wrong (viewExplanation ans))
            }
            init
    }
        |> Question


{-| Creates a stateless quiz component.

    import Html as H exposing (Html)
    import Html.Attributes as HA
    import Html.Events as HE

    type alias Answer =
        String

    quiz : Question
    quiz =
        let
            viewQuestion : Html Never
            viewQuestion =
                span []
                    [ H.text <| "Select the name of the atom that is represented in atomic symbol "
                    , H.span [ HA.style "font-weight" "bold" ]
                        [ H.text "P" ]
                    , H.text "."
                    ]

            viewResponseField : Html Answer
            viewResponseField =
                [ "Pluto"
                , "Plutinum"
                , "Phosphorus"
                , "Protoactinium"
                ]
                    |> List.map
                        (\name ->
                            H.button
                                [ HE.onClick name ]
                                [ H.text name ]
                        )
                    |> H.div []

            isCorrect : Answer -> Bool
            isCorrect =
                (==) "Phosphorus"

            viewExplanation : Answer -> Html Never
            viewExplanation userAnswer =
                if isCorrect userAnswer then
                    H.text "Correct!"

                else
                    H.span
                        []
                        [ H.span [ HA.style "font-weight" "bold" ]
                            [ H.text "P" ]
                        , H.text " stands for "
                        , H.strong []
                            [ H.text "phosphorus"
                            ]
                        , H.text <|
                            ", while you have selected "
                                ++ userAnswer
                                ++ "."
                        ]
        in
        createStateless
            { viewQuestion = viewQuestion
            , viewRespanseField = viewResponseField
            , isCorrect = isCorrect
            , viewExplanation = viewExplanation
            }

-}
createStateless :
    { viewQuestion : Html Never
    , viewResponseField : Html answer
    , isCorrect : answer -> Bool
    , viewExplanation : answer -> Html Never
    }
    -> Question
createStateless { viewQuestion, viewResponseField, isCorrect, viewExplanation } =
    createStateful
        { init = ()
        , viewQuestion = viewQuestion
        , viewResponseField =
            \{ userHasAnswered } () ->
                viewResponseField
                    |> H.map
                        (\ans -> userHasAnswered ans)
        , isCorrect = isCorrect
        , viewExplanation = viewExplanation
        }


{-| A custom type that represents the user's state regarding a question. You need to maintain this value in your application's model and pass it to the `mapView` function.
-}
type UserState
    = Answering
    | HasAnswered Mark


{-| Renders the quiz question. The `Msg`s from the quiz are mapped to your application's `Msg` using the first argument.
The functions `viewQuestion` and `viewResponseField` are provided by the `Question` value you passed as the last argument.
So if you have lots of `Question`s and want to show them at once, following code works.

    import Array exposing (Array)
    import Browser
    import Html as H exposing (Html)
    import Html.Attributes as HA
    import Html.Events as HE
    import Questions exposing (..)

    type alias AppModel =
        { questions : Array Question
        , userStates : Array UserState
        }

    initialQuestions : Array Question
    initialQuestions =
        [ createStateless
            { viewQuestion =
                H.text "1+1 =?"
            , viewResponseField =
                List.range 1 9
                    |> List.map
                        (\n -> H.button [ HE.onClick n ] [ H.text <| String.fromInt n ])
                    |> H.div []
            , isCorrect = (==) 2
            , viewExplanation =
                \n ->
                    if n == 2 then
                        H.text "Correct!"

                    else
                        H.text "Incorrect..."
            }
        , createStateless
            { viewQuestion =
                H.text "2+1 =?"
            , viewResponseField =
                List.range 1 9
                    |> List.map
                        (\n -> H.button [ HE.onClick n ] [ H.text <| String.fromInt n ])
                    |> H.div []
            , isCorrect = (==) 3
            , viewExplanation =
                \n ->
                    if n == 3 then
                        H.text "Correct!"

                    else
                        H.text "Incorrect..."
            }
        ]
            |> Array.fromList

    type AppMsg
        = NoOp
        | UserHasAnswered Int UserState
        | ModelUpdated Int Question

    update : AppMsg -> AppModel -> AppModel
    update msg model =
        case msg of
            NoOp ->
                model

            UserHasAnswered index userState ->
                { model
                    | userStates =
                        Array.set index userState model.userStates
                }

            ModelUpdated index question ->
                { model
                    | questions =
                        Array.set index question model.questions
                }

    view : AppModel -> Html AppMsg
    view model =
        List.map2
            (\a b -> ( a, b ))
            (Array.toList model.userStates)
            (Array.toList model.questions)
            |> List.indexedMap
                (\n ( userState, question ) ->
                    mapView
                        (\msg ->
                            case msg of
                                Updated q ->
                                    ModelUpdated n q

                                Marked m ->
                                    UserHasAnswered n (HasAnswered m)
                        )
                        (\us { viewQuestion, viewResponseField } ->
                            H.div
                                []
                                [ H.text "Q: "
                                , viewQuestion
                                , H.br [] []
                                , case us of
                                    Answering ->
                                        viewResponseField

                                    HasAnswered m ->
                                        let
                                            ( correct, explanation ) =
                                                case m of
                                                    Correct e ->
                                                        ( True, e )

                                                    Wrong e ->
                                                        ( False, e )
                                        in
                                        H.map (always NoOp)
                                            (if correct then
                                                explanation

                                             else
                                                H.span [ HA.style "color" "red" ]
                                                    [ explanation ]
                                            )
                                ]
                        )
                        userState
                        question
                )
            |> H.div []

    main : Program () AppModel AppMsg
    main =
        Browser.sandbox
            { init =
                { questions = initialQuestions
                , userStates = Array.repeat (Array.length initialQuestions) Answering
                }
            , update = update
            , view = view
            }

-}
mapView :
    (Msg -> msg)
    ->
        (UserState
         ->
            { viewQuestion : Html msg
            , viewResponseField : Html msg
            }
         -> Html msg
        )
    -> UserState
    -> Question
    -> Html msg
mapView mapper frame userState (Question q) =
    let
        mapped =
            { viewQuestion =
                q.viewQuestion
                    |> H.map (\_ -> defaultMsg)
                    |> H.map mapper
            , viewResponseField =
                H.map mapper q.viewResponseField
            }
    in
    frame userState mapped


{-| Returns view of question sentence. Use `mapView` for rendering purpose.
-}
getQuestionView : Question -> Html Never
getQuestionView (Question q) =
    q.viewQuestion


{-| Returns view of response field where users make their answers. Use `mapView` for rendering purpose.
-}
getResponseFieldView : Question -> Html Msg
getResponseFieldView (Question q) =
    q.viewResponseField
