module Main exposing (..)

import Html exposing (Html, div, text, span, button, input, p, h1)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class, classList, style)
import RemoteData exposing (WebData)
import Json.Decode as Json exposing (dict, float, int)
import Json.Encode
import Http
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Task exposing (Task)
import Time


-- main : Program Never Model Msg
-------------------------------------------------------


fetchDecoder : Json.Decoder ServerResponse
fetchDecoder =
    decode ServerResponse
        |> required "allScores" (dict int)
        |> required "timeLeftMs" float


sendDecoder : Json.Decoder ()
sendDecoder =
    Json.succeed ()


fetch : Cmd Msg
fetch =
    Http.get "http://10.112.157.157:8080/api/state" fetchDecoder
        |> Http.send (RemoteData.fromResult >> FetchResponse)



-- temp : Cmd Msg
-- temp =
--     { allScores =
--         Dict.fromList
--             [ ( "team1", 5 )
--             , ( "team2", 6 )
--             , ( "team3", 9 )
--             ]
--     , timeLeftMs = 50000
--     }
--         |> RemoteData.Success
--         |> FetchResponse
--         |> toCmd


toCmd : Msg -> Cmd Msg
toCmd msg =
    Task.perform (\_ -> msg) (Task.succeed ())


sendVote : String -> Vote -> Cmd Msg
sendVote teamName vote =
    Http.post "http://10.112.157.157:8080/api/vote" (encodeBody teamName vote) sendDecoder
        |> Http.send (RemoteData.fromResult >> SetVoteResponse)


encodeBody : String -> Vote -> Http.Body
encodeBody teamName vote =
    Json.Encode.object
        [ ( "object", Json.Encode.string <| toString vote )
        , ( "userID", Json.Encode.string teamName )
        ]
        |> Http.jsonBody



-------------------------------------------------------


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always <| Time.every 500 (always Reload)
        }


type Vote
    = Rock
    | Paper
    | Scissors


type alias ServerResponse =
    { allScores : Dict String Int
    , timeLeftMs : Float
    }


type alias Model =
    { data : WebData ServerResponse
    , teamName : String
    , lastVote : Maybe Vote
    }


type Msg
    = SetVote Vote
    | FetchResponse (WebData ServerResponse)
    | SetVoteResponse (WebData ())
    | SetTeamName String
    | Reload


init : ( Model, Cmd Msg )
init =
    { data = RemoteData.Loading
    , teamName = "My Team"
    , lastVote = Nothing
    }
        ! [ fetch ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetVote vote ->
            { model
                | data = RemoteData.Loading
                , lastVote = Just vote
            }
                ! [ sendVote model.teamName vote ]

        FetchResponse status ->
            { model | data = status } ! []

        SetVoteResponse status ->
            case status of
                RemoteData.Success _ ->
                    model ! [ fetch ]

                otherwise ->
                    model ! []

        SetTeamName teamName ->
            { model | teamName = teamName } ! []

        Reload ->
            model ! [ fetch ]


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            [ style
                [ ( "color", "white" )
                , ( "text-align", "center" )
                ]
            ]
            [ text "10.112.157.157" ]
        , div
            [ style
                [ ( "width", "500px" )
                , ( "padding", "2em" )
                , ( "margin", "auto" )
                , ( "background-color", "white" )
                , ( "border-radius", "5px" )
                , ( "border", "3px solid black" )
                ]
            ]
            [ h1 [] [ text "Rock, Paper, Scissors" ]
            , input
                [ onInput SetTeamName
                , value model.teamName
                , class "form-control"
                ]
                []
            , div
                [ class "text-center"
                , style [ ( "margin", "15px" ) ]
                ]
                [ choices model.lastVote ]
            , div
                []
                [ callWhenLoaded showScores model.data ]
            ]
        ]


choices : Maybe Vote -> Html Msg
choices vote =
    div
        [ class "btn-group" ]
        [ choiceBtn vote Rock
        , choiceBtn vote Scissors
        , choiceBtn vote Paper
        ]


choiceBtn : Maybe Vote -> Vote -> Html Msg
choiceBtn lastVote vote =
    button
        [ onClick <| SetVote vote
        , classList
            [ ( "btn", True )
            , ( "btn-primary", Just vote == lastVote )
            , ( "btn-default", Just vote /= lastVote )
            ]
        ]
        [ text <| toString vote ]


showScores : ServerResponse -> Html Msg
showScores { allScores, timeLeftMs } =
    div
        []
        [ div
            [ style [ ( "font-weight", "bold" ) ]
            ]
            [ text "Time left: "
            , timeRemaining timeLeftMs
            , text "s"
            ]
        , allScores
            |> Dict.toList
            |> List.map
                (\( teamName, score ) ->
                    teamName
                        ++ ": "
                        ++ toString score
                        |> text
                        |> List.singleton
                        |> div []
                )
            |> div []
        ]


timeRemaining timeLeftMs =
    let
        time =
            timeLeftMs
                |> flip (/) 1000
                |> ceiling

        color =
            if (time < 3) then
                "red"
            else if (time < 6) then
                "orange"
            else
                "black"
    in
        span [ style [ ( "color", color ) ] ] [ text <| toString time ]


callWhenLoaded : (a -> Html msg) -> WebData a -> Html msg
callWhenLoaded f status =
    case status of
        RemoteData.Success data ->
            f data

        RemoteData.Loading ->
            text "Loading"

        RemoteData.NotAsked ->
            text "Something went wrong :("

        RemoteData.Failure err ->
            text <| errorMessage err


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl wrongUrl ->
            "Invalid url: " ++ wrongUrl

        Http.Timeout ->
            "The server didn't respond on time. Please try again"

        Http.NetworkError ->
            "Unable to connect to server"

        Http.BadPayload errMessage { status } ->
            "Unable to parse server response: " ++ errMessage

        Http.BadStatus { status, body } ->
            "Server returned " ++ (toString status.code) ++ ". " ++ status.message
