module Main exposing (..)

import Browser 
import Http exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main: Program () Model Msg
main = 
    Browser.element
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

type alias Model =
    { quote: String
    }

init : () -> (Model, Cmd Msg)
init _ = 
    (Model "", getQuote)

type Msg 
    = GetQuote
    | QuoteReceived (Result Http.Error String)

update: Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of 
        GetQuote ->
            (model, getQuote)
        QuoteReceived (Ok result) ->
            ( { model | quote = result }, Cmd.none)
        QuoteReceived (Err _) ->
            ( { model | quote = "There was an error" }, Cmd.none)

getQuote =
    Http.get
    { url = "http://localhost:3001/api/random-quote"
    , expect = Http.expectString QuoteReceived
    }
view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [ class "text-center"  ] 
            [ text "Chuck Norris!"]
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick GetQuote ] 
                [ text "Get Quote" ]
            ]
        , blockquote [] [ text model.quote ]
        ]
