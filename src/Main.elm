module Main exposing (..)

import Browser 
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
    (Model "", Cmd.none)

type Msg = 
    GetQuote

update: Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of 
        GetQuote ->
            ( { model | quote = model.quote ++ "A quote! " }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [ class "text-center"  ] 
            [ text "Chuck Norris!"]
        , p [ class "text-center" ]
            [ button [ class "btn btn-success", onClick GetQuote ] 
                [ text "Get Quote" ]
            ]
        , text model.quote
        ]
