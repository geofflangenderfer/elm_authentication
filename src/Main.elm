module Main exposing (..)

import Browser 
import Http exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

type alias Model =
    { quote : String
    , errorMsg : String
    }

type Msg 
    = GetQuote
    | QuoteReceived (Result Http.Error String)
--    | SetUsername String
--    | SetPassword String
--    | ClickLogin 
--    | ClickRegister 
--    | GetToken
--    | TokenReceived (Result Http.Error String)
--    | LogOut 

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of 
        --
        GetQuote ->
            (model, getQuote)
        --
        QuoteReceived (Ok result) ->
            ( { model | quote = result }, Cmd.none)
        --
        QuoteReceived (Err _) ->
            ( { model | errorMsg = "There was an error" }, Cmd.none)
--        SetUsername u ->
--            ( { model | username = u }, Cmd.none)
--        SetPassword pw ->
--            ( { model | password = pw }, Cmd.none)
--        --
--        ClickLogin ->
--            ( model, authUser)
--        GetToken ->
--            (model, getToken)
--        TokenReceived (Ok result) ->
--            ( { model | token = result }, Cmd.none)
--        --
--        TokenReceived (Err _) ->
--            ( { model | errorMsg = "There was an error" }, Cmd.none)
--        --
--        ClickRegister ->
--            ( model, authUser )
--        LogOut ->
--            ( { model | username = "", token = "" }, Cmd.none)

view : Model -> Html Msg
view model =
    let 
        loggedIn : Bool
        loggedIn = True

        authBoxView = 
            let 
                showError : String
                showError = 
                    if String.isEmpty model.errorMsg then
                        "hidden"
                    else 
                        ""
                greeting : String
                greeting = "Hello, " ++ "CUnty" ++ "!"
            in
                if loggedIn then
                    div [ id "greeting"]
                        [ h3 [ class "text-center"] [ text greeting ]
                        , p [ class "text-center"] [ text "You can access protected quotes"]
                        , p [ class "text-center"]
                            [ button [ class "btn btn-danger"] [ text "Log Out"]
                            ]
                        ]
                else
                    div [ id "form"]
                        [ h2 [ class "text-center" ] [ text "Log In or Register" ]
                        , p [ class "help-block"] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password. Then click Register."]
                        , div [ class showError ] 
                            [ div [class "alert alert-danger"] [ text model.errorMsg]
                            ]
                        , div [ class "form-group row"]
                            [ div [ class "col-md-offset-2 col-md-8"]
                                [ label [ for "username"] [ text "Username:"]
                                , input [ id "username", type_ "text", class "form-control"] []
                                ]
                            ]
                        , div [ class "form-group row"]
                            [ div [ class "col-md-offset-2 col-md-8"]
                                [ label [ for "password"] [ text "Password:"]
                                , input [ id "password", type_ "password", class "form-control" ] []
                                ]
                            ]
                        , div [ class "text-center"]
                            [ button [class "btn btn-primary"] [text "Login"]
                            , button [class "btn btn-link"] [text "Register"]
                            ]
                        ]
    in
        div [ class "container" ]
            [ h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
            , p [ class "text-center" ]
                [ button [ class "btn btn-success", onClick GetQuote ] [ text "Grab a quote!" ]
                ]
              -- Blockquote with quote
            , blockquote []
                [ p [] [ text model.quote ]
                ]
            , div [ class "jumbotron text-left" ]
                [ -- Login/Register form or user greeting
                  authBoxView
                ]
            ]



registerUrl = "http://localhost:8000/users"

getQuote : Cmd Msg
getQuote =
    Http.get
    { url = "http://localhost:3001/api/random-quote"
    , expect = Http.expectString QuoteReceived
    }

main: Program () Model Msg
main = 
    Browser.element
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

init :  () -> (Model, Cmd Msg)
init _ = 
    (Model "" "", getQuote)
