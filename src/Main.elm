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
    , errorMsg : Maybe String
    , username : String
    , password : String
    , token : String
    }

type Msg 
    = GetQuote
    | QuoteReceived (Result Http.Error String)
    | SetUsername String
    | SetPassword String
    | ClickLogin 
    | ClickRegister 
--    | GetToken
    | TokenReceived (Result Http.Error String)
    | LogOut 

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
            ( { model | errorMsg = Just "There was an error" }, Cmd.none)
        SetUsername u ->
            ( { model | username = u }, Cmd.none)
        SetPassword pw ->
            ( { model | password = pw }, Cmd.none)
        --
        ClickLogin ->
            ( model, authUser model loginUserUrl )
--        GetToken ->
--            (model, getToken)
        TokenReceived (Ok result) ->
            ( { model | token = result }, Cmd.none)
--        --
        TokenReceived (Err error) ->
            ( { model | errorMsg =  handleHttpError error }, Cmd.none)
--        --
        ClickRegister ->
            ( model, authUser model registerUserUrl)
        LogOut ->
            ( { model | username = "", token = "" }, Cmd.none)

view : Model -> Html Msg
view model =
    let 
        loggedIn : Bool
        loggedIn = 
          case String.length model.token of
            0 ->
              False
            _ ->
              True


        authBoxView = 
            let 
                showError : String
                showError = 
                    case model.errorMsg of
                      Just err ->
                        "hidden"
                      Nothing ->
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
                            [ div [class "alert alert-danger"] [ getErroText model.errorMsg |> text ]
                            ]
                        , div [ class "form-group row"]
                            [ div [ class "col-md-offset-2 col-md-8"]
                                [ label [ for "username"] [ text "Username:"]
                                , input [ id "username", type_ "text", class "form-control", onInput SetUsername] []
                                ]
                            ]
                        , div [ class "form-group row"]
                            [ div [ class "col-md-offset-2 col-md-8"]
                                [ label [ for "password"] [ text "Password:"]
                                , input [ id "password", type_ "password", class "form-control", onInput SetPassword ] []
                                ]
                            ]
                        , div [ class "text-center"]
                            [ button [class "btn btn-primary", onClick ClickLogin ] [text "Login"]
                            , button [class "btn btn-link", onClick ClickRegister ] [text "Register"]
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
                --[ p [] [ text model.username ]
                --, p [] [ text model.password ]
                --, p [] [ text model.quote ]
                [ p [] [ "token: " ++ model.token |> text ]
                , p [] [ "error:" ++  (getErroText model.errorMsg) |> text]
                ]
            , div [ class "jumbotron text-left" ]
                [ -- Login/Register form or user greeting
                  authBoxView
                ]
            ]

decodeToken : String -> String
decodeToken json =
  let
      value = 
       Decode.decodeString (field "access_token" Decode.string) json
        
  in
      case value of 
        Ok token ->
          token
        Err error ->
          handleJsonError error


getErroText : Maybe String -> String
getErroText maybe =
  case maybe of 
    Just str ->
      str
    Nothing ->
      ""
handleJsonError : Decode.Error -> String 
handleJsonError err = 
  case err of 
    Failure errMsg _ ->
      errMsg
    _ ->
      "Error: Invalid JSON"

handleHttpError : Http.Error -> Maybe String
handleHttpError httpError =
    case httpError of
        Http.BadUrl message ->
            Just message

        Http.Timeout ->
            Just "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            Just "Unable to reach server."

        Http.BadStatus statusCode ->
            Just ("Request failed with status code: " ++ String.fromInt statusCode)

        Http.BadBody message ->
            Just message

registerUserUrl = "http://localhost:3001/users"
loginUserUrl = "http://localhost:3001/sessions/create"

authUser : Model -> String -> Cmd Msg
authUser model authUrl = 
  let 
      body = 
        Encode.object
          [ ("username", Encode.string model.username)
          , ("password", Encode.string model.password)
--          , ("extra", Encode.string "")
          ]
        |> Http.jsonBody
  in
    Http.post
    { url = authUrl
    , body = body
    , expect = Http.expectJson TokenReceived (field "access_token" Decode.string)
    }

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
    ( Model "" Nothing  "" "" ""
    , getQuote
    )
