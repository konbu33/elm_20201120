port module A exposing (main)

import Browser
import Browser.Navigation as Nav exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as JD exposing (..)
import Url
import Url.Parser as UP exposing (..)

import File
import File.Select as FS exposing (..)

port receivedUserInfo : (User -> msg) -> Sub msg

-- MAIN
main : Program JD.Value Model Msg
main =
  Browser.application 
    { init = init 
    , update = update
    , subscriptions = subsc
    , view = view
    , onUrlRequest = UrlRequested
    , onUrlChange = UrlChanged
    }

-- MODEL
type alias Model =
  { flagsData : FlagsData
  , url : Url.Url
  , navKey : Nav.Key
  , title : String
  , userInfo : Maybe User
  , route : Route
  }

init : JD.Value ->  Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  let
    result = JD.decodeValue initUrlDecoder flags
    flagsData = case result of
                  Ok data ->
                    data
                  Err e ->
                    { initUrl = "FlagsData Decoer Error." }
  in
    ( Model flagsData url key "abc" Nothing NotFound
    , Cmd.none
    )

-- FlagsData
type alias FlagsData =
  { initUrl : String
  }

-- Decoder Url
initUrlDecoder : Decoder FlagsData
initUrlDecoder =
  JD.map FlagsData
    (JD.at ["initUrl"] JD.string)

-- Routing Url
type Route
  = Top
  | About
  | Home
  | NotFound

route : Parser (Route -> a) a
route =
  UP.oneOf [
    UP.map Top top
  , UP.map About (UP.s "about")
  , UP.map Home (UP.s "home")
  ]

toRoute : String -> Route
toRoute urlStr =
  case Url.fromString urlStr of
    Nothing -> 
      NotFound
    Just url ->
      Maybe.withDefault NotFound (parse route url)


-- Decoder User
type alias User =
  { id : String
  , name : String
  , age : Int
  }

userDecoder : Decoder User
userDecoder =
  JD.map3 User
    (JD.at ["id"] JD.string)
    (JD.at ["name"] JD.string)
    (JD.at ["age"] JD.int)

-- UPDATE
type Msg 
  = UrlRequested Browser.UrlRequest
  | UrlChanged Url.Url
  | ReceivedUserInfo User

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg : " msg of
    UrlRequested urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.navKey <| Url.toString url
          )
  
        Browser.External href ->
          ( model
          , Nav.load href
          )
  
    UrlChanged url ->
      ( { model 
        | url = url 
        , route = toRoute <| Url.toString url
        }
      , Cmd.none
      )
  
    ReceivedUserInfo userInfo ->
      let
        _ = Debug.log "userInfo : " userInfo

      in
        ( { model | userInfo = Just userInfo }
        , Cmd.none
        )

-- SUBSC
subsc : Model -> Sub Msg
subsc model =
  Sub.batch [
    receivedUserInfo ReceivedUserInfo
  ]

-- VIEW
view : Model -> Browser.Document Msg
view model =
  { title = model.title
  , body = [
    div []
      [
        div [] [ text <| Url.toString <| model.url ]
      , div [] [ text <| Debug.toString <| model.flagsData ]
      , div [] [ text <| Debug.toString <| model.route ]
      , div [] [ text <| Debug.toString <| model.userInfo ]
      , ul []
          [
            viewLink "/"
          , viewLink "/home"
          , viewLink "/about"
          , viewLink "/other"
          ]
      , div []
        [
          case model.route of
            NotFound ->
              div [] [ text "NotFound PAGE" ]
            Top ->
              div [] [ text "TOP PAGE" ]
            Home ->
              div [] [ text "HOME PAGE" ]
            About ->
              div [] [ text "ABOUT PAGE" ]
        ]
      ]
  ]}

viewLink : String -> Html Msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
