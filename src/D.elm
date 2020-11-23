module D exposing (main)

-- Image upload with a drag and drop zone.
--
-- Dependencies:
--   elm install elm/file
--   elm install elm/json
--

import Browser
import Browser.Navigation as Nav exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Url
import File.Download exposing (url)
import Task



-- MAIN


main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = OnUrlRequested
    , onUrlChange = OnUrlChanged
    }



-- MODEL


type alias Model =
  { hover : Bool
  , files : List File
  , url : Url.Url
  , navKey : Nav.Key
  }


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  (Model False [] url key
  , Cmd.none
  )



-- UPDATE


type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFiles File (List File)
  | GotFile
  | OnUrlRequested Browser.UrlRequest
  | OnUrlChanged Url.Url


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg : " msg of
    OnUrlRequested urlReqest ->
      case urlReqest of
        Browser.Internal url ->
          ( model
          , Cmd.none
          )
        
        Browser.External href ->
          ( model 
          , Cmd.none
          )
    
    OnUrlChanged url ->
      ( model
      , Cmd.none
      )

    Pick ->
      ( model
      , Select.files ["image/*"] GotFiles
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFiles file files ->
      ( { model
            | files = file :: files
            , hover = False
        }
      , Cmd.none
      )

    GotFile ->
      ( model
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "d & d"
  , body = [
      div
        [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "height" "100px"
        , style "margin" "100px auto"
        , style "padding" "20px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ button [ onClick Pick ] [ text "Upload Images" ]
        , span [ style "color" "#ccc" ] [ text (Debug.toString model) ]
        ]
      , div
        [
        --  preventDefaultOn "drop" (D.map hijack (D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)) )
        --  hijackOn "drop" dropDecoder
        --, hijackOn "dragenter" (D.succeed DragEnter)
          preventDefaultOn "drop" (D.succeed (GotFile, True))
        , preventDefaultOn "dragover" (D.succeed (DragEnter, True))
        --, hijackOn "dragover" (D.succeed DragEnter)
        --, hijackOn "dragleave" (D.succeed DragLeave)
        , style "width" "100px"
        , style "height" "100px"
        , style "padding" "100px"
        , style "background" "#BBDD44"
        ]
        [
          text "original D & D"
        ]
    ]
  }


dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)