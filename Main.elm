import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (contenteditable)
import Html.Events
import Json.Decode as Json
import Regex exposing (..)
import String
import VirtualDom
import Markdown

type alias Model =
  { source: String
  , domTree: List (VirtualDom.Node Msg)
  }

type Msg = UpdateSource String

main : Program Never
main =
  Html.beginnerProgram
    { model = initModel
    , update = update
    , view = view
    }

initModel : Model
initModel =
  { source = ""
  , domTree = []
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateSource value ->
      let
        domTree = value |> Markdown.toDom
      in
        { model | source = value, domTree = domTree }

view : Model -> Html Msg
view model =
  div []
    [ div [contenteditable True, onInput handleInput] []
    , div [] model.domTree
    ]

onInput : (String -> msg) -> Attribute msg
onInput tagger =
  Html.Events.on "input" (Json.map tagger targetTextContent)

targetTextContent : Json.Decoder String
targetTextContent =
  Json.at ["target", "textContent"] Json.string

handleInput value =
  UpdateSource value
