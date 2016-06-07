module Markdown exposing (toDom)

import Debug
import Html exposing (..)
import String
import VirtualDom

type ParseState = Text | Em | Strong | EndStrong

type alias State any =
  { domTree: List (VirtualDom.Node any)
  , length: Int
  , parseState: ParseState
  , text: String
  , index: Int
  }

initialParseState : Int -> State any
initialParseState length =
  { domTree = []
  , length = length
  , parseState = Text
  , text = ""
  , index = 0
  }

toDom : String -> List (VirtualDom.Node any)
toDom string =
  let
    parseState = initialParseState(String.length(string))

    {domTree} =
      string
        |> String.toList
        |> List.foldl processChar parseState
  in
    domTree

processChar : Char -> State any -> State any
processChar char state =
  let
    atLastChar : Bool
    atLastChar = state.index + 1 == state.length
  in
    case state.parseState of
      Text ->
        if atLastChar then
          let
            innerText = state.text ++ String.fromChar(char)
            domTree = List.append state.domTree [text innerText]
          in
            { state
            | domTree = domTree
            , index = state.index + 1
            }
        else
          if char == '*' then
            { state
            | parseState = Em
            , domTree = List.append state.domTree [text state.text]
            , text = ""
            , index = state.index + 1
            }
          else
            { state
            | text = state.text ++ String.fromChar(char)
            , index = state.index + 1
            }
      Em ->
        if char == '*' && state.text /= "" then
          { state
          | parseState = Text
          , domTree = List.append state.domTree [em [] [text state.text]]
          , text = ""
          , index = state.index + 1
          }
        else if atLastChar then
          let
            innerText = "*" ++ state.text ++ String.fromChar(char)
            domTree = List.append state.domTree [text innerText]
          in
            { state
            | domTree = domTree
            , index = state.index + 1
            }
        else if char == '*' && state.text == "" then
          { state
          | parseState = Strong
          , index = state.index + 1
          }
        else
          { state
          | text = state.text ++ String.fromChar(char)
          , index = state.index + 1
          }
      Strong ->
        if atLastChar then
          let
            innerText = "**" ++ state.text ++ String.fromChar(char)
            domTree = List.append state.domTree [text innerText]
          in
            { state
            | domTree = domTree
            , index = state.index + 1
            }
        else if char == '*' then
          { state
          | parseState = EndStrong
          , index = state.index + 1
          }
        else
          { state
          | text = state.text ++ String.fromChar(char)
          , index = state.index + 1
          }
      EndStrong ->
        if char == '*' then
          { state
          | parseState = Text
          , domTree = List.append state.domTree [strong [] [text state.text]]
          , text = ""
          , index = state.index + 1
          }
        else if atLastChar then
          let
            innerText = "**" ++ state.text ++ "*" ++ String.fromChar(char)
            domTree = List.append state.domTree [text innerText]
          in
          { state
          | domTree = domTree
          , index = state.index + 1
          }
        else
          { state
          | parseState = Text
          , text = "**" ++ state.text ++ "*" ++ String.fromChar(char)
          , index = state.index + 1
          }
