module Counter where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Effects exposing (Effects)
import Json.Encode as Encode
import Debug
import ElmFire exposing (Location)
import Cache

-- MODEL

type alias Model =
  Cache.Model Int

-- UPDATE

type Action =
  Nop |
  Increment |
  Decrement

update : String -> Action -> Model -> (Model, Effects Action)
update url action model =
  let result =
        case action of
          Nop ->
            (model, Effects.none)
          Increment ->
            (model, effects 1)
          Decrement ->
            (model, effects -1)
      effects delta =
        (url
        |> ElmFire.fromUrl
        |> ElmFire.set (currentValue + delta |> Encode.int)
        |> Task.mapError (Debug.log "ElmFire.set failed")
        |> Task.map (always Nop))
        `Task.onError` (always (Nop |> Task.succeed))
        |> Effects.task
      currentValue =
        case model of
          Cache.Success value ->
            value
          _ ->
            0
  in result


-- VIEW

view : String -> Signal.Address Action -> Model -> Html
view url address model =
  case model of
    Cache.Loading ->
      "Loading " ++ url ++ "..." |> text
    Cache.SubscriptionFailed message ->
      "Can't subscribe to " ++ url |> text
    Cache.DecodingFailed message ->
      "Bad data at " ++ url ++ ": " ++ message |> text
    Cache.Success value ->
      div []
        [ button [ onClick address Decrement ] [ text "-" ]
        , div [ countStyle ] [ value |> toString |> text ]
        , button [ onClick address Increment ] [ text "+" ]
        ]


countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("display", "inline-block")
    , ("text-align", "center")
    ]
