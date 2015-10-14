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
  Int

-- UPDATE

type Action =
  NoAction |
  Increment |
  Decrement

update : String -> Action -> Cache.Entry Model -> Effects Action
update url action model =
  let result =
        case action of
          NoAction ->
            Effects.none
          Increment ->
            effects 1
          Decrement ->
            effects -1
      effects delta =
        (url
        |> ElmFire.fromUrl
        |> ElmFire.set (currentValue + delta |> Encode.int)
        |> Task.mapError (Debug.log "ElmFire.set failed")
        |> Task.map (always NoAction))
        `Task.onError` (always (NoAction |> Task.succeed))
        |> Effects.task
      currentValue =
        case model of
          Cache.Success _ value ->
            value
          _ ->
            0
  in result

-- VIEW

view : String -> Signal.Address Action -> Cache.Entry Model -> Html
view url address model =
  let result =
        div [] children
      children =
        case model of
          Cache.NotSubscribed ->
            ["Subscribing to " ++ url ++ "..." |> text]
          Cache.Loading _ ->
            ["Loading " ++ url ++ "..." |> text]
          Cache.SubscriptionFailed message ->
            ["Can't subscribe to " ++ url |> text]
          Cache.DecodingFailed _ message ->
            ["Bad data at " ++ url ++ ": " ++ message |> text]
          Cache.Success _ value ->
            [ button [ onClick address Decrement ] [ text "-" ]
            , div [ countStyle ] [ value |> toString |> text ]
            , button [ onClick address Increment ] [ text "+" ]
            ]
  in result

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("display", "inline-block")
    , ("text-align", "center")
    ]
