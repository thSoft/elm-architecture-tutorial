module Counter where

import Signal exposing (Address)
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

type alias Context =
  {
    url: String,
    removeAddress: Address (),
    actionAddress: Address Action
  }

view : Context -> Cache.Entry Model -> Html
view context model =
  let result =
        div [] (remove :: children)
      remove =
        button [ onClick context.removeAddress () ] [ text "Remove" ]
      children =
        case model of
          Cache.NotSubscribed ->
            ["Subscribing to " ++ context.url ++ "..." |> text]
          Cache.Loading _ ->
            ["Loading " ++ context.url ++ "..." |> text]
          Cache.SubscriptionFailed message ->
            ["Can't subscribe to " ++ context.url |> text]
          Cache.DecodingFailed _ message ->
            ["Bad data at " ++ context.url ++ ": " ++ message |> text]
          Cache.Success _ value ->
            [ button [ onClick context.actionAddress Decrement ] [ text "-" ]
            , div [ countStyle ] [ value |> toString |> text ]
            , button [ onClick context.actionAddress Increment ] [ text "+" ]
            ]
  in result

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("display", "inline-block")
    , ("text-align", "center")
    ]
