module Cache where

import Signal exposing (Mailbox, Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Task exposing (Task)
import Effects
import StartApp
import ElmFire exposing (..)

type alias Cache a =
  StartApp.Config (Entry a) Action

type Entry a =
  Loading |
  SubscriptionFailed ElmFire.Error |
  DecodingFailed String |
  Success a

type Action =
  Nop |
  Subscribed Subscription |
  SubscriptionError ElmFire.Error |
  ValueChanged Value

cache : Decoder a -> String -> Cache a
cache decoder url =
  let result =
        {
          init = init,
          update = update,
          view = dummyView,
          inputs = inputs
        }
      init =
        (Loading, initialTask)
      initialTask =
        (ElmFire.subscribe
          (\snapshot ->
            ValueChanged snapshot.value |> Signal.send actionMailbox.address
          )
          dummyTask
          (valueChanged noOrder)
          (url |> fromUrl)
        |> Task.map Subscribed)
        `Task.onError` (\error ->
          SubscriptionError error |> Task.succeed
        )
        |> Effects.task
      actionMailbox =
        Signal.mailbox Nop
      update action model =
        case action of
          Nop ->
            (model, Effects.none)
          Subscribed subscription ->
            (model, Effects.none)
          SubscriptionError error ->
            (SubscriptionFailed error, Effects.none)
          ValueChanged value ->
            let model =
                  case value |> Decode.decodeValue decoder of
                    Ok data ->
                      Success data
                    Err message ->
                      DecodingFailed message
            in (model, Effects.none)
      inputs =
        [actionMailbox.signal]
  in result

dummyTask : a -> Task x ()
dummyTask _ =
  Task.succeed ()

dummyView : Address Action -> Entry a -> Html
dummyView _ _ =
  Html.text ""
