module Cache where

import Signal exposing (Mailbox, Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Task exposing (Task)
import Effects exposing (Effects)
import ElmFire exposing (..)

type Model a =
  Loading |
  SubscriptionFailed ElmFire.Error |
  DecodingFailed String |
  Success a

init : String -> (Model a, Effects Action)
init url =
  let initialTask =
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
  in (Loading, initialTask)

dummyTask : a -> Task x ()
dummyTask _ =
  Task.succeed ()

type Action =
  Nop |
  Subscribed Subscription |
  SubscriptionError ElmFire.Error |
  ValueChanged Value

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox Nop

update : Decoder a -> Action -> Model a -> (Model a, Effects Action)
update decoder action model =
  case action of
    Nop ->
      (model, Effects.none)
    Subscribed subscription ->
      (model, Effects.none)
    SubscriptionError error ->
      (SubscriptionFailed error, Effects.none)
    ValueChanged value ->
      let updatedModel =
            case value |> Decode.decodeValue decoder of
              Ok data ->
                Success data
              Err message ->
                DecodingFailed message
      in (updatedModel, Effects.none)

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
