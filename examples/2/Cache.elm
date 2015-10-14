module Cache where

import Dict exposing (Dict)
import Signal exposing (Mailbox, Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Task exposing (Task)
import Effects exposing (Effects)
import StartApp
import ElmFire exposing (..)

type alias Cache a =
  Dict String (Entry a)

type Entry a =
  NotSubscribed |
  Loading |
  SubscriptionFailed ElmFire.Error |
  DecodingFailed String |
  Success a

get : String -> Cache a -> Entry a
get url cache =
  cache |> Dict.get url |> Maybe.withDefault NotSubscribed

init : List String -> (Cache a, Effects Action)
init urls =
  let initialCache =
        urls
        |> List.map (\url ->
          (url, Loading)
        )
        |> Dict.fromList
      initialTasks =
        urls
        |> List.map (\url ->
          (ElmFire.subscribe
            (\snapshot ->
              ValueChanged url snapshot.value |> Signal.send actionMailbox.address
            )
            dummyTask
            (valueChanged noOrder)
            (url |> fromUrl)
          |> Task.map (Subscribed url))
          `Task.onError` (\error ->
            SubscriptionError url error |> Task.succeed
          )
          |> Effects.task
        )
        |> Effects.batch
  in (initialCache, initialTasks)

dummyTask : a -> Task x ()
dummyTask _ =
  Task.succeed ()

type Action =
  Nop |
  Subscribed String Subscription |
  SubscriptionError String ElmFire.Error |
  ValueChanged String Value

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox Nop

update : Decoder a -> Action -> Cache a -> (Cache a, Effects Action)
update decoder action model =
  case action of
    Nop ->
      (model, Effects.none)
    Subscribed url subscription ->
      (model, Effects.none)
    SubscriptionError url error ->
      let updatedCache =
            model |> Dict.update url (\_ ->
              Just <| SubscriptionFailed error
            )
      in (updatedCache, Effects.none)
    ValueChanged url value ->
      let updatedCache =
            model |> Dict.update url (\_ ->
              Just <| case value |> Decode.decodeValue decoder of
                Ok data ->
                  Success data
                Err message ->
                  DecodingFailed message
            )
      in (updatedCache, Effects.none)

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
