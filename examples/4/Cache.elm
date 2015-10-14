module Cache where

import Dict exposing (Dict)
import Signal exposing (Mailbox, Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Task exposing (Task)
import Effects exposing (Effects)
import ElmFire exposing (..)

type alias Cache a =
  Dict String (Entry a)

type Entry a =
  NotSubscribed |
  SubscriptionFailed ElmFire.Error |
  Loading Subscription |
  DecodingFailed Subscription String |
  Success Subscription a

get : String -> Cache a -> Entry a
get url cache =
  cache |> Dict.get url |> Maybe.withDefault NotSubscribed

init : Cache a
init =
  Dict.empty

dummyTask : a -> Task x ()
dummyTask _ =
  Task.succeed ()

type Action =
  Event Event |
  Request Request

type Event =
  NoEvent |
  Subscribed String Subscription |
  SubscriptionError String ElmFire.Error |
  Unsubscribed String |
  UnsubscriptionError String ElmFire.Error |
  ValueChanged String Subscription Value

type Request =
  NoRequest |
  Subscribe String |
  Unsubscribe String

eventMailbox : Mailbox Event
eventMailbox =
  Signal.mailbox NoEvent

update : Decoder a -> Action -> Cache a -> (Cache a, Effects Action)
update decoder action model =
  case action of
    Event event ->
      let updatedModel =
            case event of
              NoEvent ->
                model
              Subscribed url subscription ->
                model |> Dict.update url (\maybeEntry ->
                  case maybeEntry of
                    Just entry ->
                      case entry of
                        NotSubscribed ->
                          Just (Loading subscription)
                        _ ->
                          maybeEntry
                    Nothing ->
                      Just (Loading subscription)
                )
              Unsubscribed url ->
                model |> Dict.remove url
              SubscriptionError url error ->
                model |> Dict.insert url (SubscriptionFailed error)
              UnsubscriptionError url error ->
                model
              ValueChanged url subscription value ->
                let result =
                      model |> Dict.insert url entry
                    entry =
                      case value |> Decode.decodeValue decoder of
                        Ok data ->
                          Success subscription data
                        Err message ->
                          DecodingFailed subscription message
                in result
      in (updatedModel, Effects.none)
    Request request ->
      let effects =
            case request of
              Subscribe url ->
                (subscribe
                  (\snapshot ->
                    ValueChanged url snapshot.subscription snapshot.value |> Signal.send eventMailbox.address
                  )
                  dummyTask
                  (valueChanged noOrder)
                  (url |> fromUrl)
                |> Task.map (\subscription ->
                  Event <| Subscribed url subscription
                ))
                `Task.onError` (\error ->
                  (Event <| SubscriptionError url error) |> Task.succeed
                )
                |> Effects.task
              Unsubscribe url ->
                case model |> get url of
                  NotSubscribed ->
                    Effects.none
                  Loading subscription ->
                    unsubscribeEffects subscription url
                  SubscriptionFailed _ ->
                    Effects.none
                  DecodingFailed subscription _ ->
                    unsubscribeEffects subscription url
                  Success subscription _ ->
                    unsubscribeEffects subscription url
          unsubscribeEffects subscription url =
            (unsubscribe subscription
            |> Task.map (\_ ->
              Event <| Unsubscribed url
            ))
            `Task.onError` (\error ->
              (Event <| UnsubscriptionError url error) |> Task.succeed
            )
            |> Effects.task
      in (model, effects)

inputs : List (Signal Action)
inputs =
  [
    eventMailbox.signal |> Signal.map Event
  ]
