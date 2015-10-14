module Main where

import Signal exposing (Address, Mailbox)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Html exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import StartApp exposing (App)
import Debug
import ElmFire exposing (Location)
import Cache exposing (Cache)
import Counter

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

app : App Model
app =
  StartApp.start
    {
      init = init,
      update = update,
      view = view,
      inputs = inputs
    }

type alias Model =
  {
    cache: Cache Counter.Model,
    urls: List String
  }

init : (Model, Effects Action)
init =
  let model =
        {
          cache = cache,
          urls = []
        }
      cache =
        Cache.init
      subscribe action query =
        (ElmFire.subscribe
          (\snapshot ->
            snapshot.reference |> ElmFire.toUrl |> action |> Signal.send actionMailbox.address
          )
          Cache.dummyTask
          (query ElmFire.noOrder)
          (rootUrl |> ElmFire.fromUrl)
        |> Task.map (always NoAction))
        `Task.onError` (always (NoAction |> Task.succeed))
        |> Effects.task
      effects =
        Effects.batch [
          subscribe Created ElmFire.childAdded,
          subscribe Removed ElmFire.childRemoved
        ]
  in (model, effects)

rootUrl : String
rootUrl =
  "https://thsoft.firebaseio.com/elm-architecture-tutorial/4"

type Action =
  NoAction |
  Create |
  Created String |
  Remove String |
  Removed String |
  CounterAction String Counter.Action |
  CacheAction Cache.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoAction ->
      (model, Effects.none)
    Create ->
      let effects =
            (pushLocation
            |> ElmFire.set (Encode.int 0)
            |> Task.mapError (Debug.log "ElmFire.set failed")
            |> Task.map (always NoAction))
            `Task.onError` (always (NoAction |> Task.succeed))
            |> Effects.task
      in (model, effects)
    Created url ->
      let updatedModel =
            { model | urls <- model.urls ++ [url] }
          effects =
            url |> Cache.Subscribe |> Cache.Request |> CacheAction |> Task.succeed |> Effects.task
      in (updatedModel, effects)
    Remove url ->
      let effects =
            (url
            |> ElmFire.fromUrl
            |> ElmFire.remove
            |> Task.mapError (Debug.log "ElmFire.remove failed")
            |> Task.map (always NoAction))
            `Task.onError` (always (NoAction |> Task.succeed))
            |> Effects.task
      in (model, effects)
    Removed url ->
      let updatedModel =
            { model | urls <- model.urls |> remove url }
      in (updatedModel, Effects.none)
    CounterAction url counterAction ->
      let counterEffects =
            Counter.update url counterAction (model.cache |> Cache.get url)
          effects =
            counterEffects |> Effects.map (CounterAction url)
      in (model, effects)
    CacheAction cacheAction ->
      let updatedModel =
            { model | cache <- updatedCache }
          (updatedCache, cacheEffects) =
            Cache.update Decode.int cacheAction model.cache
          effects =
            cacheEffects |> Effects.map CacheAction
      in (updatedModel, effects)

pushLocation : Location
pushLocation =
  rootUrl |> ElmFire.fromUrl |> ElmFire.push

remove : a -> List a -> List a
remove element list =
  list |> List.filter (\elem -> elem /= element)

view : Address Action -> Model -> Html
view address model =
  let result =
        Html.div [] (insert :: counterViews)
      insert =
        button [ onClick address Create ] [ text "Add" ]
      counterViews =
        model.urls |> List.sort |> List.map (\url ->
          Counter.view
            {
              url = url,
              removeAddress = (Signal.forwardTo address (always <| Remove url)),
              actionAddress = (Signal.forwardTo address (CounterAction url))
            }
            (model.cache |> Cache.get url)
        )
  in result

inputs : List (Signal Action)
inputs =
  let result =
        fromMailbox :: fromCache
      fromCache =
        Cache.inputs |> List.map (\signal ->
          signal |> Signal.map CacheAction
        )
      fromMailbox =
        actionMailbox.signal
  in result

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoAction
