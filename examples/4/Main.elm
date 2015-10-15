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
import RemoteList
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
    remoteList: RemoteList.Model
  }

init : (Model, Effects Action)
init =
  let result =
        (model, effects)
      model =
        {
          cache =
            cacheModel,
          remoteList =
            remoteListModel
        }
      effects =
        remoteListEffects |> Effects.map RemoteListAction
      cacheModel =
        Cache.init
      (remoteListModel, remoteListEffects) =
        RemoteList.init rootUrl
  in result

rootUrl : String
rootUrl =
  "https://thsoft.firebaseio.com/elm-architecture-tutorial/4"

type Action =
  NoAction |
  Create |
  Remove String |
  CounterAction String Counter.Action |
  CacheAction Cache.Action |
  RemoteListAction RemoteList.Action

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
    RemoteListAction remoteListAction ->
      let updatedModel =
            { model | remoteList <- updatedRemoteList }
          updatedRemoteList =
            RemoteList.update remoteListAction model.remoteList
          effects =
            case remoteListAction of
              RemoteList.Created url ->
                url |> Cache.Subscribe |> Cache.Request |> CacheAction |> Task.succeed |> Effects.task
              RemoteList.Removed url ->
                url |> Cache.Unsubscribe |> Cache.Request |> CacheAction |> Task.succeed |> Effects.task
              _ ->
                Effects.none
      in (updatedModel, effects)

pushLocation : Location
pushLocation =
  rootUrl |> ElmFire.fromUrl |> ElmFire.push

view : Address Action -> Model -> Html
view address model =
  let result =
        Html.div [] (insert :: counterViews)
      insert =
        button [ onClick address Create ] [ text "Add" ]
      counterViews =
        model.remoteList |> List.sort |> List.map (\url ->
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
  [
    Cache.inputs |> List.map (Signal.map CacheAction),
    RemoteList.inputs |> List.map (Signal.map RemoteListAction)
  ] |> List.concat
