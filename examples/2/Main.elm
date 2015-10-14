module Main where

import Signal exposing (Address)
import Json.Decode as Decode
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Effects, Never)
import StartApp exposing (App)
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
  Cache Counter.Model

init : (Model, Effects Action)
init =
  let (model, cacheEffects) =
        Cache.init urls
      effects =
        cacheEffects |> Effects.map CacheAction
  in (model, effects)

type Action =
  CounterAction String Counter.Action |
  CacheAction Cache.Action

urls : List String
urls =
  [
    rootUrl ++ "0",
    rootUrl ++ "1"
  ]

rootUrl : String
rootUrl =
  "https://thsoft.firebaseio.com/elm-architecture-tutorial/2/"


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    CounterAction url counterAction ->
      let counterEffects =
            Counter.update url counterAction (model |> Cache.get url)
          effects =
            counterEffects |> Effects.map (CounterAction url)
      in (model, effects)
    CacheAction cacheAction ->
      let (updatedModel, cacheEffects) =
            Cache.update Decode.int cacheAction model
          effects =
            cacheEffects |> Effects.map CacheAction
      in (updatedModel, effects)

view : Address Action -> Model -> Html
view address model =
  let result =
        Html.div [] counterViews
      counterViews =
        urls |> List.map (\url ->
          Counter.view
            url
            (Signal.forwardTo address (CounterAction url))
            (model |> Cache.get url)
        )
  in result

inputs : List (Signal Action)
inputs =
  Cache.inputs |> List.map (\signal ->
    signal |> Signal.map CacheAction
  )
