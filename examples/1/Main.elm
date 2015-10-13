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

app : App Counter.Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }

type Action =
  CounterAction Counter.Action |
  CacheAction Cache.Action

counterCache : Cache Int
counterCache =
  Cache.cache
    Decode.int
    url

url : String
url =
  "https://thsoft.firebaseio.com/example"

init : (Counter.Model, Effects Action)
init =
  let (model, cacheEffects) =
        counterCache.init
      effects =
        cacheEffects |> Effects.map CacheAction
  in (model, effects)

update : Action -> Counter.Model -> (Counter.Model, Effects Action)
update action model =
  case action of
    CounterAction counterAction ->
      let (updatedModel, counterEffects) =
            Counter.update url counterAction model
          effects =
            counterEffects |> Effects.map CounterAction
      in (updatedModel, effects)
    CacheAction cacheAction ->
      let (updatedModel, cacheEffects) =
            counterCache.update cacheAction model
          effects =
            cacheEffects |> Effects.map CacheAction
      in (updatedModel, effects)

view : Address Action -> Counter.Model -> Html
view address model =
  Counter.view
    url
    (Signal.forwardTo address CounterAction)
    model

inputs : List (Signal Action)
inputs =
  counterCache.inputs |> List.map (\signal ->
    signal |> Signal.map CacheAction
  )

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
