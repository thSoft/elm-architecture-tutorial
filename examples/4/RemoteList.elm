module RemoteList where

import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Effects exposing (Effects, Never)
import Debug
import ElmFire

type alias Model =
  List String

init : String -> (Model, Effects Action)
init rootUrl =
  let result =
        (model, effects)
      model =
        []
      effects =
        Effects.batch [
          subscribe Created ElmFire.childAdded,
          subscribe Removed ElmFire.childRemoved
        ]
      subscribe action query =
        (ElmFire.subscribe
          (\snapshot ->
            snapshot.reference |> ElmFire.toUrl |> action |> Signal.send actionMailbox.address
          )
          (always <| Task.succeed ())
          (query ElmFire.noOrder)
          (rootUrl |> ElmFire.fromUrl)
        |> Task.mapError (Debug.log "ElmFire.subscribe failed")
        |> Task.map (always NoAction))
        `Task.onError` (always (NoAction |> Task.succeed))
        |> Effects.task
  in result

type Action =
  NoAction |
  Created String |
  Removed String

update : Action -> Model -> Model
update action model =
  case action of
    NoAction ->
      model
    Created url ->
      model ++ [url]
    Removed url ->
      model |> remove url

remove : a -> List a -> List a
remove element list =
  list |> List.filter (\elem -> elem /= element)

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox NoAction
