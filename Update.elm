module Update exposing (update)

import Subs
import Model exposing (..)


update : Subs.Msg -> Model.Game -> ( Model.Game, Cmd Subs.Msg )
update msg game =
  case msg of
    Subs.ArrowPressed arrow ->
      ( updateDirection arrow game, Cmd.none )


updateDirection : Subs.Arrow -> Model.Game -> Model.Game
updateDirection arrow game =
  let
    direction =
      game.direction

    direction' =
      if arrow == Subs.Left && direction /= Direction.Right then
        Model.Left
      else if arrow == Subs.Right && direction /= Left then
        Model.Right
      else if arrow == Subs.Up && direction /= Down then
        Model.Up
      else if arrow == Subs.Down && direction /= Down then
        Model.Down
      else
        direction
  in
    { game | direction = direction' }
