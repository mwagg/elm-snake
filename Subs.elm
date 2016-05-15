module Subs exposing (subscriptions, initCmds)

import Keyboard
import Model exposing (ArrowKey(..), Msg(..), Game)
import Window
import Task
import Time


subscriptions : Model.Game -> Sub Model.Msg
subscriptions model =
  Sub.batch [ arrowChanged, windowDimensionsChanged, tick ]


initCmds : Cmd Model.Msg
initCmds =
  Task.perform SizeUpdated SizeUpdated Window.size


windowDimensionsChanged : Sub Model.Msg
windowDimensionsChanged =
  Window.resizes Model.SizeUpdated


tick : Sub Model.Msg
tick =
  Time.every (100 * Time.millisecond) Model.Tick


arrowChanged : Sub Model.Msg
arrowChanged =
  Keyboard.downs toArrowChanged


toArrowChanged : Keyboard.KeyCode -> Msg
toArrowChanged code =
  case code of
    37 ->
      ArrowPressed LeftKey

    38 ->
      ArrowPressed UpKey

    39 ->
      ArrowPressed RightKey

    40 ->
      ArrowPressed DownKey

    default ->
      ArrowPressed NoKey
