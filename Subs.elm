module Subs exposing (subscriptions, initCmds)

import Keyboard
import Model exposing (ArrowKey(..), Msg(..), Game)
import Window
import Task
import Time


subscriptions : Game -> Sub Msg
subscriptions model =
  Sub.batch [ arrowChanged, windowDimensionsChanged, tick ]


initCmds : Cmd Msg
initCmds =
  Task.perform SizeUpdated SizeUpdated Window.size


windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
  Window.resizes SizeUpdated


tick : Sub Msg
tick =
  Time.every (100 * Time.millisecond) Tick


arrowChanged : Sub Msg
arrowChanged =
  Keyboard.downs toArrowChanged


toArrowChanged : Keyboard.KeyCode -> Msg
toArrowChanged code =
  case code of
    32 ->
      ArrowPressed Space

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
