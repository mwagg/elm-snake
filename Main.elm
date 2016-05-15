module Main exposing (..)

import View
import Subs
import Model
import Html.App exposing (program)


main : Program Never
main =
  program { init = ( Model.init, Subs.initCmds ), update = Model.update, view = View.render, subscriptions = Subs.subscriptions }
