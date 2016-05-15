module View exposing (render)

import Model exposing (Game, Msg, Color(..), Snake, Block)
import Html
import Html.Attributes exposing (style)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result, in')
import Window


size : String
size =
  "100"


backgroundColor : Attribute Msg
backgroundColor =
  fill "#333333"


render : Game -> Html.Html Msg
render game =
  let
    ( scaledWidth, scaledHeight ) =
      scale game.dimensions

    parentStyle =
      style [ ( "margin", "0 auto" ), ( "display", "block" ) ]
  in
    svg
      [ width scaledWidth, height scaledHeight, viewBox "0 0 50 50", parentStyle ]
      ([ renderBackground ]
        ++ renderSnake game.snake
        ++ renderFruit game.fruit
      )


renderBackground : Svg Msg
renderBackground =
  rect [ x "0", y "0", width size, height size, backgroundColor ] []


renderSnake : Snake -> List (Svg Msg)
renderSnake snake =
  List.map renderBlock snake


renderBlock : Block -> Svg Msg
renderBlock block =
  let
    ( strX, strY ) =
      ( toString block.x, toString block.y )
  in
    rect [ x strX, y strY, width "1", height "1", toFill block.color, rx "0.2" ] []


renderFruit : Maybe Block -> List (Svg Msg)
renderFruit fruit =
  case fruit of
    Nothing ->
      []

    Just fruit ->
      [ renderBlock fruit ]


toFill : Color -> Attribute Msg
toFill color =
  case color of
    Red ->
      fill "rgb(255, 0, 0)"

    Green ->
      fill "#87BD1C"

    Blue ->
      fill "#5DC4E7"

    Pink ->
      fill "#FFA2A2"

    Yellow ->
      fill "#EBFF00"


scale : Window.Size -> ( String, String )
scale size =
  let
    toPixelStr =
      \i -> round i |> toString

    ( fWidth, fHeight ) =
      ( toFloat size.width, toFloat size.height )

    ( scaledX, scaledY ) =
      if fWidth > fHeight then
        ( fHeight / fWidth, 1.0 )
      else
        ( 1.0, fWidth / fHeight )
  in
    ( toPixelStr (fWidth * scaledX), toPixelStr (fHeight * scaledY) )


glowFilter : Svg Msg
glowFilter =
  defs
    []
    [ filter
        [ id "glow", x "-30%", y "-30%", width "160%", height "160%" ]
        [ feGaussianBlur [ stdDeviation "10 10", result "glow" ] []
        , feMerge
            []
            [ feMergeNode [ in' "glow" ] []
            , feMergeNode [ in' "glow" ] []
            , feMergeNode [ in' "glow" ] []
            ]
        ]
    ]
