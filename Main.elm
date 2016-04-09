module Main (..) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Signal
import Keyboard
import Time
import Window
import Random


-- Inputs


type alias Input =
  { arrows : { x : Int, y : Int }
  , dimensions : ( Int, Int )
  }


inputs : Signal Input
inputs =
  let
    tick =
      Time.fps 12

    rawInputs =
      Signal.map2 toInput Keyboard.arrows Window.dimensions

    toInput =
      \arrows dimensions ->
        { arrows = arrows
        , dimensions = dimensions
        }
  in
    Signal.sampleOn tick rawInputs



-- Update


genRandoms : Random.Seed -> ( ( ( Int, Int ), Float ), Random.Seed )
genRandoms seed =
  let
    halfSize =
      round (blockCount / 2)

    positionGen =
      Random.pair (Random.int -halfSize halfSize) (Random.int -halfSize halfSize)

    spawnGen =
      Random.float 0 1

    gameGenerator =
      Random.pair positionGen spawnGen
  in
    Random.generate gameGenerator seed


updateGame : Input -> Game -> Game
updateGame input game =
  let
    game' =
      if game.running then
        updatePlayingGame input game
      else
        game
  in
    { game' | dimensions = input.dimensions }


updatePlayingGame : Input -> Game -> Game
updatePlayingGame input game =
  let
    { direction } =
      game

    ( ( ( fruitX, fruitY ), spawn ), newSeed ) =
      genRandoms game.seed

    newDirection =
      if input.arrows.x == -1 && direction /= Right then
        Left
      else if input.arrows.x == 1 && direction /= Left then
        Right
      else if input.arrows.y == -1 && direction /= Up then
        Down
      else if input.arrows.y == 1 && direction /= Down then
        Up
      else
        game.direction

    ateFruit =
      case game.fruit of
        Just f ->
          f == getHead game.snake

        Nothing ->
          False

    newFruit =
      case ateFruit of
        True ->
          Nothing

        False ->
          case game.fruit of
            Just fruit ->
              game.fruit

            Nothing ->
              if spawn <= 0.1 then
                Just ( fruitX, fruitY )
              else
                Nothing

    newSnake =
      updateSnake newDirection ateFruit game.snake

    collision =
      checkCollision newSnake

    outOfBounds =
      checkOutOfBounds newSnake

    newRunning =
      not (collision || outOfBounds)
  in
    { game
      | direction = newDirection
      , snake = newSnake
      , fruit = newFruit
      , seed = newSeed
      , running = newRunning
    }


checkOutOfBounds : List Block -> Bool
checkOutOfBounds snake =
  let
    halfBlocks =
      round (blockCount / 2)

    ( x, y ) =
      case snake of
        [] ->
          ( 0, 0 )

        x :: _ ->
          x
  in
    case snake of
      [] ->
        False

      head :: _ ->
        (x < -halfBlocks)
          || (y < -halfBlocks)
          || (x > halfBlocks)
          || (y > halfBlocks)


checkCollision : List Block -> Bool
checkCollision snake =
  case snake of
    [] ->
      False

    [ x ] ->
      False

    head :: tail ->
      List.foldl (\block collided -> block == head || collided) False tail


updateSnake : Direction -> Bool -> List Block -> List Block
updateSnake direction ateFruit snake =
  let
    oldHead =
      getHead snake

    newHead =
      updateHead direction oldHead

    newTailSize =
      case ateFruit of
        True ->
          List.length snake

        False ->
          (List.length snake) - 1

    newTail =
      List.take newTailSize snake
  in
    newHead :: newTail


getHead : List Block -> Block
getHead snake =
  case List.head snake of
    Just a ->
      a

    Nothing ->
      ( 0, 0 )


updateHead : Direction -> ( Int, Int ) -> ( Int, Int )
updateHead direction ( x, y ) =
  case direction of
    Left ->
      ( x - 1, y )

    Right ->
      ( x + 1, y )

    Up ->
      ( x, y + 1 )

    Down ->
      ( x, y - 1 )



-- Model


type Direction
  = Up
  | Down
  | Left
  | Right


type alias Block =
  ( Int, Int )


type alias Game =
  { snake : List Block
  , fruit : Maybe Block
  , direction : Direction
  , seed : Random.Seed
  , dimensions : ( Int, Int )
  , running : Bool
  }


initialGame : Game
initialGame =
  { snake =
      [ ( 0, 0 )
      , ( -1, 0 )
      , ( -2, 0 )
      ]
  , fruit = Just ( -3, 20 )
  , direction = Right
  , seed = Random.initialSeed 42
  , dimensions = ( 0, 0 )
  , running = True
  }


game : Signal Game
game =
  Signal.foldp updateGame initialGame inputs



-- Views


blockSize : number
blockSize =
  10


blockCount : number
blockCount =
  50


gameSize : number
gameSize =
  blockSize * blockCount


renderBackground : Form
renderBackground =
  square gameSize
    |> filled Color.black


renderBlock : Block -> Form
renderBlock block =
  let
    ( x, y ) =
      block

    position =
      ( toFloat (x * blockSize), toFloat (y * blockSize) )
  in
    square blockSize
      |> filled Color.white
      |> move position


renderBlocks : List Block -> Form
renderBlocks blocks =
  group (List.map renderBlock blocks)


renderGame : Game -> Element
renderGame game =
  let
    snake =
      renderBlocks game.snake

    fruit =
      case game.fruit of
        Just fruit ->
          renderBlock fruit

        Nothing ->
          toForm Graphics.Element.empty

    content =
      group [ renderBackground, snake, fruit ]
  in
    displayFullScreen game.dimensions content


displayFullScreen : ( Int, Int ) -> Form -> Element
displayFullScreen ( w, h ) content =
  let
    gameScale =
      min (toFloat w / gameSize) (toFloat h / gameSize)
  in
    collage w h [ content |> scale gameScale ]


main : Signal Element
main =
  Signal.map renderGame game
