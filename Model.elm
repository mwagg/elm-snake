module Model exposing (Game, init, update, Msg(..), ArrowKey(..), Snake, Color(..), Block)

import Window
import Time exposing (Time)
import Random


type alias Game =
  { direction : Direction
  , dimensions : Window.Size
  , snake : Snake
  , isDead : Bool
  , fruit : Maybe Block
  , ateFruit : Bool
  }


type Color
  = Red
  | Green
  | Blue
  | Pink
  | Yellow


type alias Block =
  { x : Int
  , y : Int
  , color : Color
  }


type alias Snake =
  List Block


type Direction
  = Left
  | Right
  | Up
  | Down


type ArrowKey
  = NoKey
  | LeftKey
  | RightKey
  | UpKey
  | DownKey


type alias FruitSpawn =
  { position : ( Int, Int )
  , color : Color
  , chance : Int
  }


type Msg
  = ArrowPressed ArrowKey
  | SizeUpdated Window.Size
  | Tick Time
  | MaybeSpawnFruit FruitSpawn


initSnake : Snake
initSnake =
  [ Block 25 25 Red
  , Block 24 25 Green
  , Block 23 25 Blue
  ]


init : Game
init =
  { direction = Right
  , dimensions = Window.Size 0 0
  , snake = initSnake
  , isDead = False
  , fruit = Nothing
  , ateFruit = False
  }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
  case msg of
    ArrowPressed arrow ->
      ( updateDirection arrow game, Cmd.none )

    SizeUpdated dimensions ->
      ( { game | dimensions = dimensions }, Cmd.none )

    Tick time ->
      updateGame game

    MaybeSpawnFruit spawn ->
      if spawn.chance == 0 then
        ( spawnFruit game spawn, Cmd.none )
      else
        ( game, Cmd.none )


spawnFruit : Game -> FruitSpawn -> Game
spawnFruit game spawn =
  let
    ( x, y ) =
      spawn.position
  in
    { game | fruit = Just { x = x, y = y, color = spawn.color } }


updateGame : Game -> ( Game, Cmd Msg )
updateGame game =
  if game.isDead then
    ( game, Cmd.none )
  else
    ( game, Cmd.none )
      |> checkIfOutOfBounds
      |> checkIfEatenSelf
      |> checkIfAteFruit
      |> updateSnake
      |> updateFruit


checkIfEatenSelf : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfEatenSelf ( game, cmd ) =
  let
    head =
      snakeHead game.snake

    tail =
      List.drop 1 game.snake

    isDead =
      game.isDead || List.any (samePosition head) tail
  in
    ( { game | isDead = isDead }, cmd )


checkIfAteFruit : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfAteFruit ( game, cmd ) =
  let
    head =
      snakeHead game.snake
  in
    case game.fruit of
      Nothing ->
        ( { game | ateFruit = False }, cmd )

      Just fruit ->
        ( { game | ateFruit = samePosition head fruit }, cmd )


samePosition : Block -> Block -> Bool
samePosition a b =
  a.x == b.x && a.y == b.y


checkIfOutOfBounds : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfOutOfBounds ( game, cmd ) =
  let
    head =
      snakeHead game.snake

    isDead =
      (head.x == 0 && game.direction == Left)
        || (head.y == 0 && game.direction == Up)
        || (head.x == 49 && game.direction == Right)
        || (head.y == 49 && game.direction == Down)
  in
    ( { game | isDead = isDead }, cmd )


snakeHead : Snake -> Block
snakeHead snake =
  List.head snake
    |> Maybe.withDefault { x = 0, y = 0, color = Red }


updateFruit : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateFruit ( game, cmd ) =
  case game.fruit of
    Nothing ->
      let
        chance =
          Random.int 0 9

        x =
          Random.int 0 49

        y =
          Random.int 0 49

        pos =
          Random.pair x y
      in
        ( game, Random.generate MaybeSpawnFruit makeFruitSpawnGenerator )

    Just fruit ->
      if game.ateFruit then
        ( { game | fruit = Nothing }, cmd )
      else
        ( game, cmd )


makeFruitSpawnGenerator : Random.Generator FruitSpawn
makeFruitSpawnGenerator =
  let
    spawnPosition =
      Random.pair (Random.int 0 49) (Random.int 0 49)

    spawnChance =
      Random.int 0 9

    spawnColor =
      Random.map intToColor (Random.int 0 4)
  in
    Random.map3 (\pos chance color -> { position = pos, chance = chance, color = color }) spawnPosition spawnChance spawnColor


intToColor : Int -> Color
intToColor i =
  case i of
    1 ->
      Green

    2 ->
      Blue

    3 ->
      Pink

    4 ->
      Yellow

    default ->
      Red


updateSnake : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateSnake ( game, cmd ) =
  let
    head =
      snakeHead game.snake

    headColor =
      if game.ateFruit then
        case game.fruit of
          Nothing ->
            Red

          Just fruit ->
            fruit.color
      else
        head.color

    head' =
      case game.direction of
        Up ->
          { head | y = head.y - 1, color = headColor }

        Down ->
          { head | y = head.y + 1, color = headColor }

        Left ->
          { head | x = head.x - 1, color = headColor }

        Right ->
          { head | x = head.x + 1, color = headColor }

    tailPositions =
      if game.ateFruit then
        List.map position game.snake
      else
        List.take ((List.length game.snake) - 1) game.snake
          |> List.map position

    tailColors =
      if game.ateFruit then
        List.map color game.snake
      else
        List.drop 1 game.snake
          |> List.map color

    tail' =
      List.map2 blockFromPositionAndColor tailPositions tailColors
  in
    if game.isDead then
      ( game, cmd )
    else
      ( { game | snake = head' :: tail' }, cmd )


blockFromPositionAndColor : ( Int, Int ) -> Color -> Block
blockFromPositionAndColor ( x, y ) color =
  { x = x, y = y, color = color }


position : Block -> ( Int, Int )
position block =
  ( block.x, block.y )


color : Block -> Color
color block =
  block.color


updateDirection : ArrowKey -> Game -> Game
updateDirection key game =
  let
    { direction } =
      game

    direction' =
      if key == LeftKey && direction /= Right then
        Left
      else if key == RightKey && direction /= Left then
        Right
      else if key == UpKey && direction /= Down then
        Up
      else if key == DownKey && direction /= Up then
        Down
      else
        direction
  in
    { game | direction = direction' }
