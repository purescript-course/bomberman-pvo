module Main
  ( Bomb
  , Tile(..)
  , World
  , coolerFlipIt
  , draw
  , flipIt
  , flipIts
  , handleEvent
  , height
  , initial
  , isBox
  , isCorner
  , isWall
  , main
  , movePlayer
  , reactor
  , updateGrid
  , width
  )
  where

import Data.List
import Prelude

import Data.Array as Array
import Data.Grid (Coordinates, Grid(..), construct)
import Data.Grid as Grid
import Data.Int (even)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Reactor (Reactor, dimensions, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)

width :: Int
width = 9

height :: Int
height = 9

main :: Effect Unit
main = do 
  x <- reactor 
  runReactor x { title: "Bomberman", width, height }

type Bomb = Int

data Tile = Wall | Box | Bomb | Empty

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || ((even x) && (even y))

isCorner :: Coordinates -> Boolean
isCorner { x, y } = (x == 1 || x == width - 2) && (y == 1 || y == height - 2)

-- giveNumber :: Int
-- giveNumber = do 
--   x <- liftEffect (randomInt 1 3)
---   x

isBox :: Coordinates -> Effect Boolean
isBox { x, y } = do
  h <- (randomInt 1 3)
  if isWall {x, y} || isCorner {x, y} then pure false
  else if h == 1 then pure false
  else pure true


reactor :: Effect (Reactor World)
reactor = do
  x <- initial
  pure { initial: x, draw, handleEvent, isPaused: const true }

initial :: Effect World
initial = do
  b <- board
  pure { board: b, player: { x: 1, y: 1}}
  where
  board = constructM width height (\point -> do
    x <- isBox point
    if isWall point then pure Wall else if x then pure Box else pure Empty)

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill Color.blue400 $ tile player
  where
  drawTile Empty = Just Color.green50
  drawTile Box = Just Color.blue300
  drawTile Wall = Just Color.gray500
  drawTile Bomb = Just Color.red500

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player, board} <- getW
  let newBoard = updateGrid player board
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> updateW_ {board: newBoard}

    _ -> executeDefaultBehavior

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty

updateGrid :: Coordinates -> Grid Tile -> Grid Tile
updateGrid cords board = do
  let newBoard = (Grid.updateAt cords Bomb board)
  case newBoard of 
    Nothing -> board
    Just x -> x



flipIts :: forall a. List (Effect a) -> List a
flipIts Nil = Nil 
flipIts (Cons f r) = Cons (unsafePerformEffect f) (flipIts r)

flipIt :: forall a m. Monad m => List (m a) -> m (List a)
flipIt Nil = pure Nil
flipIt (Cons f r) = do
  x <- f
  y <- flipIt r
  pure $ Cons x y

coolerFlipIt :: forall a b m. Monad m => List a -> (a -> m b) -> m (List b)
coolerFlipIt Nil _ = pure Nil 
coolerFlipIt (Cons f r) func = do
  x <- func f
  y <- coolerFlipIt r func
  pure $ Cons x y

to2D :: Int -> Int -> Coordinates
to2D width i = { x: i `mod` width, y: i / width }

constructM :: forall a. Int -> Int -> (Coordinates -> Effect a) -> Effect (Grid a )
constructM width height f = do
  (tiles :: Array a) <- tilesM
  pure (Grid tiles { width, height })
  where
  tilesM = map (Array.fromFoldable) (flipIt (List.fromFoldable xs)) 
  xs = map (f <<< to2D width) $0 .. (width * height)