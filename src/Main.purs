module Main where

import Prelude

import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Math ((%))
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
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Box | Empty

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || ((even x) && (even y))

isCorner :: Coordinates -> Boolean
isCorner { x, y } = (x == 1 || x == width - 1) && (y == 1 || y == height - 1)

-- giveNumber :: Int
-- giveNumber = do 
--   x <- liftEffect (randomInt 1 3)
--   x

isBox :: Coordinates -> Boolean
isBox { x, y } = 
  if isWall {x, y} || isCorner {x, y} then false
  else if unsafePerformEffect (randomInt 1 3) == 1 then false
  else true


reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: 1, y: 1}, board }
  where
  board = Grid.construct width height (\point -> if isWall point then Wall else if isBox point then Box else Empty)

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill Color.blue400 $ tile player
  where
  drawTile Empty = Just Color.green50
  drawTile Box = Just Color.blue300
  drawTile Wall = Just Color.gray500

handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }

    _ -> executeDefaultBehavior

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty