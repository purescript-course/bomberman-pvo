module Main
  ( Tile(..)
  , World
  , constructM
  , draw
  , flipIt
  , handleEvent
  , height
  , initial
  , isBox
  , isCorner
  , isWall
  , main
  , movePlayer
  , to2D
  , updateBombs
  , updateGrid
  , width
  )
  where

import Data.List
import Data.Tuple
import Debug
import Prelude

import Data.Array (filter, index)
import Data.Array as Array
import Data.Array.ST (withArray)
import Data.Grid (Grid(..), Coordinates, construct, enumerate, updateAt', index)
import Data.Grid as Grid
import Data.HeytingAlgebra.Generic (genericDisj)
import Data.Int (even)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML (elementNS)
import Reactor (Reactor, dimensions, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)
import Type.Data.Boolean (class Not)



width :: Int
width = 9

height :: Int
height = 9

main :: Effect Unit
main = do 
  x <- reactor 
  runReactor x { title: "Bomberman", width, height }


data Tile = Wall | Box | Bomb {time :: Int} | Explosion {time :: Int, r :: Int} | Empty

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile}

isWall :: Coordinates -> Boolean
isWall { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || ((even x) && (even y))

isCorner :: Coordinates -> Boolean
isCorner { x, y } = (x == 1 || x == width - 2) && (y == 1 || y == height - 2)


isBox :: Coordinates -> Effect Boolean
isBox { x, y } = do
  h <- (randomInt 1 3)
  if isWall {x, y} || isCorner {x, y} then pure false
  else if h < 3 then pure false
  else pure true


reactor :: Effect (Reactor World)
reactor = do
  x <- initial
  pure { initial: x, draw, handleEvent, isPaused: const false }

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
  drawTile tile = 
    case tile of
      Empty -> Just Color.green50
      Box -> Just Color.blue500
      Wall -> Just Color.gray500
      Bomb{time: _} -> Just Color.red500
      Explosion {time} -> Just Color.yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player, board} <- getW
  let newBoard = updateGrid player board
  let updatedBombs = updateBombs board
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> updateW_ {board: newBoard}
    Tick {delta} ->
      do
        updateW_ {board: updatedBombs}
        {board} <- getW
        let e = enumerate board
        let explosion = explosionCheck e 0 board
        updateW_ {board: explosion}

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
  let newBoard = (Grid.updateAt cords (Bomb{time: 50}) board)
  case newBoard of 
    Nothing -> board
    Just x -> x

updateBombs :: Grid Tile -> Grid Tile
updateBombs grid = do
  let arr = Array.fromFoldable grid
  Grid (map x arr) {height: height, width: width}
  where
    x a = do
      case a of
        Bomb {time: 0} -> Explosion{time: 50, r: 3}
        Bomb {time} -> Bomb{time: time - 1}
        Explosion {time: 0} -> Empty
        Explosion {time} -> Explosion{time: time -1, r: 3}
        _ -> a
        


explosionCheck :: Array (Tuple Coordinates Tile) -> Int -> Grid Tile -> Grid Tile
explosionCheck grid i board = do
  let x = (fromMaybe (Tuple {x:0,y:0} Empty) (Array.index grid i))
  if i > (height*width) then
    board
  else if snd x == Explosion{time: 50, r: 3} then
    explode (fst x)
  else
    explosionCheck grid (i+1) board
  where 
  explode :: Coordinates -> Grid Tile
  explode {x,y} = do

    let a = explodeX {x,y} 3 board
    let b = explodeY {x,y} 3 a
    Grid.updateAt' {x: x, y: y} (Explosion{time: 49, r: 3}) b

  explodeX :: Coordinates -> Int -> Grid Tile -> Grid Tile
  explodeX {x,y} r grid = 
    case (fromMaybe Empty (Grid.index grid {x:x+r, y})) of
      Wall -> grid
      _ ->
          if r < -2 then
            Grid.updateAt' {x: x+r, y: y} (Explosion{time: 49, r: 3}) grid
          else
            explodeX {x,y} (r-1) (Grid.updateAt' {x: x+r, y: y} (Explosion{time: 49, r: 3}) grid)

  explodeY {x,y} r grid =
    case (fromMaybe Empty (Grid.index grid {x:x, y:y+r})) of
      Wall -> grid
      _ ->
        if r < -2 then
          Grid.updateAt' {x: x, y: y+r} (Explosion{time: 49, r: 3}) grid
        else
          explodeY {x,y} (r-1) (Grid.updateAt' {x: x, y: y+r} (Explosion{time: 49, r: 3}) grid)




flipIt :: forall a m. Monad m => List (m a) -> m (List a)
flipIt Nil = pure Nil
flipIt (Cons f r) = do
  x <- f
  y <- flipIt r
  pure $ Cons x y

{-coolerFlipIt :: forall a b m. Monad m => List a -> (a -> m b) -> m (List b)
coolerFlipIt Nil _ = pure Nil 
coolerFlipIt (Cons f r) func = do
  x <- func f
  y <- coolerFlipIt r func
  pure $ Cons x y-}

to2D :: Int -> Int -> Coordinates
to2D width i = { x: i `mod` width, y: i / width }

constructM :: forall a. Int -> Int -> (Coordinates -> Effect a) -> Effect (Grid a )
constructM width height f = do
  (tiles :: Array a) <- tilesM
  pure (Grid tiles { width, height })
  where
  tilesM = map (Array.fromFoldable) (flipIt (List.fromFoldable xs)) 
  xs = map (f <<< to2D width) $0 .. (width * height)