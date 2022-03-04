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

import Color.Scheme.X11 (turquoise)
import Data.Array (filter, index)
import Data.Array as Array
import Data.Array.ST (withArray)
import Data.Grid (Grid(..), Coordinates, construct, enumerate, index, updateAt')
import Data.Grid as Grid
import Data.HeytingAlgebra.Generic (genericDisj)
import Data.Int (even)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML (b, elementNS)
import Reactor (Reactor, Widget(..), dimensions, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, widget)
import Type.Data.Boolean (class Not)
import Web.HTML.Event.BeforeUnloadEvent.EventTypes (beforeunload)
import Web.HTML.Event.EventTypes (offline)

playerMaxHp :: Int
playerMaxHp = 100
width :: Int
width = 15

height :: Int
height = 15

main :: Effect Unit
main = do 
  x <- reactor 
  runReactor x { title: "Bomberman", width, height,  
        widgets: [
          "section_hp" /\ Section {title: "Health"},
          "label_hp" /\ Label {content: show $ playerMaxHp}
        ]  }


data Tile = Wall | Box | Bomb {time :: Int} | Explosion {time :: Int} | Empty
data Direction = Right | Left | Down | Up

derive instance tileEq :: Eq Tile

type World = { 
  player :: {coordinates :: Coordinates, playerHealth :: Int}, 
  enemy :: Coordinates, 
  lastEnemyDir :: Int, 
  board :: Grid Tile, 
  timer :: Number, 
  bombCooldown :: Number}

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
  pure {initial: x, 
        draw, 
        handleEvent, 
        isPaused: const false
        }

initial :: Effect World
initial = do
  b <- board
  pure { 
        board: b, 
        player: {coordinates: { x: 1, y: 1}, playerHealth: 100}, 
        enemy: { x: width - 2, y: height -2}, 
        lastEnemyDir: 0, 
        timer: 0.0, 
        bombCooldown: 0.0
        }
  where
  board = constructM width height (\point -> do
    x <- isBox point
    if isWall point then pure Wall else if x then pure Box else pure Empty)

draw :: World -> Drawing
draw { player: p@{ coordinates, playerHealth }, board, enemy } = do
  drawGrid board drawTile
  fill Color.blue600 $ tile coordinates
  fill Color.green600 $ tile enemy
  where
  drawTile tile = 
    case tile of
      Empty -> Just Color.green50
      Box -> Just Color.blue300
      Wall -> Just Color.gray500
      Bomb{time: _} -> Just Color.red500
      Explosion {time} -> Just Color.yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player: p@{ coordinates, playerHealth }, board, enemy, timer, lastEnemyDir, bombCooldown} <- getW
  let newBoard = updateGrid coordinates board
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
        {board, player: p@{ coordinates, playerHealth }} <- getW        
        let e = enumerate board
        let explosion = explosionCheck e 0 board
        bombBool <- liftEffect bombChance
        reducePlayerHealth
        if playerHealth == 0 then do
          restart <- liftEffect initial
          updateW_ restart
        else
          if bombBool && (bombCooldown > 250.0) then do
            let newBomb = updateGrid enemy board
            updateW_ {board: newBomb, bombCooldown: 0.0}
          else
            if (timer / 7.0) == 1.0 then do
              dir <- liftEffect (generateRandomDirection lastEnemyDir)
              moveEnemy dir
              updateW_ {board: explosion, timer: 0.0, bombCooldown: bombCooldown + 1.0}
            else 
              updateW_ {board: explosion, timer: timer + 1.0, bombCooldown: bombCooldown + 1.0}

    _ -> executeDefaultBehavior


reducePlayerHealth = do
  {player: p@{ coordinates, playerHealth }, board} <- getW
  when (isExplosion coordinates board) $
    updateW_ { player: p {playerHealth = playerHealth - 1} }
  widget "label_hp" $ Label {content: show playerHealth}
  where
    isExplosion position board = 
      case Grid.index board position of
        Nothing -> false
        Just (Explosion {time}) -> true
        _ -> false


generateRandomDirection :: Int -> Effect Coordinates
generateRandomDirection lastDir = do
  h <- (randomInt 1 5)
  case h of
    1 -> if lastDir == 2 then pure {x:0,y:0} else pure { x: -1, y: 0 }
    2 -> if lastDir == 1 then pure {x:0,y:0} else pure { x: 1, y: 0 }
    3 -> if lastDir == 4 then pure {x:0,y:0} else pure { x: 0, y: 1 }
    4 -> if lastDir == 3 then pure {x:0,y:0} else pure { x: 0, y: -1 }
    _ -> pure {x:0,y:0}

bombChance :: Effect Boolean
bombChance = do
  h <- (randomInt 1 5)
  case h of
    1 -> pure true
    _ -> pure false

moveEnemy :: Coordinates -> Reaction World
moveEnemy { x: xd, y: yd } = do
  { enemy: { x, y }, board, lastEnemyDir } <- getW
  let newEnemyPosition = { x: x + xd, y: y + yd }
  let lDir = case {x: xd, y: yd} of
                { x: -1, y: 0 } -> 1
                { x: 1, y: 0 } -> 2
                { x: 0, y: 1 } -> 3 
                { x: 0, y: -1 } -> 4
                {x:_,y:_} -> lastEnemyDir
  updateW_ {lastEnemyDir: lDir}
  when (isEmpty newEnemyPosition board) $
    updateW_ { enemy: newEnemyPosition}
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
        Bomb {time: 0} -> Explosion{time: 30}
        Bomb {time} -> Bomb{time: time - 1}
        Explosion {time: 0} -> Empty
        Explosion {time} -> Explosion{time: time -1}
        _ -> a
        
explosionCheck :: Array (Tuple Coordinates Tile) -> Int -> Grid Tile -> Grid Tile
explosionCheck grid i board = do
  let x = (fromMaybe (Tuple {x:0,y:0} Empty) (Array.index grid i))
  if i > (height*width) then
    board
  else if snd x == Explosion{time: 30} then
    explode (fst x)
  else
    explosionCheck grid (i+1) board   

  where 
  explode :: Coordinates -> Grid Tile
  explode {x,y} = do

    let a = megaExplode {x,y} 1 board Right
    let b = megaExplode {x,y} 1 a Down
    let c = megaExplode {x,y} 1 b Left
    let d = megaExplode {x,y} 1 c Up
    Grid.updateAt' {x: x, y: y} (Explosion{time: 29}) d

  megaExplode :: Coordinates -> Int -> Grid Tile -> Direction -> Grid Tile
  megaExplode {x,y} r grid dir = do
    let newCords = 
              case dir of
                Right -> {x: x+r, y}
                Left -> {x: x-r, y}
                Up -> {x, y: y-r}
                Down -> {x, y: y+r}  
    case (fromMaybe Empty (Grid.index grid newCords)) of
      Wall -> grid
      Box -> Grid.updateAt' newCords (Explosion{time: 29}) grid
      Bomb{time} -> Grid.updateAt' newCords (Explosion{time: 31}) grid
      _ ->
          if r > 2 then
            Grid.updateAt' newCords (Explosion{time: 29}) grid
          else
            megaExplode {x,y} (r+1) (Grid.updateAt' newCords (Explosion{time: 29}) grid) dir
  

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: p@{ coordinates, playerHealth }, board } <- getW
  let x = coordinates.x
  let y = coordinates.y
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: p {coordinates = newPlayerPosition} }
  where
  isEmpty position board = Grid.index board position == Just Empty





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