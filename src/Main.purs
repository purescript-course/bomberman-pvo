module Main
  ( Direction(..)
  , Tile(..)
  , World
  , Owner
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

import Debug
import Prelude

import Data.Array as Array
import Data.Grid (Grid(..), Coordinates, enumerate)
import Data.Grid as Grid
import Data.Int (even)
import Data.List (List(..), (..), (:), filter)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Reactor (Reactor, Widget(..), executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, widget)



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

data Owner = Player | AI
data Tile = Wall | Box | Bomb {time :: Int, owner :: Owner} | Explosion {time :: Int, owner :: Owner} | Empty
data Direction = Right | Left | Down | Up

derive instance ownerEq :: Eq Owner

derive instance tileEq :: Eq Tile

type World = { 
  player :: {coordinates :: Coordinates, playerHealth :: Int}, 
  enemy :: List{enemyCoordinates :: Coordinates, lastEnemyDir :: Int},
  board :: Grid Tile, 
  timer :: Int}

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
        enemy: ({enemyCoordinates: { x: width - 2, y: height -2},lastEnemyDir: 0} : {enemyCoordinates: { x: 1, y: height -2},lastEnemyDir: 0} : Nil),  
        timer: 0
        }
  where
  board = constructM width height (\point -> do
    x <- isBox point
    if isWall point then pure Wall else if x then pure Box else pure Empty)

draw :: World -> Drawing
draw { player: { coordinates}, board, enemy } = do
  drawGrid board drawTile
  fill Color.blue600 $ tile coordinates
  drawEnemies enemy
  where
  drawEnemies Nil = fill Color.gray500 $ tile {x:0,y:0}
  drawEnemies (Cons f r) = do
    fill Color.green600 $ tile f.enemyCoordinates
    drawEnemies r
  drawTile tile = 
    case tile of
      Empty -> Just Color.green50
      Box -> Just Color.blue300
      Wall -> Just Color.gray500
      Bomb{time: _} -> Just Color.red500
      Explosion {} -> Just Color.yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player:{ coordinates }, board, enemy, timer} <- getW
  let newBoard = updateGrid coordinates board Player
  let updatedBombs = updateBombs board
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> updateW_ {board: newBoard}
    Tick {} -> do
      
      let makeEnemiesMove = mapEnemies enemy timer board

      --this is really scuffed, but it works for now ,:D
      let enemyBombs = mapEnemyBombs enemy timer 
      let poi = filter (_ /= {x:0,y:0}) enemyBombs
      let newEnemyBomb = updateGrid (fromMaybe {x:0,y:0} (List.head $ poi)) updatedBombs AI
      updateW_ {board: newEnemyBomb, enemy: makeEnemiesMove}

      reducePlayerHealth
      checkPlayerHealth
      --progressEnemyCooldowns has to be under updateW_ for explosions to work
      progressEnemyCooldowns
      

    _ -> executeDefaultBehavior
  where
    mapEnemies Nil _ _ = Nil
    mapEnemies (Cons f r) timer board =
      (enemyAction f timer board) : (mapEnemies r timer board)

    mapEnemyBombs Nil _ = Nil
    mapEnemyBombs (Cons f r) timer =
      (enemyPlaceBomb f timer) : (mapEnemyBombs r timer)

    checkPlayerHealth = do
      {player: {playerHealth}} <- getW
      if playerHealth == 0 then do
          restart <- liftEffect initial
          updateW_ restart
      else
        executeDefaultBehavior

    bombChance :: Effect Boolean
    bombChance = do
      h <- (randomInt 1 5)
      case h of
        1 -> pure true
        _ -> pure false
    
    enemyPlaceBomb {enemyCoordinates} timer =
      let 
        bombBool = unsafePerformEffect bombChance
      in
        if bombBool && ((timer `mod` 50) == 0) then do
          enemyCoordinates
        else
          {x:0,y:0}

    --enemyAction :: {enemyCoordinates :: Coordinates, lastEnemyDir :: Int} -> Number -> Grid Tile
    enemyAction enemy@{enemyCoordinates, lastEnemyDir} timer board =
      case Grid.index board enemyCoordinates of
          Nothing -> enemy
          Just (Explosion {owner: Player}) -> {enemyCoordinates: {x:0,y:0}, lastEnemyDir}
          _ ->
            if ((timer `mod` 7) == 1) then 
              let 
                dir = unsafePerformEffect (generateRandomDirection lastEnemyDir)
              in
                moveEnemy board dir enemy
            else
              enemy

    progressEnemyCooldowns = do
      {board, timer} <- getW        
      let e = enumerate board
      let explosion = explosionCheck e 0 board
      updateW_ {board: explosion, timer: timer + 1}
      executeDefaultBehavior
    


reducePlayerHealth :: Reaction World
reducePlayerHealth = do
  {player: p@{ coordinates, playerHealth }, board} <- getW
  when (isExplosion coordinates board) $
    updateW_ { player: p {playerHealth = playerHealth - 1} }
  widget "label_hp" $ Label {content: show playerHealth}
  where
    isExplosion position board = 
      case Grid.index board position of
        Nothing -> false
        Just (Explosion {}) -> true
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


moveEnemy :: Grid Tile -> Coordinates -> 
  {enemyCoordinates :: Coordinates, lastEnemyDir :: Int} -> 
  {enemyCoordinates :: Coordinates, lastEnemyDir :: Int}
moveEnemy board { x: xd, y: yd } {enemyCoordinates: {x,y}, lastEnemyDir}=
  if (isEmpty newEnemyPosition board) then
    {enemyCoordinates: newEnemyPosition, lastEnemyDir: lDir}
  else
    {enemyCoordinates:{x,y}, lastEnemyDir}
  where
  isEmpty position board = Grid.index board position == Just Empty
  newEnemyPosition = { x: x + xd, y: y + yd }
  lDir = case {x: xd, y: yd} of
              { x: -1, y: 0 } -> 1
              { x: 1, y: 0 } -> 2
              { x: 0, y: 1 } -> 3 
              { x: 0, y: -1 } -> 4
              {x:_,y:_} -> 0


updateGrid :: Coordinates -> Grid Tile -> Owner -> Grid Tile
updateGrid cords board ownr = do
  let newBoard = (Grid.updateAt cords (Bomb{time: 50, owner: ownr}) board)
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
        Bomb {time: 0, owner} -> spy "" Explosion{time: 30, owner: owner}
        Bomb {time, owner} -> Bomb{time: time - 1, owner}
        Explosion {time: 0} -> Empty
        Explosion {time, owner} -> Explosion{time: time -1, owner}
        _ -> a
        
explosionCheck :: Array (Tuple Coordinates Tile) -> Int -> Grid Tile -> Grid Tile
explosionCheck grid i board = do
  let x = (fromMaybe (Tuple {x:0,y:0} Empty) (Array.index grid i))
  if i > (height*width) then
    board
  else if (snd x == Explosion{time: 30, owner: Player}) then
    explode (fst x) Player
  else if (snd x == Explosion{time: 30, owner: AI}) then
    explode (fst x) AI
  else
    explosionCheck grid (i+1) board   

  where 
  explode :: Coordinates -> Owner -> Grid Tile
  explode {x,y} ownr = do

    let a = megaExplode {x,y} 1 board Right ownr
    let b = megaExplode {x,y} 1 a Down ownr
    let c = megaExplode {x,y} 1 b Left ownr
    let d = megaExplode {x,y} 1 c Up ownr
    Grid.updateAt' {x: x, y: y} (Explosion{time: 29, owner: ownr}) d

  megaExplode :: Coordinates -> Int -> Grid Tile -> Direction -> Owner -> Grid Tile
  megaExplode {x,y} r grid dir ownr = do
    let newCords = 
              case dir of
                Right -> {x: x+r, y}
                Left -> {x: x-r, y}
                Up -> {x, y: y-r}
                Down -> {x, y: y+r}  
    case (fromMaybe Empty (Grid.index grid newCords)) of
      Wall -> grid
      Box -> Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid
      Bomb{owner} -> Grid.updateAt' newCords (Explosion{time: 31, owner: owner}) grid
      _ ->
          if r > 2 then
            Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid
          else
            megaExplode {x,y} (r+1) (Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid) dir ownr
  

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: p@{ coordinates }, board } <- getW
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

to2D :: Int -> Coordinates
to2D i = { x: i `mod` width, y: i / width }

constructM :: forall a. Int -> Int -> (Coordinates -> Effect a) -> Effect (Grid a )
constructM width height f = do
  (tiles :: Array a) <- tilesM
  pure (Grid tiles { width, height })
  where
  tilesM = map (Array.fromFoldable) (flipIt (List.fromFoldable xs)) 
  xs = map (f <<< to2D ) $0 .. (width * height)