module Main
  ( Direction(..)
  , Tile(..)
  , World
  , Owner
  , Buff
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
  , placeBomb
  , width
  )
  where


import Prelude

import Data.Array as Array
import Data.Grid (Grid(..), Coordinates, enumerate)
import Data.Grid as Grid
import Data.Int (even)
import Data.List (List(..), (..), (:), filter, length)
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



playerMaxHP :: Int
playerMaxHP = 100

globalRespawnTime :: Int
globalRespawnTime = 375

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
          "label_hp" /\ Label {content: show $ playerMaxHP},
          "section_score" /\ Section {title: "Score"},
          "label_score" /\ Label {content: show $ 0},
          "section_currentBuff" /\ Section {title: "Active Buff"},
          "label_currentBuff" /\ Label {content: show $ "Nothing"}
        ]  }

data Owner = Player | AI
data Tile = 
  Wall | 
  Box | 
  Bomb {time :: Int, owner :: Owner} | 
  Explosion {time :: Int, owner :: Owner} | 
  Buff {type :: Buff} | 
  Empty
data Direction = Right | Left | Down | Up | None

data Buff = Slow | Immortal | Heal | Power | Default
{-
  slow - enemies move slower
  immortal - you cannot take damage for a while
  heal - heals 25 hp (but it cannot go over the maximum hp)
  power - makes explosions 1 larger and explosions can go through boxes, but they dont drop any buffs
-}

derive instance buffEq :: Eq Buff
derive instance ownerEq :: Eq Owner
derive instance directionEq :: Eq Direction
derive instance tileEq :: Eq Tile

type World = { 
  player :: {
    coordinates :: Coordinates, 
    playerHealth :: Int, 
    buff :: Buff, 
    buffTimer :: Int}, 
  enemy :: List{
    enemyCoordinates :: Coordinates, 
    lastEnemyDir :: Direction, 
    respawnTime :: Int, 
    respawnCoordinates :: Coordinates, 
    wasKilled :: Boolean},
  board :: Grid Tile, 
  timer :: Int,
  score :: Int
  }

isWall :: Coordinates -> Boolean
isWall { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || ((even x) && (even y))

isCorner :: Coordinates -> Boolean
isCorner crds@{ x, y } = 
  ((x == 1 || x == width - 2) && (y == 1 || y == height - 2)) || 
  crds == {x:1,y:2} || 
  crds == {x:2,y:1} || 
  crds == {x:width - 2,y:2} || 
  crds == {x:width - 3,y:1} || 
  crds == {x:1,y:height - 3} || 
  crds == {x:2,y:height - 2} ||
  crds == {x:width - 2,y:height - 3} || 
  crds == {x:width - 3,y:height - 2}



isBox :: Coordinates -> Effect Boolean
isBox { x, y } = do
  rInt <- (randomInt 1 10)
  if isWall {x, y} || isCorner {x, y} then pure false
  else if rInt < 6 then pure false
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
        player: {coordinates: { x: 1, y: 1}, playerHealth: playerMaxHP, buff: Default, buffTimer: 0}, 
        enemy: ({enemyCoordinates: { x: width - 2, y: height -2},lastEnemyDir: None, respawnTime: 0, respawnCoordinates: { x: width - 2, y: height -2}, wasKilled: false} : 
          {enemyCoordinates: { x: 1, y: height -2},lastEnemyDir: None, respawnTime: 0, respawnCoordinates: { x: 1, y: height -2}, wasKilled: false} :
          {enemyCoordinates: { x: width - 2, y: 1},lastEnemyDir: None, respawnTime: 0, respawnCoordinates: { x: width - 2, y: 1}, wasKilled: false} : Nil),  
        timer: 0,
        score: 0
        }
  where
  board = constructM (\point -> do
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
      Buff {type: Slow} -> Just Color.pink900
      Buff {type: Immortal} -> Just Color.blue900
      Buff {type: Heal} -> Just Color.green900
      Buff {type: Power} -> Just Color.yellow800
      _ -> Nothing

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player:{ coordinates, buff }, board, enemy, timer, score} <- getW
  let newBoard = placeBomb coordinates board Player
  let updatedBombs = updateBombs board
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> updateW_ {board: newBoard}
    Tick {} -> do
      let makeEnemiesMove = mapEnemies enemy timer board buff
      let checkNewKills = length (filter (\x -> x.wasKilled) makeEnemiesMove)      
      updateW_ { score: score + checkNewKills} 
      widget "label_score" $ Label {content: show score}


      --this is really scuffed, but it works for now ,:D
      let enemyBombs = mapEnemyBombs enemy timer 
      let filteredBombs = filter (_ /= {x:0,y:0}) enemyBombs
      let newEnemyBomb = placeBomb (fromMaybe {x:0,y:0} (List.head $ filteredBombs)) updatedBombs AI
      updateW_ {board: newEnemyBomb, enemy: makeEnemiesMove}
      if buff /= Immortal then
        reducePlayerHealth
      else
        executeDefaultBehavior
      checkPlayerHealth
      --progressCooldowns has to be under updateW_ for explosions to work
      progressCooldowns
      
    _ -> executeDefaultBehavior
  where
    mapEnemies :: 
      List{enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean} -> 
      Int -> 
      Grid Tile -> 
      Buff -> 
      List{enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean}
    mapEnemies Nil _ _ _ = Nil
    mapEnemies (Cons f r) timer board b =
      (enemyAction f timer board b) : (mapEnemies r timer board b)

    mapEnemyBombs :: 
      List{enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean} ->
      Int ->
      List Coordinates
    mapEnemyBombs Nil _ = Nil
    mapEnemyBombs (Cons f r) timer =
      (enemyPlaceBomb f timer) : (mapEnemyBombs r timer)

    checkPlayerHealth :: Reaction World
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

    enemyPlaceBomb :: 
      {enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean} -> 
      Int -> 
      Coordinates
    enemyPlaceBomb {enemyCoordinates} timer =
      let 
        --I'm not sure how to get rid of this..
        bombBool = unsafePerformEffect bombChance
      in
        if bombBool && ((timer `mod` 50) == 0) then do
          enemyCoordinates
        else
          {x:0,y:0}

    enemyAction :: 
      {enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean} ->
      Int ->
      Grid Tile ->
      Buff ->
      {enemyCoordinates :: Coordinates, 
        lastEnemyDir :: Direction, 
        respawnTime :: Int, 
        respawnCoordinates :: Coordinates, 
        wasKilled :: Boolean}
    enemyAction enemy@{enemyCoordinates, lastEnemyDir, respawnTime, respawnCoordinates} timer board buff =
      let 
        speedDivision = if buff == Slow then 20 else 7
      in
        case Grid.index board enemyCoordinates of
            Nothing -> enemy
            Just (Explosion {owner: Player}) -> 
              enemy {enemyCoordinates = {x:0,y:0}, respawnTime = globalRespawnTime, wasKilled = true}
            _ ->
              if (enemyCoordinates == {x:0,y:0}) && (respawnTime == 0) then
                enemy {enemyCoordinates = respawnCoordinates, lastEnemyDir = None}
              else
                if ((timer `mod` (speedDivision)) == 1) then 
                  let 
                    --Same with this
                    dir = unsafePerformEffect (generateRandomDirection lastEnemyDir)
                  in
                    moveEnemy board dir enemy
                else
                  enemy { respawnTime = respawnTime - 1, wasKilled = false}

    progressCooldowns :: Reaction World
    progressCooldowns = do
      {board, timer, player: p@{ buff, buffTimer}} <- getW        
      let e = enumerate board      
      case buff of
        Default -> do
          let explosion = explosionCheck e 0 board Default
          updateW_ {board: explosion, timer: timer + 1}
        _ -> do
          let explosion = explosionCheck e 0 board buff
          updateW_ {board: explosion, timer: timer + 1}
          if buffTimer == 375 then do
            updateW_ {player:p { buff = Default, buffTimer = 0}}
            widget "label_currentBuff" $ Label {content: show "Nothing"}
          else
            updateW_ {player: p { buffTimer = buffTimer + 1 }}

    
generateRandomDirection :: Direction -> Effect Coordinates
generateRandomDirection lastDir = do
  h <- (randomInt 1 5)
  case h of
    1 -> if lastDir == Right then pure {x:0,y:0} else pure { x: -1, y: 0 }
    2 -> if lastDir == Left then pure {x:0,y:0} else pure { x: 1, y: 0 }
    3 -> if lastDir == Up then pure {x:0,y:0} else pure { x: 0, y: 1 }
    4 -> if lastDir == Down then pure {x:0,y:0} else pure { x: 0, y: -1 }
    _ -> pure {x:0,y:0}

moveEnemy :: 
  Grid Tile -> 
  Coordinates -> 
  {enemyCoordinates :: Coordinates, 
    lastEnemyDir :: Direction, 
    respawnTime :: Int, 
    respawnCoordinates :: Coordinates, 
    wasKilled :: Boolean} -> 
  {enemyCoordinates :: Coordinates, 
  lastEnemyDir :: Direction, 
  respawnTime :: Int, 
  respawnCoordinates :: Coordinates, 
  wasKilled :: Boolean}
moveEnemy board { x: xd, y: yd } enemy@{enemyCoordinates: {x,y}, respawnTime}=
  if (isEmpty newEnemyPosition board) then
    enemy {enemyCoordinates = newEnemyPosition, lastEnemyDir = lDir, respawnTime = respawnTime - 1, wasKilled = false}
  else
    enemy {enemyCoordinates ={x,y}, respawnTime = respawnTime - 1, wasKilled = false}
  where
  isEmpty position board = Grid.index board position == Just Empty
  newEnemyPosition = { x: x + xd, y: y + yd }
  lDir = case {x: xd, y: yd} of
              { x: -1, y: 0 } -> Left
              { x: 1, y: 0 } -> Right
              { x: 0, y: 1 } -> Down
              { x: 0, y: -1 } -> Up
              {x:_,y:_} -> None


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

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: p@{ coordinates, playerHealth }, board } <- getW
  let x = coordinates.x
  let y = coordinates.y
  let newPlayerPosition = { x: x + xd, y: y + yd }
  let newTile = Grid.index board newPlayerPosition 
  case newTile of
    Just Empty -> updateW_ { player: p {coordinates = newPlayerPosition} }
    Just (Buff {type: Slow}) -> do
      let newBoard = Grid.updateAt' newPlayerPosition Empty board 
      updateW_ { board: newBoard, player: p {coordinates = newPlayerPosition, buff = Slow, buffTimer = 0}}
      widget "label_currentBuff" $ Label {content: show "Slow"}    

    Just (Buff {type: Immortal}) -> do
      let newBoard = Grid.updateAt' newPlayerPosition Empty board 
      updateW_ { board: newBoard, player: p {coordinates = newPlayerPosition, buff = Immortal, buffTimer = 0}}
      widget "label_currentBuff" $ Label {content: show "Immortal"}

    Just (Buff {type: Heal}) -> do
      let newHP = playerHealth + 25
      let newBoard = Grid.updateAt' newPlayerPosition Empty board 
      updateW_ { board: newBoard, player: p {coordinates = newPlayerPosition, 
        playerHealth = if newHP > playerMaxHP then playerMaxHP else newHP, buff = Default, buffTimer = 0}}
      widget "label_currentBuff" $ Label {content: show "Nothing"}

    Just (Buff {type: Power}) -> do
      let newBoard = Grid.updateAt' newPlayerPosition Empty board 
      updateW_ { board: newBoard, player: p {coordinates = newPlayerPosition, buff = Power, buffTimer = 0}}
      widget "label_currentBuff" $ Label {content: show "Power"}

    _ -> executeDefaultBehavior




placeBomb :: Coordinates -> Grid Tile -> Owner -> Grid Tile
placeBomb cords board ownr = do
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
        Bomb {time: 0, owner} -> Explosion{time: 30, owner: owner}
        Bomb {time, owner} -> Bomb{time: time - 1, owner}
        Explosion {time: 0} -> Empty
        Explosion {time, owner} -> Explosion{time: time - 1, owner}
        _ -> a
        
explosionCheck :: Array (Tuple Coordinates Tile) -> Int -> Grid Tile -> Buff -> Grid Tile
explosionCheck grid i board buff = do
  let x = (fromMaybe (Tuple {x:0,y:0} Empty) (Array.index grid i))
  if i > (height*width) then
    board
  else if (snd x == Explosion{time: 30, owner: Player}) then
    explode (fst x) Player buff
  else if (snd x == Explosion{time: 30, owner: AI}) then
    explode (fst x) AI buff
  else
    explosionCheck grid (i+1) board buff  

  where 
  explode :: Coordinates -> Owner -> Buff -> Grid Tile
  explode {x,y} ownr buff = do

    let a = megaExplode {x,y} 1 board Right ownr buff
    let b = megaExplode {x,y} 1 a Down ownr buff
    let c = megaExplode {x,y} 1 b Left ownr buff
    let d = megaExplode {x,y} 1 c Up ownr buff
    Grid.updateAt' {x: x, y: y} (Explosion{time: 29, owner: ownr}) d

  megaExplode :: Coordinates -> Int -> Grid Tile -> Direction -> Owner -> Buff -> Grid Tile
  megaExplode {x,y} r grid dir ownr buff = do
    let newCords = 
              case dir of
                Right -> {x: x+r, y}
                Left -> {x: x-r, y}
                Up -> {x, y: y-r}
                Down -> {x, y: y+r}
                _ -> {x,y}  
    let buffR = if buff == Power then r-1 else r
    case (fromMaybe Empty (Grid.index grid newCords)) of
      Wall -> grid
      Box -> if buff == Power then
        if buffR > 2 then
            Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid
          else
            megaExplode {x,y} (r+1) (Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid) dir ownr buff 
        else
          Grid.updateAt' newCords (generateRandomBuff ownr) grid
      Bomb{} -> Grid.updateAt' newCords (Explosion{time: 31, owner: ownr}) grid
      _ ->
          if buffR > 2 then
            Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid
          else
            megaExplode {x,y} (r+1) (Grid.updateAt' newCords (Explosion{time: 29, owner: ownr}) grid) dir ownr buff
  
  generateRandomBuff :: Owner -> Tile
  generateRandomBuff ownr = 
    let
      --Neither here :(
      h = unsafePerformEffect (randomInt 1 5)
    in
      case h of
        1 -> Buff{type: Slow}
        2 -> Buff {type: Immortal}
        3 -> Buff {type: Heal}
        4 -> Buff {type: Power}
        _ -> Explosion{time: 29, owner: ownr}
  






flipIt :: forall a m. Monad m => List (m a) -> m (List a)
flipIt Nil = pure Nil
flipIt (Cons f r) = do
  x <- f
  y <- flipIt r
  pure $ Cons x y

to2D :: Int -> Coordinates
to2D i = { x: i `mod` width, y: i / width }

constructM :: forall a. (Coordinates -> Effect a) -> Effect (Grid a )
constructM f = do
  (tiles :: Array a) <- tilesM
  pure (Grid tiles { width, height })
  where
  tilesM = map (Array.fromFoldable) (flipIt (List.fromFoldable xs)) 
  xs = map (f <<< to2D ) $0 .. (width * height)