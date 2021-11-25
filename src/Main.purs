module Main where

import Prelude

import Data.Grid (Coordinates)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (Reactor, dimensions, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, fill, tile)
import Reactor.Internal.Helpers (withJust)
import Reactor.Reaction (Reaction)

main :: Effect Unit
main = runReactor reactor { title: "Moving Dot", width: 20, height: 20 }

type World = { player :: Coordinates, cursor :: Maybe Coordinates }

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: 0, y: 0 }, cursor: Nothing }

draw :: World -> Drawing
draw { cursor, player } = do
  fill Color.blue400 $ tile player
  withJust cursor $ \position ->
    fill Color.gray200 $ tile position

handleEvent :: Event -> Reaction World
handleEvent event = do
  { width, height } <- dimensions
  let
    clip a m = min (max 0 a) (m - 1)
    bound { x, y } = { x: clip x width, y: clip y height }
  { player: { x, y } } <- getW
  case event of
    Mouse { position } -> updateW_ { cursor: Just position }

    KeyPress { key: "ArrowLeft" } -> updateW_ { player: bound { x: x - 1, y } }
    KeyPress { key: "ArrowRight" } -> updateW_ { player: bound { x: x + 1, y } }
    KeyPress { key: "ArrowDown" } -> updateW_ { player: bound { x, y: y + 1 } }
    KeyPress { key: "ArrowUp" } -> updateW_ { player: bound { x, y: y - 1 } }

    _ -> executeDefaultBehavior
