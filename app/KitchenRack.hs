module KitchenRack (run) where

import Data.Function ((&))
import HoneyCombWall
import OpenSCAD
import Prelude

sample :: OpenSCADM Model3d
sample = do
  wall <- honeyCombWall 10 (200, 75, 4)
  endWall <- fmap (translate (200, 0, 0) . rotate3d (0, -90, 0)) (honeyCombWall 10 (79, 75, 5))
  pure $
    wall
      & with union (wall & rotate3d (90, 0, 0) & translate (0, 4, 2))
      & with union (wall & rotate3d (90, 0, 0) & translate (0, 75, 2))
      & with union (wall & translate (0, 0, 75))
      & with union endWall

halfHeight :: OpenSCADM Model3d
halfHeight = do
  wall <- honeyCombWall 10 (100, 80, 4)
  endWall <- fmap (translate (100, 0, 0) . rotate3d (0, -90, 0)) (honeyCombWall 10 (79, 80, 5))
  pure $
    wall
      & with union (wall & rotate3d (90, 0, 0) & translate (0, 4, 2))
      & with union (wall & rotate3d (90, 0, 0) & translate (0, 80, 2))
      & with union (wall & translate (0, 0, 80))
      & with union endWall

adapter :: Model3d
adapter =
  let keel = box 1 12.5 7 & translate (4, 0, 0)
      frame = box 9 1 7 & translate (0, -0.5, 0)
      hookbase = box 1 3 7 & translate (-0.5, 0, 0)
      hook1 = hookbase & rotate3d (0, 0, 30) & translate (0.8, -1.5, 0)
      hook2 = hookbase & rotate3d (0, 0, -30) & translate (8.2, -1.5, 0)
      half =
        union
          [ keel,
            frame,
            hook1,
            hook2
          ]
   in half & with union (half & mirror (0, 1, 0) & translate (0, 12, 0))

run :: IO ()
run = do
  render sample & writeFile "product.scad"
  render halfHeight & writeFile "halfHeight.scad"
  render (pure adapter) & writeFile "adapter.scad"
