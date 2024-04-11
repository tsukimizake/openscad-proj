module HexCup (run) where

import Data.Function ((&))
import Debug.Trace
import HoneyCombWall
import OpenSCAD

sample :: OpenSCADM Model3d
sample = do
  wall <-
    honeyCombWall 10 (100, 30, 4)
      & fmap
        ( rotate3d (0, -90, 0)
        )

  floor <- honeyCombWallHex 10 30 4
  pure $
    union
      [ wall & translate (4, 0, 0) & rotate3d (0, 0, 60),
        wall & translate (8, 0, 0) & translate (-30, 15, 0),
        wall & rotate3d (0, 0, -60),
        wall & translate (-4, 0, 0) & translate (-30, 15, 0) & translate (60, 0, 0),
        wall & translate (4, 0, 0) & rotate3d (0, 0, 240) & translate (0, 60, 0),
        wall & rotate3d (0, 0, 120) & translate (0, 60, 0),
        floor
      ]

run :: IO ()
run = render sample & writeFile "product.scad"
