module HexCup (run) where

import Data.Function ((&))
import Debug.Trace
import HoneyCombWall
import OpenSCAD

sample :: OpenSCADM Model3d
sample = do
  let edgelength = 40
  wall <-
    honeyCombWall 10 (60, edgelength, 4)
      & fmap
        ( rotate3d (0, -90, 0)
        )

  floor <- honeyCombWallHex 10 edgelength 4
  pure $
    union
      [ wall & translate (4, 0, 0) & rotate3d (0, 0, 60),
        wall & translate (8.5, 0, 0) & translate (-edgelength, edgelength / 2, 0),
        wall & rotate3d (0, 0, -60),
        wall & translate (-5.5, 0, 0) & translate (-edgelength, edgelength / 2, 0) & translate (2 * edgelength, 0, 0),
        wall & translate (4, 0, 0) & rotate3d (0, 0, 240) & translate (0, 2 * edgelength, 0),
        wall & rotate3d (0, 0, 120) & translate (0, 2 * edgelength, 0),
        floor
      ]

run :: IO ()
run = render sample & writeFile "product.scad"
