module LGuard (run) where

import Data.Function ((&))
import HoneyCombWall
import OpenSCAD

sample :: OpenSCADM Model3d
sample = do
  pure $
    union
      [ box 50 100 3,
        box 50 3 100,
        polygon 3 [[(0, 0), (15, 0), (0, 15)]]
          & linearExtrudeDefault 50
          & rotate3d (0, -90, 0)
          & translate (50, 0, 0)
      ]

run :: IO ()
run = do
  render sample & writeFile "product.scad"
