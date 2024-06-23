module IveFrontCaster2 (obj, run) where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

run :: IO ()
run = do
  translate (0, 51, 0) obj & pure & render & writeFile "IveFrontCaster2.scad"

-- `difference`

obj :: Model3d
obj =
  ( ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 25), (46, 25), (46, 0)]])
        & rotate3d (0, -90, 0)
        & translate (10, 0, 0)
    )
      `mappend` (triangle & translate (0, 0, 23))
      `mappend` (triangle & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, 7, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, 7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, -7, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, -7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (box 180 3 50 & translate (-90, 0, 0))
      & with minkowski (sphere 0.5 def)
  )
    `difference` catcher
    `difference` pinHole
    `difference` (screwHoles & translate (83, -4, 8))
    `difference` (screwHoles & translate (-70, -4, 8))

triangle :: Model3d
triangle = polygon 3 [[(0, 0), (0, 22), (90, 0)]] & linearExtrudeDefault 4 & translate (0, 2, 0)

catcher :: Model3d
catcher =
  polygon 3 [[(0, 0), (1, 19), (12, 19), (13, 0)]]
    & linearExtrudeDefault 35
    & rotate3d (0, 0, 180)
    & translate (6.5, 26, 5)

pinHole :: Model3d
pinHole =
  cylinder 50 (7 / 2) def & translate (0, 16, 0)

screwHoles :: Model3d
screwHoles =
  let hole = screwHole M5 15 True & rotate3d (90, 0, 0)
   in union
        [ hole & translate (0, 10, 0),
          hole & translate (15, 10, 0),
          hole & translate (15, 10, 38),
          hole & translate (0, 10, 38)
        ]
        & translate (-14, 0, -2)
