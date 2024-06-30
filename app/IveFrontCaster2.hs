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
  ( ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 25), (51, 25), (51, 0)]])
        & rotate3d (0, -90, 0)
        & translate (10, 0, 0)
    )
      `mappend` (triangle & translate (0, 0, 23))
      `mappend` (triangle & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, 7, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, 7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, -7, 0) & translate (0, 0, 23))
      `mappend` (triangle & rotate3d (0, -7, 0) & mirror (1, 0, 0) & translate (0, 0, 23))
      `mappend` (frame & translate (-90, 0, 0))
      & with minkowski (sphere 0.5 def)
  )
    `difference` catcher
    `difference` pinHole
    `difference` (screwHoles & translate (83, -4, 8))
    `difference` (screwHoles & translate (-70, -4, 8))

frame :: Model3d
frame =
  box 180 3 50
    `mappend` box 5 4 50
    `mappend` (box 5 4 50 & translate (180 - 4, 0, 0))
    `mappend` box 180 4 5
    `mappend` (box 180 4 5 & translate (0, 0, 50 - 4))

triangle :: Model3d
triangle = polygon 3 [[(0, 0), (0, 22), (90, 0)]] & linearExtrudeDefault 4 & translate (0, 2, 0)

catcher :: Model3d
catcher =
  rectangle 13 35
    & linearExtrude 19 0 (0.95, 0.95) 10 3 def
    & rotate3d (90, 0, 0)
    & translate (-6.5, 26, 7.5)

pinHole :: Model3d
pinHole =
  cylinder 100 (7 / 2) def & translate (0, 16, -5)

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
