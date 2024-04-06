module IveRearAdapter (run) where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

run :: IO ()
run = do
  translate (0, 51, 0) catchLeft & pure & render & writeFile "catchLeft.scad"
  translate (0, 51, 0) catchRight & pure & render & writeFile "catchRight.scad"
  casterSide & pure & render & writeFile "casterside.scad"

catchCover :: Model3d
catchCover =
  let outer =
        linearExtrudeDefault 23 (polygon 3 [[(0, 0), (5, 33), (37, 38), (43, 25), (23, 0)]])
      inner =
        linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 30), (35, 35), (40, 25), (20, 0)]])
   in outer
        `difference` (boltHoleM11 & translate (25.5, 25.5, 10))
        `difference` inner
        & rotate3d (5, -90, 0)
        & translate (23, 0, 0)

catchRight :: Model3d
catchRight =
  (box 40 6 20 & translate (5, 0, 0))
    `mappend` ( linearExtrudeDefault 8 (polygon 3 [[(0, 0), (-20, 20), (-15, 30), (35, 35), (40, 25), (20, 0)]])
                  & rotate3d (5, -90, 0)
                  & translate (8, 0, 0)
              )
    `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
    `difference` (boltHoleM5 & rotate3d (90, 0, 0) & translate (28, 10, 10))
    `difference` catcher
    `difference` (polygon 3 [[(-10, -10), (0, 12), (7, 0)]] & linearExtrudeDefault 50 & translate (-1, 0, -20))
    `difference` (screwHole M10 20 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))

catchLeft :: Model3d
catchLeft =
  mirror
    (1, 0, 0)
    ( (box 40 6 20 & translate (5, -3, 0))
        `mappend` ( linearExtrudeDefault 8 (polygon 3 [[(0, 0), (-20, 20), (-15, 30), (35, 35), (40, 25), (20, 0)]])
                      & rotate3d (5, -90, 0)
                      & translate (8, 0, 0)
                  )
        `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
        `difference` (boltHoleM5 & rotate3d (90, 0, 0) & translate (28, 10, 10))
        `difference` catcher
        `difference` (polygon 3 [[(-10, -10), (0, 12), (7, 0)]] & linearExtrudeDefault 50 & translate (-1, 0, -20))
    )
    `difference` (screwHole M10 20 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))

boltHoleM5 :: Model3d
boltHoleM5 = cylinder (5.3 / 2) 20 def

boltHoleM11 :: Model3d
boltHoleM11 = cylinder (11.5 / 2) 20 def

catcher :: Model3d
catcher =
  hull
    [ boltHoleM11 & rotate3d (0, 90, 0) & translate (-10, 25, -5),
      box 3 2 8 & translate (-2, 39, -5 - 11.1 / 2)
    ]

casterSide :: Model3d
casterSide =
  cylinder (11.1 / 2) 26 def
    `mappend` cylinder (13.1 / 2) 6 def
    `mappend` (cylinder 15 3 def & translate (0, 0, 1))
    `difference` (cylinder (8.2 / 2) 50 def & translate (0, 0, -1))
