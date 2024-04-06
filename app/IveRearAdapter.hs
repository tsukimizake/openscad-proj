module IveRearAdapter (run) where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

main :: IO ()
main = do
  translate (0, 51, 0) catch & OS.render & writeFile "catch.scad"
  casterSide & OS.render & writeFile "casterside.scad"

catchCover :: Model3d
catchCover =
  let outer =
        linearExtrudeDefault 23 (OS.polygon 3 [[(0, 0), (5, 33), (37, 38), (43, 25), (23, 0)]])
      inner =
        linearExtrudeDefault 20 (OS.polygon 3 [[(0, 0), (0, 30), (35, 35), (40, 25), (20, 0)]])
   in outer
        `difference` (boltHoleM11 & translate (25.5, 25.5, 10))
        `difference` inner
        & OS.rotate3d (5, -90, 0)
        & OS.translate (23, 0, 0)

catch :: OS.Model3d
catch =
  mirror
    (1, 0, 0)
    ( (OS.box 40 6 20 & translate (5, -3, 0))
        `mappend` ( linearExtrudeDefault 8 (OS.polygon 3 [[(0, 0), (-20, 20), (-15, 30), (35, 35), (40, 25), (20, 0)]])
                      & OS.rotate3d (5, -90, 0)
                      & OS.translate (8, 0, 0)
                  )
        `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
        `difference` (boltHoleM5 & OS.rotate3d (90, 0, 0) & OS.translate (28, 10, 10))
        `difference` catcher
        `difference` (polygon 3 [[(-10, -10), (0, 12), (7, 0)]] & linearExtrudeDefault 50 & OS.translate (-1, 0, -20))
    )
    `difference` (screwHole M10 20 True & OS.rotate3d (0, 90, 0) & OS.translate (0, 25.5, 25))

boltHoleM5 :: OS.Model3d
boltHoleM5 = OS.cylinder (5.3 / 2) 20 def

boltHoleM11 :: OS.Model3d
boltHoleM11 = OS.cylinder (11.5 / 2) 20 def

catcher :: OS.Model3d
catcher =
  hull
    [ boltHoleM11 & OS.rotate3d (0, 90, 0) & OS.translate (-10, 25, -5),
      OS.box 3 2 8 & OS.translate (-2, 39, -5 - 11.1 / 2)
    ]

casterSide :: OS.Model3d
casterSide =
  cylinder (11.1 / 2) 26 def
    `mappend` cylinder (13.1 / 2) 6 def
    `mappend` (cylinder 15 3 def & translate (0, 0, 1))
    `difference` (cylinder (8.2 / 2) 50 def & OS.translate (0, 0, -1))

linearExtrudeDefault :: Double -> OS.Model2d -> OS.Model3d
linearExtrudeDefault height = OS.linearExtrude height 0 (1, 1) 1 3 def
