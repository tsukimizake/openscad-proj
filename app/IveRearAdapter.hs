module IveRearAdapter (run) where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

run :: IO ()
run = do
  translate (0, 51, 0) catchLeft & pure & render & writeFile "catchLeft.scad"
  translate (0, 51, 0) catchRight & pure & render & writeFile "catchRight.scad"
  casterSide & pure & render & writeFile "casterside.scad"

catchRight :: Model3d
catchRight =
  (box 40 6 20 & translate (5, 0, 0))
    `mappend` ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 50), (35, 50), (35, 25), (20, 0)]])
                  & rotate3d (5, -90, 0)
                  & translate (20, 0, 0)
              )
    `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
    `difference` (boltHoleM5 & rotate3d (90, 0, 0) & translate (28, 10, 10))
    `difference` (polygon 3 [[(-10, -10), (0, 12), (3, -3)]] & linearExtrudeDefault 50 & translate (-1, 0, -20))
    `difference` (screwHole M10 40 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))
    `difference` catcher

catchLeft :: Model3d
catchLeft =
  mirror
    (1, 0, 0)
    ( (box 40 6 20 & translate (5, -3, 0))
        `mappend` ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 50), (35, 50), (35, 25), (20, 0)]])
                      & rotate3d (5, -90, 0)
                      & translate (20, 0, 0)
                  )
        `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
        `difference` (boltHoleM5 & rotate3d (90, 0, 0) & translate (28, 10, 10))
        `difference` catcher
        `difference` (polygon 3 [[(-10, -10), (0, 12), (7, 0)]] & linearExtrudeDefault 50 & translate (-1, 0, -20))
    )
    `difference` (screwHole M10 40 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))

boltHoleM5 :: Model3d
boltHoleM5 = cylinder (5.3 / 2) 20 def

boltHoleM11 :: Model3d
boltHoleM11 = cylinder (11.5 / 2) 20 def

catcher :: Model3d
catcher =
  polygon 3 [[(0, 0), (0, 6), (-2, 6), (0, 19), (15, 19), (17, 6), (15, 6), (15, 0)]]
    & linearExtrudeDefault 10
    & rotate3d (5, -90, 180)
    & translate (0, 51, 10)

casterSide :: Model3d
casterSide =
  polygon 3 [[(0, -3), (0, 6), (-1, 6), (0, 19), (3, 19), (3, 0), (12, 0), (12, 19), (15, 19), (16, 6), (15, 6), (15, -3)]]
    & linearExtrudeDefault 10
