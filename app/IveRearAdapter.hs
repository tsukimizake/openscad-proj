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
    `mappend` ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 50), (46, 50), (46, 25), (20, 0)]])
                  & rotate3d (0, -90, 0)
                  & translate (16, 0, 0)
              )
    `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
    `difference` (boltHoleM5 20 & rotate3d (90, 0, 0) & translate (25.5, 10, 10))
    `difference` (polygon 3 [[(-10, -1), (-5, 22), (3, -1)]] & linearExtrudeDefault 60 & translate (-1, 0, -10))
    `difference` (screwHole M10 40 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))
    `difference` catcher
    `difference` pinHole

-- `difference`

catchLeft :: Model3d
catchLeft =
  mirror
    (1, 0, 0)
    ( (box 40 6 20 & translate (5, -3, 0))
        `mappend` ( linearExtrudeDefault 20 (polygon 3 [[(0, 0), (0, 50), (46, 50), (46, 25), (20, 0)]])
                      & rotate3d (0, -90, 0)
                      & translate (16, 0, 0)
                  )
        `mappend` (polygon 3 [[(0, 0), (0, 22), (15, 0)]] & linearExtrudeDefault 20)
        `difference` (boltHoleM5 20 & rotate3d (90, 0, 0) & translate (25.5, 10, 10))
        `difference` catcher
        `difference` pinHole
        `difference` (polygon 3 [[(-10, -1), (-5, 22), (3, -1)]] & linearExtrudeDefault 90 & translate (-1, 0, -20))
    )
    `difference` (screwHole M10 40 True & rotate3d (0, 90, 0) & translate (0, 25.5, 25))

boltHoleM5 :: Double -> Model3d
boltHoleM5 len = cylinder (5.3 / 2) len def

boltHoleM11 :: Model3d
boltHoleM11 = cylinder (11.5 / 2) 20 def

catcher :: Model3d
catcher =
  polygon 3 [[(0, 0), (1, 19), (12, 19), (13, 0)]]
    & linearExtrudeDefault 35
    & rotate3d (0, 0, 180)
    & translate (12, 51, 5)

pinHole :: Model3d
pinHole =
  cylinder (7 / 2) 46 def & translate (5.5, 41, 0)

casterSide :: Model3d
casterSide =
  ( polygon 3 [[(0, 0), (1, 18.5), (12, 18.5), (13, 0)]]
      & linearExtrudeDefault 34.5
      & rotate3d (0, 0, 180)
  )
    `difference` ( hull
                     [ cylinder (7 / 2) 46 def & translate (0, 0, 0),
                       cylinder (7 / 2) 46 def & translate (0, -2, 0)
                     ]
                     & translate (-6.5, -10, 0)
                 )
    `mappend` ( let depth = -10
                 in polygon 3 [[(-5, 0), (18, 0), (18, depth), (-5, depth)]]
                      & linearExtrudeDefault 50
                      & rotate3d (0, 0, 180)
                      & translate (0, 0, -8)
              )
    `difference` ( let hole = screwHole M5 15 True & rotate3d (90, 0, 0)
                    in union
                         [ hole & translate (0, 10, 0),
                           hole & translate (15, 10, 0),
                           hole & translate (15, 10, 38),
                           hole & translate (0, 10, 38)
                         ]
                         & translate (-14, 0, -2)
                 )

-- holes
