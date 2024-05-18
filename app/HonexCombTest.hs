module HonexCombTest (obj, run) where

import Data.Function ((&))
import HoneyCombWall
import OpenSCAD

obj :: OpenSCADM Model3d
obj = honeyCombWall 10 (106, 106, 2)

run :: IO ()
run =
  render obj & writeFile "HonexCombTest.scad"
