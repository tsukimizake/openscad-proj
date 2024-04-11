module HexCup (run) where

import Data.Function ((&))
import Debug.Trace
import HoneyCombWall
import OpenSCAD

sample :: OpenSCADM Model3d
sample = honeyCombWallHex 10 50 5

run :: IO ()
run = render sample & writeFile "product.scad"
