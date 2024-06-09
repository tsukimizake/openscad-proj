module PaniaGuard (obj, run) where

import Data.Function ((&))
import OpenSCAD as OS
import PathExtrude
import Prelude

obj :: OpenSCADM Model3d
obj =
  pure $
    ( base
        & with union guardPlate
    )
      `difference` boltHole

base :: OS.Model3d
base = OS.box 18 6 13 & translate (-5, 0, -2)

guardPlate :: OS.Model3d
guardPlate =
  let triangle =
        [(0, 0), (10, 0), (70, 100), (0, 115), (0, 0)] & map (\(x, y) -> (x, y, 0))
   in pathExtrude triangle (cylinder 5 2 def)
        & rotate3d (0, -90, -75)
        & translate (13, 0, 0)

boltHole :: OS.Model3d
boltHole =
  OS.cylinder 20 2.8 def
    & rotate3d (90, 0, 0)
    & translate (5, 10, 5)

run :: IO ()
run = do
  obj & render & writeFile "PaniaGuard.scad"
