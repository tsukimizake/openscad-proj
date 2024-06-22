module IveFrontCaster2 (obj, run) where

import Data.Function ((&))
import OpenSCAD

obj :: OpenSCADM Model3d
obj = pure $ cylinder 1 10 def

run :: IO ()
run =
  render obj & writeFile "IveFrontCaster2.scad"
