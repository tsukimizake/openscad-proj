module TEMPLATE (obj, run) where

import Data.Function ((&))
import OpenSCAD

obj :: OpenSCADM Model3d
obj = pure $ cylinder 1 10 def

run :: IO ()
run =
  render obj & writeFile "TEMPLATE.scad"
