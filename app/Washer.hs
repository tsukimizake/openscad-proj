module Washer (obj, run) where

import Data.Function ((&))
import OpenSCAD

obj :: OpenSCADM Model3d
obj =
  pure $
    cylinder 0.8 7.5 def
      `difference` cylinder 1 2.5 def

run :: IO ()
run =
  render obj & writeFile "Washer.scad"
