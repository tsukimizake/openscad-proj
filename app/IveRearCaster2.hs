{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module IveRearCaster2 (obj, run) where

import Data.Function ((&))
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

obj :: OpenSCADM Model3d
obj = pure $ cylinder 1 10 def

run :: IO ()
run =
  render obj & writeFile "IveRearCaster2.scad"