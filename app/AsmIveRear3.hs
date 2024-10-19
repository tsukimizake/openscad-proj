{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Functor law" #-}
module AsmIveRear3 (AsmIveRear3.obj, AsmIveRear3.run) where

import qualified Data.Colour as Color
import Data.Function ((&))
import Data.Functor ((<&>))
import IveRearAdapter3
import IveRearCaster3
import OpenSCAD

obj :: OpenSCADM Model3d
obj = do
  adapter :: Model3d <-
    IveRearAdapter3.obj
      >>= declModule

  caster :: Model3d <-
    IveRearCaster3.obj
      <&> withOrigin (20, 0, 0) (rotate3d (0, 0, 180))
      <&> translate (20, 0, 31)
      >>= declModule
  let inter = intersection [adapter, caster]
  pure $
    union
      [ inter & transparent (Color.withOpacity OpenSCAD.red 0.5),
        adapter & transparent (Color.withOpacity OpenSCAD.blue 0.3),
        caster & transparent (Color.withOpacity OpenSCAD.yellow 0.3)
      ]

run :: IO ()
run =
  render AsmIveRear3.obj & writeFile "AsmIveRear3.scad"
