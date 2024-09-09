module AsmIveRear where

import qualified Data.Colour as Color
import qualified Data.Colour.SRGB as Color
import Data.Function ((&))
import Data.Functor ((<&>))
import IveRearAdapter2
import IveRearCaster2
import OpenSCAD
import Sketch
import SketchTH
import SketchTypes

obj :: OpenSCADM Model3d
obj = do
  adapter :: Model3d <-
    IveRearAdapter2.obj
      >>= declModule

  caster :: Model3d <-
    IveRearCaster2.obj
      <&> withOrigin (20, 0, 0) (rotate3d (0, 0, 180))
      <&> translate (20, 0, -31)
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
  render AsmIveRear.obj & writeFile "AsmIveRear.scad"
