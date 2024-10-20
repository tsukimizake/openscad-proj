module Main (main) where

import AirClothCap
import AsmIveFront
import AsmIveRear
import AsmIveRear3
import HexCup
import HonexCombTest
import IronBrushCase
import IveCaster
import IveFrontAdapter2
import IveFrontCaster2
import IveRearAdapter
import IveRearAdapter2
import IveRearAdapter3
import IveRearCaster2
import IveRearCaster3
import KitchenRack
import LGuard
import PaniaGuard
import SketchTH
import SketchTry
import Washer

main :: IO ()
main = do
  IveFrontAdapter2.run
  IveFrontCaster2.run
  AsmIveFront.run
  IveRearAdapter3.run
  IveRearCaster3.run
  AsmIveRear3.run
