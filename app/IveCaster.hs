module IveCaster (run, obj) where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

-- botLR = 85.2
-- topLR = 82.1
-- botZ = 4.73
-- topZ = 4.23
-- winW = 61
-- levWinW = 22
obj :: Model3d
obj =
  let inner =
        union
          [ adapterInner,
            adapterWindow,
            upperLeverWindow,
            hookReceiver
          ]
      outerScrewHole = (screwHole M4 10 True & translate (0, 0, 17))
   in adapterHull
        `difference` inner
        `difference` (inner & mirror (1, 0, 0))
        `difference` (boltHoles & rotate3d (0, 5, 0))
        `difference` (boltHoles & mirror (1, 0, 0) & rotate3d (0, 5, 0))
        `difference` (outerScrewHole & translate (40, 5, 0))
        `difference` (outerScrewHole & translate (-40, 5, 5))
        `difference` (outerScrewHole & translate (40, 55, 0))
        `difference` (outerScrewHole & translate (-40, 55, 5))

adapterHull :: Model3d
adapterHull =
  minkowski
    [ polygon 3 [[(0, 0), (94, -8), (94, -20), (0, -20)]] & linearExtrudeDefault 61 & rotate3d (90, 0, 0) & translate (0, 61, 20),
      cylinder 1 1 def
    ]
    & translate (-47, 0, 2.5)

adapterInner :: Model3d
adapterInner =
  let topL = 0
      topR = 41
      botL = 0
      botR = 42.6
      botZ = 4.93
      topZ = 4.43
      v0 = (botL, 0, 0)
      v1 = (botR, 0, 0)
      v2 = (topR, 61, 0)
      v3 = (topL, 61, 0)
      v4 = (botL, 0, botZ)
      v5 = (botR, 0, botZ)
      v6 = (topR, 61, topZ)
      v7 = (topL, 61, topZ)
   in unsafePolyhedron
        10
        [v0, v1, v2, v3, v4, v5, v6, v7]
        (Faces [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4], [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]])
        & translate (0, -1, 5)

adapterWindow :: Model3d
adapterWindow =
  box 31 61 12
    & translate (0, -1, -3)

upperLeverWindow :: Model3d
upperLeverWindow =
  box 11 61 7
    & translate (0, 20, 0)

hookReceiver :: Model3d
hookReceiver =
  box 11 7 13
    & translate (0, 43.5, -1)

-- pitch: 36mm, 15mm 5M bolt
boltHoles :: Model3d
boltHoles =
  union
    [ cylinder 30 2.8 def,
      boltHeadHole & translate (0, 0, 0),
      cylinder 30 2.8 def & translate (0, 36, 0),
      boltHeadHole & translate (0, 36, 0)
    ]
    & translate (7.5, 19.3, 0)

boltHeadHole :: Model3d
boltHeadHole =
  cylinder 12 4.7 def

run :: IO ()
run =
  do
    pure obj
    & render
    & writeFile "product.scad"
