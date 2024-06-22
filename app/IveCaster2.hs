module IveCaster2 (obj, run) where

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
      outerScrewHole = (screwHole M4 15 True & translate (0, 0, 17))
   in adapterHull
        `difference` inner
        `difference` (inner & mirror (1, 0, 0))
        `difference` (outerScrewHole & rotate3d (0, -45, 0) & translate (45, 5, 15))
        `difference` (outerScrewHole & rotate3d (0, 45, 0) & translate (-45, 5, 15))
        `difference` (outerScrewHole & rotate3d (0, -45, 0) & translate (45, 55, 15))
        `difference` (outerScrewHole & rotate3d (0, 45, 0) & translate (-45, 55, 15))
        `mappend` (casterSide & rotate3d (-90, 0, 0) & translate (6, 10, 15))

adapterHull :: Model3d
adapterHull =
  minkowski
    [ ( union
          [ polygon 3 [[(0, 12), (94, 12), (94, 0), (0, 0)]],
            polygon 3 [[(0, 12), (10, 25), (30, 12)]],
            polygon 3 [[(64, 12), (84, 25), (94, 12)]]
          ]
          & linearExtrudeDefault 61
          & rotate3d (90, 0, 0)
          & translate (0, 61, 0)
      )
        `difference` (box 100 40 30 & translate (0, 10, 12)),
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

casterSide :: Model3d
casterSide =
  ( polygon 3 [[(0, 0), (1, 18.5), (12, 18.5), (13, 0)]]
      & linearExtrudeDefault 34.5
      & rotate3d (0, 0, 180)
  )
    `difference` ( hull
                     [ cylinder 46 (7 / 2) def & translate (0, 0, 0),
                       cylinder 46 (7 / 2) def & translate (0, -2, 0)
                     ]
                     & translate (-6.5, -10, 0)
                 )

run :: IO ()
run =
  do
    pure obj & render & writeFile "ivefronteadapter2.scad"
