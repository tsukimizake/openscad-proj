module IveFrontAdapter2 (obj, run) where

import Data.Function ((&))
import OpenSCAD as OS
import Sketch
import Prelude

-- botLR = 85.2
-- topLR = 82.1
-- botZ = 4.73
-- topZ = 4.23
-- winW = 61
-- leverWinW = 22

obj :: OpenSCADM Model3d
obj =
  do
    let innerHeight = 60
    let outerThickness = 10

    -- Z
    let ((outerhull, inner, adapterWindow, stopperHook), center) = sketchTuple do
          -- outer hull
          outa <- point & x 0 & y 0
          outc <- point & x 100 & y 65
          (outb, outd) <- rectSketch outa outc
          out' <- poly [outa, outb, outc, outd]
          ac <- line & between outa outc
          bd <- line & between outb outd
          center' <- intersectionPoint ac bd

          -- adapter inner
          innera <- point & relx center' (-(85.2 / 2)) & y 0
          innerb <- point & relx center' (85.2 / 2) & y 0
          innerc <- point & relx center' (82.1 / 2) & y innerHeight
          innerd <- point & relx center' (-(82.1 / 2)) & y innerHeight
          inner' <- poly [innera, innerb, innerc, innerd]

          -- adapter window
          windowa <- point & relx center' (-30.5) & y 0
          windowc <- point & relx windowa 61 & rely innerc 0
          (windowb, windowd) <- rectSketch windowa windowc
          window' <- poly [windowa, windowb, windowc, windowd]

          -- stopper hook
          stopperc <- point & relx center' 11 & rely innerc (-9)
          stoppera <- point & relx center' (-11) & rely stopperc (-7)
          (stopperb, stopperd) <- rectSketch stoppera stopperc
          stopperHook' <- poly [stoppera, stopperb, stopperc, stopperd]
          pure ((out', inner', window', stopperHook'), center')

    -- X
    let ((innerSide, adapterx, hookx), ()) = sketchTuple do
          -- inner side
          innera <- point & x 2.5 & y 0
          innerb <- point & relx innera 4.93 & rely innera 0
          innerc <- point & relx innera 4.43 & rely innera innerHeight
          innerd <- point & relx innera 0 & rely innerc 0
          innerSide' <- poly [innera, innerb, innerc, innerd]
          center' <- point & x 0 & y 33

          -- adapter to casterside
          adaptera <- point & relx center' outerThickness & rely center' (-17.25)
          adapterb <- point & relx adaptera 20 & rely adaptera 0.5
          adapterd <- point & relx adaptera 0 & rely adaptera 34.5
          adapterc <- point & relx adaptera 20 & rely adapterd (-0.5)
          adapterab <- line & between adaptera adapterb
          adaptercd <- line & between adapterc adapterd
          adapterhead <- intersectionPoint adapterab adaptercd
          adapter <- poly [adaptera, adapterhead, adapterd]

          -- hook for casterside adapter
          hooka <- point & relx center' 0 & rely center' (-6.3)
          hookb <- point & relx hooka 100 & rely hooka 0
          hookc <- point & relx hookb 100 & rely hookb 12.6
          hookd <- point & relx hooka 0 & rely hookc 0
          hookx' <- poly [hooka, hookb, hookc, hookd]
          pure ((innerSide', adapter, hookx'), ())

    -- Y
    let ((upperLeverWindow, adaptery, hook, divider, enfol, enfor), ()) = sketchTuple do
          --  upper lever window
          center' <- point & x center.x & y 0
          upperLeverWindowa <- point & relx center' (-11) & rely center' (-3)
          upperLeverWindowc <- point & relx upperLeverWindowa 22 & rely upperLeverWindowa 6
          (upperLeverWindowb, upperLeverWindowd) <- rectSketch upperLeverWindowa upperLeverWindowc
          upperLeverWindow' <- poly [upperLeverWindowa, upperLeverWindowb, upperLeverWindowc, upperLeverWindowd]

          -- adapter to caster side
          adaptera <- point & relx center' (-7) & rely center' outerThickness
          adapterb <- point & relx center' 7 & rely adaptera 0
          adapterc <- point & relx center' 6.5 & rely adapterb 20
          adapterd <- point & relx center' (-6.5) & rely adapterc 0
          adapter' <- poly [adaptera, adapterb, adapterc, adapterd]

          -- hook on the adapter
          chook <- point & relx adapterc 0.8 & rely adapterc 0 & chamfer 0.4
          chead <- point & relx adapterc 0 & rely adapterc 7 & chamfer 0.3
          dhead <- point & relx adapterd 0 & rely adapterd 7 & chamfer 0.3
          dhook <- point & relx adapterd (-0.8) & rely adapterd 0 & chamfer 0.4
          hook' <- poly [chook, chead, dhead, dhook]

          -- adapter divider
          dividera <- point & relx adaptera 2.5 & rely adaptera 0 & chamfer 4
          dividerb <- point & relx adapterb (-2.5) & rely adapterb 0 & chamfer 4
          dividerc <- point & relx adapterc (-2.5) & rely adapterc 0
          dividerd <- point & relx adapterd 2.5 & rely adapterd 0
          dividerad <- line & between dividera dividerd
          dividerbc <- line & between dividerb dividerc
          dividerhead <- intersectionPoint dividerad dividerbc
          divider' <- poly [dividera, dividerhead, dividerb]

          -- enforcer
          enfolbot <- point & relx dividerd (-1) & rely adapterd (-4)
          enfolmid <- point & relx dividerd 4 & rely adapterd 0 & chamfer 2
          enfoltop <- point & relx dividerd 0 & rely adapterd 4
          enfol' <- poly [enfolbot, enfolmid, enfoltop]

          enforbot <- point & relx dividerc 1 & rely enfolbot 0
          enformid <- point & relx dividerc (-4) & rely enfolmid 0 & chamfer 2
          enfortop <- point & relx dividerc 0 & rely enfoltop 0
          enfor' <- poly [enforbot, enformid, enfortop]
          pure ((upperLeverWindow', adapter', hook', divider', enfol', enfor'), ())

    outerhull
      & sketchExtrude 0 outerThickness OnZAxis
      & diff (upperLeverWindow & sketchExtrude 30 100 OnYAxis)
      & diff (adapterWindow & sketchExtrude 0 4 OnZAxis)
      & diff
        ( intersection
            [ inner & sketchExtrude 0 12 OnZAxis,
              innerSide & sketchExtrude 0 100 OnXAxis
            ]
        )
      & mappend
        ( intersection
            [ adaptery & sketchExtrude 0 100 OnYAxis,
              adapterx & sketchExtrude 0 200 OnXAxis
            ]
        )
      & mappend
        ( (hook & sketchExtrude 0 100 OnYAxis)
            & with intersection (hookx & sketchExtrude 0 200 OnXAxis)
        )
      & diff (divider & sketchExtrude 0 100 OnYAxis)
      & mappend
        ( enfol
            & mappend enfor
            & sketchExtrude 0 100 OnYAxis
            & with intersection (hookx & sketchExtrude 0 200 OnXAxis)
        )
      & diff (stopperHook & sketchExtrude 0 8.5 OnZAxis)
      & pure

run :: IO ()
run =
  obj & render & writeFile "ivefronteadapter2.scad"

-- 旧実装
-- adapterInner :: Model3d
-- adapterInner =
--   let topL = 0
--       topR = 41
--       botL = 0
--       botR = 42.6
--       botZ = 4.93
--       topZ = 4.43
--       v0 = (botL, 0, 0)
--       v1 = (botR, 0, 0)
--       v2 = (topR, 61, 0)
--       v3 = (topL, 61, 0)
--       v4 = (botL, 0, botZ)
--       v5 = (botR, 0, topZ)
--       v6 = (topR, 61, topZ)
--       v7 = (topL, 61, botZ)
--    in unsafePolyhedron
--         10
--         [v0, v1, v2, v3, v4, v5, v6, v7]
--         (Faces [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4], [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]])
--         & rotate3d (0, 0, 90)
--         & translate (71, -1, 5)
-- obj :: Model3d
-- obj =
--   let inner =
--         union
--           [ adapterInner,
--             adapterWindow,
--             upperLeverWindow,
--             hookReceiver
--           ]
--       outerScrewHole = (screwHole M4 15 True & translate (0, 0, 17))
--    in adapterHull
--         `difference` inner
--         `difference` (inner & mirror (1, 0, 0))
--         `difference` (outerScrewHole & rotate3d (0, -45, 0) & translate (45, 5, 15))
--         `difference` (outerScrewHole & rotate3d (0, 45, 0) & translate (-45, 5, 15))
--         `difference` (outerScrewHole & rotate3d (0, -45, 0) & translate (45, 55, 15))
--         `difference` (outerScrewHole & rotate3d (0, 45, 0) & translate (-45, 55, 15))
--         `mappend` (outerAdapter & rotate3d (0, 180, 0) & translate (6.5, 10, 35))
--
-- adapterHull :: Model3d
-- adapterHull =
--   minkowski
--     [ ( union
--           [ polygon 3 [[(0, 12), (94, 12), (94, 0), (0, 0)]],
--             polygon 3 [[(0, 12), (10, 25), (30, 12)]],
--             polygon 3 [[(64, 12), (84, 25), (94, 12)]]
--           ]
--           & linearExtrudeDefault 61
--           & rotate3d (90, 0, 0)
--           & translate (0, 61, 0)
--       )
--         `difference` (box 100 40 30 & translate (0, 10, 12)),
--       cylinder 1 1 def
--     ]
--     & translate (-47, 0, 2.5)
--
-- adapterWindow :: Model3d
-- adapterWindow =
--   box 31 61 12
--     & translate (0, -1, -3)
--
-- upperLeverWindow :: Model3d
-- upperLeverWindow =
--   box 11 61 7
--     & translate (0, 20, 0)
--
-- hookReceiver :: Model3d
-- hookReceiver =
--   box 11 7 13
--     & translate (0, 43.5, -1)
--
-- outerAdapter :: Model3d
-- outerAdapter =
--   let sock = sketchPoly do
--         leftBottom <- point & x 0 & y 0
--         mkadapter leftBottom
--    in extrudeadapter (6.5, 17.5) sock
--
-- --   ( rectangle 13 35
-- --       & linearExtrude 19 0 (0.95, 0.95) 10
-- --       & rotate3d (90, 0, 0)
-- --       & translate (-13, 0, 0)
-- --   )
-- --     `difference` (cylinder 46 4 def & translate (-6.5, -10, 0))
