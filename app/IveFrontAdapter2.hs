module IveFrontAdapter2 (obj, run) where

import Data.Function ((&))
import IveFrontCaster2 (pinHole)
import OpenSCAD as OS
import Sketch
import SketchTH
import SketchTypes
import Prelude

-- botLR = 85.2
-- topLR = 82.1
-- botZ = 4.73
-- topZ = 4.23
-- winW = 61
-- leverWinW = 22

data ZRecord = ZRecord
  { outerhull :: Polygon,
    inner :: Polygon,
    adapterWindow :: Polygon,
    stopperHook :: Polygon,
    center :: Point
  }

mkSketchRes ''ZRecord

data XRecord = XRecord
  { innerSide :: Polygon,
    adapter :: Polygon,
    base :: Polygon,
    necomimizone :: Polygon,
    cutForPrinting :: Polygon,
    pinPoint :: Point
  }

mkSketchRes ''XRecord

data YRecord = YRecord
  { upperLeverWindow :: Polygon,
    necomimil :: Polygon,
    necomimir :: Polygon,
    necoscrewl :: Point,
    necoscrewr :: Point
  }

mkSketchRes ''YRecord

data YRecordTilt = YRecordTilt
  { center :: Point,
    base :: Polygon,
    centerhook :: Polygon,
    centerhooka :: Point,
    centerhookc :: Point
  }

mkSketchRes ''YRecordTilt

obj :: OpenSCADM Model3d
obj =
  do
    let innerHeight = 60
    let outerThickness = 10

    -- Z
    let zrec = sketch do
          -- outer hull
          (outa, outb, outc, outd) <- rectSketch (point & x 0 & y 0) (\_ -> point & x 100 & y 75)
          outerhull <- poly [outa, outb, outc, outd]
          ac <- line & between outa outc
          bd <- line & between outb outd
          center <- intersectionPoint ac bd

          -- adapter inner
          innera <- point & relx center (-(85.0 / 2)) & y 0
          innerb <- point & relx center (85.0 / 2) & y 0
          innerc <- point & relx center (82.0 / 2) & y innerHeight
          innerd <- point & relx center (-(82.0 / 2)) & y innerHeight
          inner <- poly [innera, innerb, innerc, innerd]

          -- adapter window
          (windowa, windowb, windowc, windowd) <-
            rectSketch (point & relx center (-30.5) & y 0) (\a -> point & relx a 61 & rely innerc 0)
          adapterWindow <- poly [windowa, windowb, windowc, windowd]

          -- stopper hook
          (stoppera, stopperb, stopperc, stopperd) <-
            rectSketch (point & relx center (-11) & rely innerc (-16)) (\_ -> point & relx center 11 & rely innerc (-9))
          stopperHook <- poly [stoppera, stopperb, stopperc, stopperd]
          pure $ ZRecord {..}

    -- Y
    let yrec = sketch do
          --  upper lever window
          center <- point & x zrec.center.x & y 0
          (upperLeverWindowa, upperLeverWindowb, upperLeverWindowc, upperLeverWindowd) <-
            rectSketch
              (point & relx center (-11) & rely center (-3))
              (\a -> point & relx a 22 & rely a 6)
          upperLeverWindow <- poly [upperLeverWindowa, upperLeverWindowb, upperLeverWindowc, upperLeverWindowd]

          necomimila <- point & x 0 & y 10
          necomimilv1 <- line & from necomimila & degree 45
          necomimilb <- point & relx necomimila 15 >>= onLine necomimilv1 & chamfer 3
          necomimilc <- point & relx necomimila 30 & rely necomimila 0
          necomimilv2 <- line & between necomimilb necomimilc
          necoscrewl <- point & rely necomimila 10
          _ <- onLine necomimilv2 necoscrewl
          necomimil <- poly [necomimila, necomimilb, necomimilc]

          necomimira <- point & x 100 & y 10
          necomimirv1 <- line & from necomimira & degree 135
          necomimirb <- point & relx necomimira (-15) >>= onLine necomimirv1 & chamfer 3
          necomimirc <- point & relx necomimira (-30) & rely necomimira 0
          necomimirv2 <- line & between necomimirb necomimirc
          necoscrewr <- point & rely necomimira 10
          _ <- onLine necomimirv2 necoscrewr
          necomimir <- poly [necomimira, necomimirb, necomimirc]
          pure YRecord {..}

    let yrecTilt = sketch do
          center <- point & x zrec.center.x & y 0
          -- centerhook
          centerhooka <- point & relx center (-19) & rely center 10
          centerhookb <- point & relx center 19 & rely centerhooka 0
          centerhookc <- point & relx centerhookb (-1) & rely centerhooka 20
          centerhookd <- point & relx centerhooka 1 & rely centerhookc 0
          centerhook <- poly [centerhooka, centerhookb, centerhookc, centerhookd]

          -- base
          based <- point & relx centerhooka 0 & rely centerhooka 0
          basevl <- line & from based & degree 60
          basec <- point & relx centerhookb 0 & rely centerhookb 0
          basevr <- line & from basec & degree 120
          basea <- point & y (-10) >>= onLine basevl
          baseb <- point & y (-10) >>= onLine basevr
          base <- poly [basea, baseb, basec, based]
          pure YRecordTilt {..}

    -- X
    let xrec = sketch do
          -- inner side
          innera <- point & x 0 & y 2.5
          innerb <- point & relx innera 0 & rely innera 4.93
          innerc <- point & relx innera innerHeight & rely innera 4.43
          innerd <- point & relx innerc 0 & rely innera 0
          innerSide <- poly [innera, innerb, innerc, innerd]
          center <- point & x zrec.center.y & y 0

          -- adapter to casterside
          let baseHeight = yrecTilt.centerhooka.y
          let adapterHeight = yrecTilt.centerhookc.y
          basea <- point & relx center (-7.8) & rely center (-20)
          baseb <- point & relx basea 15.6 & rely basea 0
          adaptera <- point & relx basea 0 & rely center baseHeight
          adapterb <- point & relx baseb 0 & rely adaptera 0
          adapterc <- point & relx adapterb (-0.8) & rely adapterb adapterHeight
          adapterd <- point & relx adaptera 0.8 & rely adapterc 0
          base <- poly [basea, baseb, adapterb, adaptera]
          adapter <- poly [adaptera, adapterb, adapterc, adapterd]

          (necomimizonea, necomimizoneb, necomimizonec, necomimizoned) <-
            rectSketch (point & relx center (-30) & y 0) (\a -> point & relx a 60 & rely a 100)
          necomimizone <- poly [necomimizonea, necomimizoneb, necomimizonec, necomimizoned]
          (cutForPrintinga, cutForPrintingb, cutForPrintingc, cutForPrintingd) <-
            rectSketch (point & x 0 & y 0) (\a -> point & x 75 & rely a 300)
          cutForPrintingb' <- pure cutForPrintingb & chamfer 10
          cutForPrinting <- poly [cutForPrintinga, cutForPrintingb', cutForPrintingc, cutForPrintingd]
          pinPoint <- point & relx center 0 & y (baseHeight + 10)
          pure XRecord {..}

    zrec.outerhull
      & sketchExtrude 0 outerThickness OnZAxis
      & mappend
        ( union [yrecTilt.centerhook, yrecTilt.base]
            & sketchExtrude 0 60 OnYAxis
            & with intersection (union [xrec.base, xrec.adapter] & sketchExtrude 0 100 OnXAxis)
            -- 6.32で実測6.27
            & diff (pinHole 6.35 100 & rotate3d (0, 90, 0) & translate (expandVector OnXAxis xrec.pinPoint))
            & withOrigin (expandVector OnYAxis yrecTilt.center) (rotate3d (0, -10, 0))
            & translate (0, 0, 12)
        )
      & diff (yrec.upperLeverWindow & sketchExtrude 30 100 OnYAxis)
      & diff (zrec.adapterWindow & sketchExtrude 0 4 OnZAxis)
      & diff (intersection [zrec.inner & sketchExtrude 0 12 OnZAxis, xrec.innerSide & sketchExtrude 0 100 OnXAxis])
      & diff (zrec.stopperHook & sketchExtrude 0 9 OnZAxis)
      & mappend
        ( (union [yrec.necomimil, yrec.necomimir] & sketchExtrude 0 75 OnYAxis)
            & diff (xrec.necomimizone & sketchExtrude 0 100 OnXAxis)
        )
      & diff (screwHole M4 16 True & rotate3d (0, 45, 0) & translate (expandVector OnYAxis yrec.necoscrewl) & translate (0, 4, 0))
      & diff (screwHole M4 16 True & rotate3d (0, 45, 0) & translate (expandVector OnYAxis yrec.necoscrewl) & translate (0, 71.5, 0))
      & diff (screwHole M4 16 True & rotate3d (0, -45, 0) & translate (expandVector OnYAxis yrec.necoscrewr) & translate (0, 4, 0))
      & diff (screwHole M4 16 True & rotate3d (0, -45, 0) & translate (expandVector OnYAxis yrec.necoscrewr) & translate (0, 71.5, 0))
      & with intersection (xrec.cutForPrinting & sketchExtrude 0 300 OnXAxis)
      & pure

run :: IO ()
run =
  obj & render & writeFile "IveFrontAdapter2.scad"
