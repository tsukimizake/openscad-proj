{-# HLINT ignore "Use first" #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch
  ( Sketch,
    wrapShape,
    sketchTuple,
    sketchRecord,
    sketch,
    point,
    x,
    y,
    chamfer,
    line,
    onLine,
    from,
    degree,
    between,
    intersectionPoint,
    poly,
    sketchPoly,
    sketchPolys,
    rely,
    relx,
    onYAxis,
    onXAxis,
    wideLine,
    rectSketch,
    sketchExtrude,
    ExtrudeAxis (..),
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer (runWriter, tell)
import qualified Data.Bifunctor
import Data.Function ((&))
import qualified Data.List as List
import OpenSCAD (Model2d, Model3d, Vector2d, errorAssert, linearExtrudeDefault, mirror, rotate3d, translate)
import SketchSolver (runSolver)
import SketchTypes
import Prelude hiding (id)
import qualified Prelude

--- SOLVER

sketchRecord :: (ModelsTH m) => SketchM m -> ResTH m
sketchRecord m =
  m
    & fmap toListTH
    & runState 0
    & fmap fst
    & runWriter
    & run
    & ( \((sks, proxy), cs) ->
          let polys = sks & List.concatMap \case Poly s -> [Poly s]; _ -> []
              points = sks & List.concatMap \case P s -> [s]; _ -> []
           in runSolver ((polys, points), cs)
                & encodeError
                & (\(models, vecs) -> (modelAndVecToResult (models, vecs), proxy))
                & fromListTH
      )

sketchTuple :: (Models models, Points points) => SketchM (models, points) -> (Res models, PointRes points)
sketchTuple m =
  m
    & fmap (Data.Bifunctor.bimap toList toPointList)
    & runState 0
    & fmap fst
    & runWriter
    & run
    & ( \((sks, pts), cs) ->
          runSolver ((List.map fst sks, List.map fst pts), cs)
            & encodeError
            & Data.Bifunctor.bimap
              (\skr -> fromList (zip (List.map ModelRes skr) (List.map snd sks)))
              (\ptr -> fromPointList (zip (List.map (uncurry PointRes) ptr) (List.map snd pts)))
      )

sketch :: SketchM ([Polygon], [Point]) -> ([Model2d], [Vector2d])
sketch m =
  m
    & fmap (\(polys, pts) -> (List.map wrapShape polys, pts))
    & runState 0
    & fmap fst
    & runWriter
    & run
    & runSolver
    & encodeError

encodeError :: Either SketchError ([Model2d], [Vector2d]) -> ([Model2d], [Vector2d])
encodeError = \case
  Right r -> r
  Left (Contradiction s) -> ([errorAssert s], [])
  Left (Unresolved s) -> ([errorAssert s], [])

sketchPolys :: SketchM [Polygon] -> [Model2d]
sketchPolys m =
  m
    & fmap (,[])
    & sketch
    & fst

sketchPoly :: SketchM Polygon -> Model2d
sketchPoly m =
  m
    & fmap (\apoly -> ([apoly], []))
    & sketch
    & \case
      ([r], []) -> r
      _ -> error "should not happen"

genId :: SketchM Id
genId = do
  i <- get
  put (i + 1)
  pure $ Id i

--- POINT

point :: SketchM Point
point = Point <$> genId <*> genId <*> pure 0

x :: Double -> SketchM Point -> SketchM Point
x val m = do
  (Point x_ y_ cham) <- m
  putExact x_ val
  pure $ Point x_ y_ cham

y :: Double -> SketchM Point -> SketchM Point
y val m = do
  (Point x_ y_ cham) <- m
  putExact y_ val
  pure $ Point x_ y_ cham

chamfer :: Double -> SketchM Point -> SketchM Point
chamfer val m = do
  (Point x_ y_ _) <- m
  pure $ Point x_ y_ val

relx :: Point -> Double -> SketchM Point -> SketchM Point
relx fromPoint distance m = do
  newPoint <- m
  putPlus newPoint.x fromPoint.x distance
  pure newPoint

rely :: Point -> Double -> SketchM Point -> SketchM Point
rely fromPoint distance m = do
  newPoint <- m
  putPlus newPoint.y fromPoint.y distance
  pure newPoint

--- LINE

line :: SketchM Line
line = do
  x_ <- genId
  y_ <- genId
  Line x_ y_ <$> genId

from :: Point -> SketchM Line -> SketchM Line
from (Point x_ y_ _) m = do
  l <- m
  putEq l.x x_
  putEq l.y y_
  pure l

degree :: Angle -> SketchM Line -> SketchM Line
degree val m = do
  l <- m
  putExact l.angle (floor val & (\(v :: Int) -> v `mod` 360) & fromIntegral)
  pure l

onLine :: Line -> Point -> SketchM Point
onLine l p = do
  tell [OnLine p l]
  pure p

between :: Point -> Point -> SketchM Line -> SketchM Line
between p1 p2 m = do
  l <- m
  _ <- onLine l p1
  _ <- onLine l p2
  pure l

wideLine :: Double -> Point -> Point -> SketchM Polygon
wideLine width f t = do
  a <- point
  b <- point
  c <- point
  d <- point
  tell [WideLine width (f, t) (a, b, c, d)]
  poly [a, b, c, d]

-- RECT

rectSketch :: Point -> Point -> SketchM (Point, Point)
rectSketch bottomLeft topRight = do
  bottom <- line & from bottomLeft & degree 0
  top <- line & from topRight & degree 0
  left <- line & from bottomLeft & degree 90
  right <- line & from topRight & degree 90
  b <- intersectionPoint bottom right
  d <- intersectionPoint top left
  pure (b, d)

--- POLYGON
poly :: [Point] -> SketchM Polygon
poly = pure . Polygon

--- INTERSECTION POINT

intersectionPoint :: Line -> Line -> SketchM Point
intersectionPoint l1 l2 = do
  p <- point
  tell [Intersection l1 l2 p]
  _ <- onLine l1 p
  _ <- onLine l2 p
  pure p

-- helpers
putExact :: Id -> Double -> SketchM ()
putExact id v = tell [Exact id v]

putEq :: Id -> Id -> SketchM ()
putEq id1 id2 = tell [Eq id1 id2]

putPlus :: Id -> Id -> Double -> SketchM ()
putPlus idl idr distance = do
  tell [Plus idl idr distance]

-- utils for user
onYAxis :: Model3d -> Model3d
onYAxis m = m & rotate3d (90, 0, 0) & mirror (0, 1, 0)

onXAxis :: Model3d -> Model3d
onXAxis m = m & rotate3d (0, -90, 0) & mirror (1, 0, 0)

data ExtrudeAxis = OnXAxis | OnYAxis | OnZAxis
  deriving (Show, Eq)

sketchExtrude :: Double -> Double -> ExtrudeAxis -> Model2d -> Model3d
sketchExtrude bottom top axis model =
  model
    & OpenSCAD.linearExtrudeDefault (top + bottom)
    & OpenSCAD.translate (0, 0, bottom)
    & case axis of
      OnXAxis -> onXAxis
      OnYAxis -> onYAxis
      OnZAxis -> Prelude.id
