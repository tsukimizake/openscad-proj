{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch
  ( Sketch,
    wrapShape,
    sketch,
    point,
    x,
    y,
    chamfer,
    line,
    onLine,
    from,
    degree,
    intersectionPoint,
    poly,
    sketchPoly,
    sketchPolys,
    rely,
    relx,
    onYAxis,
    onXAxis,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer (runWriter, tell)
import Data.Function ((&))
import qualified Data.List as List
import OpenSCAD (Model2d, Model3d, errorAssert, mirror, rotate3d)
import SketchSolver (runSolver)
import SketchTypes
import Prelude hiding (id)

--- SOLVER

sketch :: SketchM ([Polygon], [Point]) -> ([Model2d], [Point])
sketch m =
  m
    & fmap (\(polys, pts) -> (List.map wrapShape polys, pts))
    & (runState 0)
    & fmap fst
    & runWriter
    & run
    & runSolver
    & encodeError

encodeError :: Either SketchError ([Model2d], [Point]) -> ([Model2d], [Point])
encodeError = \case
  Right r -> r
  Left (Contradiction s) -> ([errorAssert s], [])
  Left (Unresolved s) -> ([errorAssert s], [])

sketchPolys :: SketchM [Polygon] -> [Model2d]
sketchPolys m =
  m
    & fmap (\polys -> (polys, []))
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

onLine :: Point -> Line -> SketchM ()
onLine p l = tell [OnLine p l]

--- POLYGON
poly :: [Point] -> SketchM Polygon
poly = pure . Polygon

--- INTERSECTION POINT

intersectionPoint :: Line -> Line -> SketchM Point
intersectionPoint l1 l2 = do
  p <- point
  tell [Intersection l1 l2 p]
  onLine p l1
  onLine p l2
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
