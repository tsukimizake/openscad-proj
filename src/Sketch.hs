{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sketch
  ( Sketch,
    wrapShape,
    sketch,
    point,
    x,
    y,
    line,
    onLine,
    from,
    degree,
    intersect,
    poly,
    putEq, -- exported for debug
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Function ((&))
import OpenSCAD (Model2d)
import SketchSolver (runSolver)
import SketchTypes
import Prelude hiding (id)

putExact :: Id -> Double -> Eff [State Id, Writer [Constraint]] ()
putExact id v = tell [Exact id v]

putEq :: Id -> Id -> Eff [State Id, Writer [Constraint]] ()
putEq id1 id2 = tell [Eq id1 id2]

--- SOLVER

sketch :: (Shape s) => SketchM s -> Either SketchError Model2d
sketch m =
  m
    & fmap wrapShape
    & (runState 0)
    & fmap fst
    & runWriter
    & run
    & runSolver

genId :: SketchM Id
genId = do
  i <- get
  put (i + 1)
  pure i

--- POINT

point :: SketchM Point
point = Point <$> genId <*> genId

x :: Double -> SketchM Point -> SketchM Point
x val m = do
  (Point x_ y_) <- m
  putExact x_ val
  pure $ Point x_ y_

y :: Double -> SketchM Point -> SketchM Point
y val m = do
  (Point x_ y_) <- m
  putExact y_ val
  pure $ Point x_ y_

--- LINE

line :: SketchM Line
line = do
  x_ <- genId
  y_ <- genId
  Line x_ y_ <$> genId

from :: Point -> SketchM Line -> SketchM Line
from (Point x_ y_) m = do
  l <- m
  putEq l.x x_
  putEq l.y y_
  pure l

degree :: Angle -> SketchM Line -> SketchM Line
degree val m = do
  l <- m
  putExact l.angle val
  pure l

onLine :: Point -> Line -> SketchM ()
onLine p l = tell [OnLine p l]

--- POLYGON
poly :: [Point] -> SketchM Polygon
poly = pure . Polygon

--- INTERSECTION

intersect :: Line -> Line -> SketchM Point
intersect l1 l2 = do
  p <- point
  onLine p l1
  onLine p l2
  pure p
