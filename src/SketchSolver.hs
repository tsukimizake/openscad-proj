{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognisepointd-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SketchSolver (runSolver, cos, sin) where

import Control.Applicative (liftA3)
import Control.Monad (forM)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State
import Data.Function ((&))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe
import Debug.Trace
import OpenSCAD (Model2d, polygon)
import SketchTypes
import UnionFind (UnionFind, emptyUF, find, union)
import Prelude hiding (cos, id, sin, tan)
import qualified Prelude

type SolverM = Eff '[State (UnionFind, Eqs, Exacts), Reader (OnLines, Sketch, Intersections), Error SketchError]

type Intersections = [(Line, Line, Point)]

type OnLines = [(Point, Line)]

type Exacts = [(Id, Double)]

type Eqs = [(Id, Id)]

data SolverState = SolverState
  { uf :: UnionFind,
    onLines :: OnLines,
    exacts :: Exacts,
    eqs :: Eqs,
    sketch :: Sketch,
    intersections :: Intersections
  }

runSolver :: (Sketch, [Constraint]) -> Either SketchError Model2d
runSolver (sk, cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
      eqs = mapMaybe (\case Eq l r -> Just (l, r); _ -> Nothing) cs
      intersections = mapMaybe (\case Intersection l r p -> Just (l, r, p); _ -> Nothing) cs
   in (repeatUntilFixpoint (solveIntersections >> solveOnLines >> solveUf) >> validateAllJust >> generateModel)
        & runState (emptyUF, eqs, exacts)
        & runReader (onLines, sk, intersections)
        & fmap fst
        & runError
        & run

repeatUntilFixpoint :: SolverM a -> SolverM a
repeatUntilFixpoint m = do
  -- repeat until state is not changed
  beforeStat <- readStat
  res <- m
  afterStat <- readStat
  if beforeStat.uf == afterStat.uf
    then pure res
    else repeatUntilFixpoint m

--------------
-- Finalize
--------------

generateModel :: SolverM Model2d
generateModel = do
  SolverState {sketch} <- readStat
  generateModelImpl sketch
  where
    generateModelImpl :: Sketch -> SolverM Model2d
    generateModelImpl (Poly (Polygon ps)) = do
      rs <- forM ps $ \(Point x y chamfer) -> do
        -- TODO chamfer
        x1' <- getValue x >>= assertJust
        y1' <- getValue y >>= assertJust
        pure (x1', y1')
      pure $ polygon 3 [rs]
    generateModelImpl _ = undefined

validateAllJust :: SolverM ()
validateAllJust = do
  SolverState {sketch} <- readStat
  validateAllJustImpl sketch
  pure ()
  where
    validateAllJustImpl :: Sketch -> SolverM ()
    validateAllJustImpl sk@(P p) = do
      liftA2 (,) (isSolved p.x) (isSolved p.y) >>= \case
        (True, True) -> pure ()
        _ -> throwError (Unresolved $ show sk)
    validateAllJustImpl sk@(LineFunc (Line lx ly angle)) = do
      liftA3 (,,) (isSolved lx) (isSolved ly) (isSolved angle) >>= \case
        (True, True, True) -> pure ()
        _ -> throwError (Unresolved $ show sk)
    validateAllJustImpl (Poly (Polygon ps)) = do
      _ <- ps & mapM (validateAllJustImpl . P)
      pure ()

isSolved :: Id -> SolverM Bool
isSolved id = do
  getValue id & fmap Maybe.isJust

--------------
-- Intersections
--------------
solveIntersections :: SolverM ()
solveIntersections = do
  SolverState {intersections} <- readStat
  mapM_ solveIntersection intersections

solveIntersection :: (Line, Line, Point) -> SolverM ()
solveIntersection (l1, l2, p) = do
  liftA3 (,,) (getValue l1) (getValue l2) (getValue p) >>= \case
    ((Just x1, Just y1, Just angle1), (Just x2, Just y2, Just angle2), (Nothing, Nothing)) -> do
      case (angle1, angle2) of
        (0, _) ->
          putEq p.y l1.y
        (90, _) ->
          putEq p.x l1.x
        (180, _) ->
          putEq p.y l1.y
        (270, _) ->
          putEq p.x l1.x
        (_, 0) ->
          putEq p.y l2.y
        (_, 90) ->
          putEq p.x l2.x
        (_, 180) ->
          putEq p.y l2.y
        (_, 270) ->
          putEq p.x l2.x
        _ -> do
          let (x, y) = calcIntersection (x1, y1, angle1) (x2, y2, angle2)
          putExact p.x x
          putExact p.y y
    _ -> pure ()

calcIntersection :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double)
calcIntersection (x1, y1, angle1) (x2, y2, angle2) =
  let a1 = tan angle1
      a2 = tan angle2
      x = (a1 * x1 - a2 * x2 + y2 - y1) / (a1 - a2)
      y = a1 * (x - x1) + y1
   in (x, y)

--------------
-- OnLines
--------------

solveOnLines :: SolverM ()
solveOnLines = do
  SolverState {onLines} <- readStat
  mapM_ (solveOnLine) onLines

solveOnLine :: (Point, Line) -> SolverM ()
solveOnLine (p, l) = do
  angle_ <- getValue l.angle
  case angle_ of
    Just 0 -> putEq p.y l.y
    Just 90 -> putEq p.x l.x
    Just 180 -> putEq p.y l.y
    Just 270 -> putEq p.x l.x
    _ -> do
      liftA2 (,) (getValue p) (getValue l) >>= \case
        ((Nothing, Just y), (Just lx, Just _ly, Just angle)) -> do
          let x = lx + cos angle * y
          putExact p.x x
        ((Just x, Nothing), (Just _lx, Just ly, Just angle)) -> do
          let y = ly + sin angle * x
          putExact p.y y
        ((Just x, Just y), (Nothing, Just _ly, Just angle)) -> do
          let lx = x - cos angle * y
          putExact l.x lx
        ((Just x, Just y), (Just _lx, Nothing, Just angle)) -> do
          let ly = y - sin angle * x
          putExact l.y ly
        ((Just x, Just y), (Just lx, Just ly, Nothing)) -> do
          let angle = atan2 (y - ly) (x - lx)
          putExact l.angle angle
        _ -> pure ()

--------------
-- UF
--------------

solveUf :: SolverM ()
solveUf = do
  SolverState {eqs} <- readStat
  mapM_ (uncurry unifyIds) eqs

readStat :: SolverM SolverState
readStat = do
  (uf, eqs, exacts) <- get
  (onLines, sk, intersections) <- ask
  pure $ SolverState uf onLines exacts eqs sk intersections

unifyIds :: Id -> Id -> SolverM ()
unifyIds l r = do
  liftA2 (,) (getValue l) (getValue r) >>= \case
    (Just _, Nothing) -> do
      updateUf l r
    (Nothing, Just _) -> do
      updateUf r l
    (Just lv, Just rv) -> do
      if lv == rv
        then pure ()
        else
          throwContradiction (l, lv) (r, rv)
    (Nothing, Nothing) -> do
      pure ()

----------
-- helpers
----------

class HasValue a where
  type Value a :: Type
  getValue :: a -> SolverM (Value a)

instance HasValue Id where
  type Value Id = Maybe Double
  getValue id = do
    SolverState {uf, exacts} <- readStat
    case find id uf of
      parent -> pure $ lookup parent exacts

instance HasValue Point where
  type Value Point = (Maybe Double, Maybe Double)
  getValue p = do
    x <- getValue $ p.x
    y <- getValue $ p.y
    pure (x, y)

instance HasValue Line where
  type Value Line = (Maybe Double, Maybe Double, Maybe Double)
  getValue l = do
    x <- getValue $ l.x
    y <- getValue $ l.y
    angle <- getValue $ l.angle
    pure (x, y, angle)

assertJust :: Maybe a -> SolverM a
assertJust = \case
  Just a -> pure a
  _ -> error "value is not resolved"

parentIsExact :: Id -> SolverM Bool
parentIsExact id = do
  SolverState {uf, exacts} <- readStat
  pure $ isExact id uf exacts
  where
    isExact :: Id -> UnionFind -> Exacts -> Bool
    isExact id_ uf exacts =
      case find id_ uf of
        parent -> case lookup parent exacts of
          Just _ -> True
          _ -> False

updateUf :: Id -> Id -> SolverM ()
updateUf l r = do
  SolverState {uf, eqs, exacts} <- readStat
  put $ (union l r uf, eqs, exacts)

putExact :: Id -> Double -> SolverM ()
putExact id v = do
  stat <- readStat
  let exacts = [(id, v)] ++ stat.exacts
  put (stat.uf, stat.eqs, exacts)

putEq :: Id -> Id -> SolverM ()
putEq id1 id2 = do
  stat <- readStat
  let eqs = [(id1, id2)] ++ stat.eqs
  put (stat.uf, eqs, stat.exacts)

cos :: (Floating b) => b -> b
cos degree = degree * (pi / 180) & Prelude.cos

sin :: (Floating b) => b -> b
sin degree = degree * (pi / 180) & Prelude.sin

tan :: (Floating b) => b -> b
tan degree = degree * (pi / 180) & Prelude.tan

----------
-- error functions
----------

throwContradiction :: (Id, Double) -> (Id, Double) -> SolverM ()
throwContradiction (l, lv) (r, rv) = do
  SolverState {sketch} <- readStat

  throwError
    ( Contradiction $
        "Exact values are not equal: "
          ++ (show l ++ ":" ++ show lv)
          ++ ", "
          ++ (show r ++ ":" ++ show rv)
          ++ ("\nin " ++ show sketch)
    )
