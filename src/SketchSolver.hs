{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognisepointd-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SketchSolver (runSolver) where

import Control.Applicative (liftA3)
import Control.Monad (forM)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe
import Debug.Trace (traceShowId, traceShowM)
import OpenSCAD (Model2d, polygon)
import SketchTypes
import UnionFind (UnionFind, emptyUF, find, union)
import Prelude hiding (id)

type SolverM = Eff '[State UnionFind, Reader (OnLines, Exacts, Eqs, Sketch), Error SketchError]

type OnLines = [(Point, Line)]

type Exacts = [(Id, Double)]

type Eqs = [(Id, Id)]

data SolverState = SolverState
  { uf :: UnionFind,
    onLines :: OnLines,
    exacts :: Exacts,
    eqs :: Eqs,
    sketch :: Sketch
  }

runSolver :: (Sketch, [Constraint]) -> Either SketchError Model2d
runSolver (sk, cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
      eqs = mapMaybe (\case Eq l r -> Just (l, r); _ -> Nothing) cs
   in (repeatUntilFixpoint (solveOnLines >> solveUf >> solveOnLines >> validateAllJust >> generateModel))
        & runState emptyUF
        & runReader (onLines, exacts, eqs, sk)
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

solveOnLines :: SolverM ()
solveOnLines = do
  SolverState {onLines, sketch} <- readStat
  mapM_ (solveOnLine sketch) onLines

solveOnLine :: Sketch -> (Point, Line) -> SolverM ()
solveOnLine sk (p, l) = do
  undefined

generateModel :: SolverM Model2d
generateModel = do
  SolverState {sketch} <- readStat
  generateModelImpl sketch
  where
    generateModelImpl :: Sketch -> SolverM Model2d
    generateModelImpl (Poly (Polygon ps)) =
      do
        rs <-
          forM
            ps
            ( \(Point x y) -> do
                x1' <- getValue x >>= assertJust
                y1' <- getValue y >>= assertJust
                pure (x1', y1')
            )
        pure $ polygon 3 [rs]
    generateModelImpl _ = undefined

solveUf :: SolverM ()
solveUf = do
  SolverState {eqs} <- readStat
  mapM_ (uncurry unifyIds) eqs

validateAllJust :: SolverM ()
validateAllJust = do
  SolverState {sketch} <- readStat
  validateAllJustImpl sketch
  pure ()
  where
    validateAllJustImpl :: Sketch -> SolverM ()
    validateAllJustImpl sk@(P (Point x y)) = do
      liftA2 (,) (isSolved x) (isSolved y) >>= \case
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

readStat :: SolverM SolverState
readStat = do
  uf <- get
  (onLines, exacts, eqs, sk) <- ask
  pure $ SolverState uf onLines exacts eqs sk

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

getValue :: Id -> SolverM (Maybe Double)
getValue id = do
  SolverState {uf, exacts} <- readStat
  case find id uf of
    parent -> pure $ lookup parent exacts

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
  SolverState {uf} <- readStat
  put $ union l r uf

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
