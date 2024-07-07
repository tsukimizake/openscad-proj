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
import qualified Data.List as List
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

runSolver :: (Sketch, [Constraint]) -> Either SketchError Model2d
runSolver (sk, cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
      eqs = mapMaybe (\case Eq l r -> Just (l, r); _ -> Nothing) cs
   in (solveConstraints >>= validateAllJust >>= generateModel)
        & runState emptyUF
        & runReader (onLines, exacts, eqs, sk)
        & fmap fst
        & runError
        & run

generateModel :: Sketch -> SolverM Model2d
generateModel (Poly (Polygon ps)) =
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
generateModel _ = undefined

solveConstraints :: SolverM Sketch
solveConstraints = do
  (uf, onLines, exacts, eqs, sk) <- readStat
  traceShowM eqs
  mapM_ (uncurry unifyIds) eqs
  pure sk

validateAllJust :: Sketch -> SolverM Sketch
validateAllJust sk@(P (Point x y)) = do
  liftA2 (,) (isSolved x) (isSolved y) >>= \case
    (True, True) -> pure sk
    _ -> throwError (Unresolved $ show sk)
validateAllJust sk@(LineFunc (Line lx ly angle)) = do
  liftA3 (,,) (isSolved lx) (isSolved ly) (isSolved angle) >>= \case
    (True, True, True) -> pure sk
    _ -> throwError (Unresolved $ show sk)
validateAllJust sk@(Poly (Polygon ps)) = do
  _ <- ps & mapM (validateAllJust . P)
  pure sk

isSolved :: Id -> SolverM Bool
isSolved id = do
  getValue id & fmap Maybe.isJust

readStat :: SolverM (UnionFind, OnLines, Exacts, Eqs, Sketch)
readStat = do
  uf <- get
  (onLines, exacts, eqs, sk) <- ask
  pure (uf, onLines, exacts, eqs, sk)

unifyIds :: Id -> Id -> SolverM ()
unifyIds l r = do
  liftA2 (,) (parentIsExact l) (parentIsExact r) >>= \case
    (True, False) -> do
      traceShowM ("l is exact", l)
      updateUf l r
    (False, True) -> do
      traceShowM ("r is exact", r)
      updateUf r l
    (True, True) -> do
      lv <- getValue l
      rv <- getValue r
      if lv == rv
        then pure ()
        else throwError (Contradiction $ "Exact values are not equal: " ++ show lv ++ " != " ++ show rv)
    _ -> error "should not happen"

-- helpers
getValue :: Id -> SolverM (Maybe Double)
getValue id = do
  (uf, _, exacts, _, _) <- readStat
  case find id uf of
    parent -> pure $ lookup parent exacts

assertJust :: Maybe a -> SolverM a
assertJust = \case
  Just a -> pure a
  _ -> error "value is not resolved"

parentIsExact :: Id -> SolverM Bool
parentIsExact id = do
  (uf, _, exacts, _, _) <- readStat
  pure $ isExact id uf exacts

isExact :: Id -> UnionFind -> Exacts -> Bool
isExact id uf exacts =
  case find id uf of
    parent -> case lookup parent exacts of
      Just _ -> True
      _ -> False

updateUf :: Id -> Id -> SolverM ()
updateUf l r = do
  (uf, _, _, _, _) <- readStat
  put $ union l r uf
