{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SketchSolver (runSolver) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State
import Control.Monad.Freer.State (State, get, modify, put, runState)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import OpenSCAD (Model2d)
import SketchTypes
import UnionFind (UnionFind, emptyUF, find, union)
import Prelude hiding (id)

type SolverM = Eff '[Reader (OnLines, Exacts), State UnionFind, Error SketchError]

type OnLines = [(Point, Line)]

type Exacts = [(Id, Double)]

runSolver :: (Sketch, [Constraint]) -> Either SketchError Model2d
runSolver (sk, cs) =
  let onLines = mapMaybe (\case OnLine p l -> Just (p, l); _ -> Nothing) cs
      exacts = mapMaybe (\case Exact id v -> Just (id, v); _ -> Nothing) cs
   in runReader (onLines, exacts) (solveConstraints sk >>= validateAllJust)
        & (runState emptyUF)
        & fmap fst
        & runError
        & run
        & generateModel

generateModel :: Either SketchError Sketch -> Either SketchError Model2d
generateModel = undefined

solveConstraints :: Sketch -> SolverM Sketch
solveConstraints _sketch = undefined

applyConstraint :: Constraint -> SolverM ()
applyConstraint (Exact id _) = do
  uf <- get
  let root = find id uf
  modify (id `union` root)
applyConstraint (Eq id1 id2) = modify (id1 `union` id2)
applyConstraint (OnLine (Point x y) (Line lx ly angle)) = do
  undefined

validateAllJust :: Sketch -> SolverM Sketch
validateAllJust sk@(P (Point x y)) = do
  exx <- isSolved x
  exy <- isSolved y
  if exx && exy
    then pure $ sk
    else throwError (Unresolved $ show sk)

isSolved :: Id -> SolverM Bool
isSolved id = do
  (uf, _, _) <- readStat
  pure $ find id uf == id

readStat :: SolverM (UnionFind, OnLines, Exacts)
readStat = do
  uf <- get
  (onLines, exacts) <- ask
  pure (uf, onLines, exacts)

todoName :: Id -> Id -> SolverM ()
todoName l r = do
  liftA2 (,) (parentIsExact l) (parentIsExact r) >>= \case
    (True, False) -> updateUf l r
    (False, True) -> updateUf r l
    (True, True) -> throwError (Contradiction $ show l ++ " " ++ show r)
    _ -> error "should not happen"

-- helpers

parentIsExact :: Id -> SolverM Bool
parentIsExact id = do
  (uf, _, exacts) <- readStat
  pure $ isExact id uf exacts

isExact :: Id -> UnionFind -> Exacts -> Bool
isExact id uf exacts =
  case find id uf of
    parent -> case lookup parent exacts of
      Just _ -> True
      _ -> False

updateUf :: Id -> Id -> SolverM ()
updateUf l r = do
  (uf, _, _) <- readStat
  put $ union l r uf
