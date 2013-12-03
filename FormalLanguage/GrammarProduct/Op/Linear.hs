{-# LANGUAGE FlexibleInstances #-}

-- | Direct product of two grammars.
--
-- Currently implemented for linear grammars. Once we move to context-free
-- grammars with more than one non-terminal on the RHS, things become
-- interesting.

module FormalLanguage.GrammarProduct.Op.Linear where

import Data.Semigroup
import Control.Lens
import Control.Applicative
import qualified Data.Set as S
import Data.List (groupBy)
import Data.Function (on)

import FormalLanguage.Grammar

import FormalLanguage.GrammarProduct.Op.Common



newtype Linear a = Linear {runLinear :: a}



instance Semigroup (Linear Grammar) where
  (Linear g) <> (Linear h) = Linear $ Grammar ts ns es rs s (g^.name <> h^.name) where
    ts = g^.tsyms <> h^.tsyms
    ns = collectNonTerminals rs
    es = g^.epsis <> h^.epsis
    rs = S.fromList [ direct l r | l <- g^..rules.folded, r <- h^..rules.folded ]
    s  = liftA2 (\l r -> Symb $ l^.symb ++ r^.symb) (g^.start) (h^.start)
    direct (Rule l f rs) (Rule a g bs) = Rule (Symb $ l^.symb ++ a^.symb) (f++g) (mergeRHS rs bs)

instance Monoid (Linear Grammar) where
  mempty = Linear $ Grammar S.empty S.empty S.empty (S.singleton $ Rule (Symb []) [] []) Nothing ""
  mappend = (<>)

-- | Merges right-hand sides in a linear direct product. For full-fledged CFGs
-- in different normal forms, see the GNF and CNF implementations.

mergeRHS :: [Symb] -> [Symb] -> [Symb]
mergeRHS [] rs = rs -- neutral element
mergeRHS ls [] = ls -- neutral element
mergeRHS ls' rs' = concat $ go (groupRHS ls') (groupRHS rs') where
  dl = head ls'
  dr = head rs'
  go [] [] = []
  go [] (r:rs)
    | all tSymb r = map (\(Symb z) -> Symb $ genEps dl ++ z) r : go [] rs
    | all nSymb r = let [Symb z] = r
                    in  [Symb $ genEps dl ++ z] : go [] rs
  go (l:ls) []
    | all tSymb l = map (\(Symb z) -> Symb $ z ++ genEps dr) l : go ls []
    | all nSymb l = let [Symb z] = l
                    in  [Symb $ z ++ genEps dr] : go ls []
  go (l:ls) (r:rs)
    | all tSymb l && all tSymb r = goT l r : go ls rs
    | all nSymb l && all nSymb r = let [Symb y] = l
                                       [Symb z] = r
                                   in  [Symb $ y++z] : go ls rs
    | all tSymb l = go [l] []  ++ go ls     (r:rs)
    | all tSymb r = go []  [r] ++ go (l:ls) rs
    | otherwise   = go [l] []  ++ go [] [r] ++ go ls rs
  goT []            []            = []
  goT []            (Symb t : rs) = Symb (genEps dl ++ t) : goT [] rs
  goT (Symb t : ls) []            = Symb (t ++ genEps dr) : goT ls []
  goT (Symb u : ls) (Symb v : rs) = Symb (u++v)           : goT ls rs

groupRHS = groupBy ((==) `on` tSymb)

