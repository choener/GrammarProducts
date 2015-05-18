
-- | Direct product of two grammars.
--
-- Currently implemented for linear grammars. Once we move to context-free
-- grammars with more than one non-terminal on the RHS, things become
-- interesting.

module FormalLanguage.GrammarProduct.Op.Linear where

import Control.Arrow ((&&&))
import Data.Semigroup
import Control.Lens hiding (outside,indices)
import Control.Applicative
import qualified Data.Set as S
import Data.List (groupBy,nub)
import Data.Function (on)
import Data.Default
import qualified Data.Map as M

import FormalLanguage.CFG.Grammar

import FormalLanguage.GrammarProduct.Op.Common



directProduct l r = runLinear $ Linear l <> Linear r

newtype Linear a = Linear {runLinear :: a}



instance Semigroup (Linear Grammar) where
  (Linear g) <> (Linear h) = Linear $ Grammar sv st tv io rs s p ixs (g^.grammarName ++ h^.grammarName) False where -- ts ns es rs s (g^.name <> h^.name) where
    sv  = M.fromList . nub . map (_name &&& id) . concatMap _getSymbolList . uniqueSyntacticSymbols $ set rules rs def -- build a temporary @def@ grammar, extract symbols from that one
    st  = g^.synterms <> h^.synterms
    tv  = g^.termvars <> h^.termvars
    io  = g^.outside
    rs  = S.fromList [ direct l r | l <- g^..rules.folded, r <- h^..rules.folded ]
    s   = (g^.start) <> (h^.start)
    p   = (g^.params) <> (h^.params)
    ixs = (g^.indices) <> (h^.indices)
    direct l r = Rule (l^.lhs <> r^.lhs) (l^.attr <> r^.attr) (mergeRHS (l^.rhs) (r^.rhs))
    {-
    ts = g^.tsyms <> h^.tsyms
    ns = collectNonTerminals rs
    rs = S.fromList [ direct l r | l <- g^..rules.folded, r <- h^..rules.folded ]
    s  = liftA2 (<>) (g^.start) (h^.start)
    direct (Rule l f rs) (Rule a g bs) = Rule (l <> a) (f++g) (mergeRHS rs bs)
    -}

instance Monoid (Linear Grammar) where
  mempty = Linear $ set rules (S.singleton $ Rule (Symbol []) [] []) def
  mappend = (<>)

-- | Merges right-hand sides in a linear direct product. For full-fledged CFGs
-- in different normal forms, see the GNF and CNF implementations.

mergeRHS :: [Symbol] -> [Symbol] -> [Symbol]
mergeRHS [] rs = rs -- neutral element
mergeRHS ls [] = ls -- neutral element
mergeRHS ls' rs' = concat $ go (groupRHS ls') (groupRHS rs') where
  dl = head ls'
  dr = head rs'
  go [] [] = []
  go [] (r:rs)
    | all isTerminal  r = map (genDel dl <>) r : go [] rs
    | all isSyntactic r = let [z] = r
                          in  [genDel dl <> z] : go [] rs
  go (l:ls) []
    | all isTerminal  l = map (<> genDel dr) l : go ls []
    | all isSyntactic l = let [z] = l
                          in  [z <> genDel dr] : go ls []
  go (l:ls) (r:rs)
    | all isTerminal  l && all isTerminal  r = goT l r : go ls rs
    | all isSyntactic l && all isSyntactic r = let [Symbol y] = l
                                                   [Symbol z] = r
                                               in  [Symbol $ y++z] : go ls rs
    | all isSyntactic l = go [l] []  ++ go ls     (r:rs)
    | all isSyntactic r = go []  [r] ++ go (l:ls) rs
    | otherwise     = go [l] []  ++ go [] [r] ++ go ls rs
  go ls rs          = error $ "unhandled (Lib-FormalLanguage, FormalLanguage.GrammarProduct.Op.Linear): " ++ show (ls,rs)
  goT []       []       = []
  goT []       (t : rs) = (genDel dl <> t) : goT [] rs
  goT (t : ls) []       = (t <> genDel dr) : goT ls []
  goT (u : ls) (v : rs) = (u<>v)           : goT ls rs

groupRHS = groupBy ((==) `on` isTerminal)

