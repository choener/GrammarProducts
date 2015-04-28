
module FormalLanguage.GrammarProduct.Op.Common where

import qualified Data.Set as S
import Control.Lens hiding (outside)
import Data.Set (Set)

import FormalLanguage.CFG.Grammar



-- | Collect all terminal symbols from a set of rules.
--
-- TODO move to FormalGrammars library
--
-- TODO i guess, this collects multidim stuff for now!!!

collectTerminals :: S.Set Rule -> S.Set Symbol
collectTerminals = error "collectTerminals" -- S.fromList . filter isSymbT . concatMap _rhs . S.toList

-- | Collect all non-terminal symbols from a set of rules.
--
-- TODO move to FormalGrammars library

collectNonTerminals :: Set Rule -> Set Symbol -- S.Set Rule -> S.Set Symb
collectNonTerminals = error "collectNonTerminals" -- S.fromList . _ . view (folded.rhs) -- S.fromList . filter isSymbN . concatMap (\r -> r^.lhs : r^.rhs) . S.toList

-- |
--
-- TODO not needed anymore ?!

collectEpsilons :: S.Set Rule -> S.Set SynTermEps -- TN
collectEpsilons = error "collectEpsilons"
{-
collectEpsilons = S.fromList
                . filter (\case E -> True ; z -> False)
                . concatMap (view symb)
                . concatMap _rhs
                . S.toList
-}

-- | Generate a multidim epsilon symbol of the same length as the given
-- symbol.

genEps :: Symbol -> Symbol -- Symb -> [TN]
genEps s = Symbol $ replicate (length $ s^.getSymbolList) Epsilon -- replicate (length $ s^.symb) E

-- | Generate a multidim @Deletion@ symbol of the same length as the given
-- symbol.

genDel :: Symbol -> Symbol -- Symb -> [TN]
genDel s = Symbol $ replicate (length $ s^.getSymbolList) Deletion -- replicate (length $ s^.symb) E

-- | Checks if two grammars are compatible.
--
-- TODO different inside/outside status might not be a big problem!

opCompatible :: Grammar -> Grammar -> Either String ()
opCompatible l r
  | dim l /= dim r            = Left "Grammars have different dimensions"
  | l^.outside /= r^.outside  = Left "Grammars have incompatible inside/outside status"
  | otherwise                 = Right ()

