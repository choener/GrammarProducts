
module FormalLanguage.GrammarProduct.Op.Common where

import qualified Data.Set as S
import Control.Lens

import FormalLanguage.Grammar



-- | Collect all terminal symbols from a set of rules.
--
-- TODO move to FormalGrammars library

collectTerminals :: S.Set Rule -> S.Set Symb
collectTerminals = S.fromList . filter tSymb . concatMap _rhs . S.toList

-- | Collect all non-terminal symbols from a set of rules.
--
-- TODO move to FormalGrammars library

collectNonTerminals :: S.Set Rule -> S.Set Symb
collectNonTerminals = S.fromList . filter nSymb . concatMap _rhs . S.toList

genEps :: Symb -> [TN]
genEps s = replicate (length $ s^.symb) eps

