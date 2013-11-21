
module BioInf.GrammarProducts.Helper where

import Data.Function
import Control.Lens
import qualified Data.Set as S
import Data.List (groupBy)

import BioInf.GrammarProducts.Grammar


-- | Group the RHS of a rule by terminals / non-terminals

groupRHS :: [NtT] -> [[NtT]]
groupRHS = groupBy ((==) `on` isT)

-- | Determine the grammar type

chomskyHierarchy :: Grammar -> Int
chomskyHierarchy g = undefined where
  rs = map (groupRHS . (^.rhs)) . S.toList $ g^.ps
